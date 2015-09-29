/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2002  Ulric Eriksson <ulric@siag.nu>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.
 */

/*
 * fileio_html.c
 */

/*
2000-11-14: A bunch of improvements to work better with xlHtml.

 - Sheet names are placed between <H1>...</H1> tags. We can use
   that information to change the name of the sheet when this tag
   pair appears outside of a table.

 - The <HR> tag is used after each sheet. We can use that information
   to add a new sheet when this tag appears outside of a table.
   Even better: start a new sheet for every <TABLE> tag. Then we
   don't need to check <HR> at all. <TABLE> tags that appear inside
   tables (i.e. nested tables) are ignored.

 - Fonts are changed by the tag <FONT FACE="foo"> and taken back
   to default by </FONT>.

 - Boldface and italic are controlled by <B>...</B> and <I>...</I>.

 - Alignment is controlled by <TR ALIGN="foo"> and <TD ALIGN="bar">.

The most important by far of these tidbits is the sheet separator,
in that it keeps the structure of the document the same as the
original Excel file. Sheet names are second most important.
Formatting information will be ignored for now.

The writer has also been updated to produce files in this format,
including saving all the sheets in separate tables.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <Mowitz/MwFormat.h>
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

#include "calc.h"

#define START 0
#define INSIDE 1
#define OUTSIDE 2

static int sht, row, col;

static int col_last_changed(buffer *b, int s, int row)
{
	int i;

	if (row > b->sht[s].alloc_lines) return 0;
	for (i = b->sht[s].alloc_cols[row]; i > 1; i--)
		if (ret_type(b, s, row, i) != EMPTY)
			break;
	return i;
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int save(char *fn, buffer *buf)
{
	int lastcell, lr;
	FILE *fp;

	if ((fp = fopen(fn, "w")) == (FILE *) 0)
		return 1;

	fprintf(fp,	"<HTML>\n"
			"<HEAD>\n"
			"  <TITLE>%s</TITLE>\n"
			"</HEAD>\n\n"
			"<BODY>\n", fn);

	for (sht = 0; sht < buf->nsht; sht++) {
		fprintf(fp, "<H1>%s</H1>\n", buf->sht[sht].name);
		fprintf(fp, "<TABLE>\n");
		lr = line_last_used(buf, sht);

		for (row = 1; row <= lr; row++) {
			fprintf(fp, "\n<TR>\n");
			lastcell = col_last_changed(buf, sht, row);
			for (col = 1; col <= lastcell; col++) {
				fprintf(fp, "  <TD>");

				if (ret_type(buf, sht, row, col) != ERROR) {
					char *p, b[100];
					int n;
					p = ret_pvalue(NULL, buf,
							sht, row, col, -1);
					for (n = 0; p[n]; n++) {
						MwToCchar(b, p[n]);
						fprintf(fp, "%s", b);
					}
				}

				fprintf(fp, "</TD>\n");
			}
			fprintf(fp, "</TR>\n");
		}
		fprintf(fp, "</TABLE>\n");
	}

	fprintf(fp,	"</TABLE>\n"
			"</BODY>\n"
			"</HTML>\n");

	fclose(fp);
	return 0;
}				/* save */

#define STARTSTATE 0
#define BEFORETABLE 0
#define BEFOREROW 1
#define BEFOREDATA 2
#define DATA 3
#define ENDSTATE 4
#define NAMESTATE 5

static int state;

static double value;
static char instring[256];
static int pi;
static int sf = 0;
static int inname = 0;


/* it would be more elegant to do it like edit_unknown et al */
static int guess_type(char *p)
{
	char *endp;

	if (!strcmp(p, "")) return EMPTY;

	value = strtod(p, &endp);
	if (only_space(endp)) return CONSTANT;

	return LABEL;
}

static void print_cell(buffer *b, int s, int r, int c, char *p)
{
	char *texti;
	cval value;
	value.number = 0;

	switch (guess_type(p)) {
	case EMPTY:
		break;
	case CONSTANT:
		texti = p;
		value.number = strtod(texti, NULL);
		ins_data(b, siod_interpreter, texti,
			value, CONSTANT, s, r, c);
		ins_format(b, s, r, c, sf);
		break;
	default:	/* anything else is a label */
		texti = p;
		ins_data(b, siod_interpreter, texti,
			value, LABEL, s, r, c);
		ins_format(b, s, r, c, sf);
	}
}

static void breakline(buffer *b)
{
	if (pi > 0) print_cell(b, sht, row, col, instring);
	pi = 0;
	row++;
	col = 1;
}

static void breakcell(buffer *b)
{
	if (pi > 0) print_cell(b, sht, row, col, instring);
	pi = 0;
	col++;
}

static void allocsheet(buffer *b)
{
	if (sht == b->nsht) buffer_add_sheet(b, sht);
}

static void sheetname(buffer *b, char *n)
{
	while (*n && isspace(*n)) n++;
	allocsheet(b);
	buffer_rename_sheet(b, sht, n);
}

static char *tags[] =
	{"TABLE", "/TABLE", "TR", "/TR", "TH", "/TH", "TD", "/TD",
	 "H1", "/H1", "B", "/B", "I", "/I", NULL};

static void tagout(buffer *b, char *tag)
{
	int i;
	char *p = strtok(tag, "\n\r\t ");

	for (i = 0; tags[i]; i++)
		if (!MwStrcasecmp(tags[i], p)) break;

	switch (state) {
	case STARTSTATE:
		switch (i) {
		case 0: state = BEFOREROW; allocsheet(b);
			row = col = 1; break;
		case 8: state = NAMESTATE; inname = 1; break;
		}
		break;
	case NAMESTATE:
		switch (i) {
		case 9: state = STARTSTATE; inname = 0;
			sheetname(b, instring); break;
		}
		break;
	case BEFOREROW:
		switch (i) {
		case 1: state = STARTSTATE; sht++; break;
		case 2: state = BEFOREDATA; break;
		}
		break;
	case BEFOREDATA:
		switch (i) {
		case 1: state = STARTSTATE; sht++; break;
		case 2: breakline(b); break;
		case 3: breakline(b); state = BEFOREROW; break;
		case 4:
		case 6: state = DATA; instring[pi = 0] = '\0'; break;
		}
		break;
	case DATA:
		switch (i) {
		case 1: state = STARTSTATE; sht++; break;
		case 2: breakline(b); state = BEFOREDATA; break;
		case 3: breakline(b); state = BEFOREROW; break;
		case 4:
		case 6: breakcell(b); break;
		case 5:
		case 7: breakcell(b); state = BEFOREDATA; break;
		}
		break;
	default:
		break;
	}
}

static void emitchar(buffer *b, int c)
{
	if (state == NAMESTATE || state == DATA) {
		if (isspace(c)) instring[pi++] = ' ';
		else instring[pi++] = c;
	}
	instring[pi] = '\0';
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int load(char *fn, buffer * buf)
{
	int i, j;
	FILE *fp;
	int c;
	int inatag = 0;
	int inachar = 0;
	char tag[1000];
	cval value;
	value.number = 0;

	state = STARTSTATE;
	sht = 0;

	if ((fp = fopen(fn, "r")) == NULL)
		return 1;

	for (i = line_last_used(buf, sht); i > 0; i--) {
		for (j = col_last_changed(buf, sht, i); j > 0; j--) {
			ins_data(buf, siod_interpreter, NULL,
				value, EMPTY, sht, i, j);
			ins_format(buf, sht, i, j, sf);
		}
	}

	pi = 0;
	sht = 0;
	row = 1;
	col = 1;

	while ((c = getc(fp)) != EOF) {
		if (inatag) {
			if (c == '>') {
				inatag = 0;
				tag[i] = '\0';
				tagout(buf, tag);
			} else {
				if (i < 999) tag[i++] = c;
			}
		} else if (inachar) {
			if (c == ';') {
				inachar = 0;
				tag[i] = '\0';
				emitchar(buf, MwFromCchar(tag));
			} else {
				if (i < 999) tag[i++] = c;
			}
		} else {
			if (c == '<') {
				inatag = 1;
				i = 0;
			} else if (c == '&') {
				inachar = 1;
				i = 0;
			} else {
				emitchar(buf, c);
			}
		}
	}

	fclose(fp);
	return 0;
} /* load */

/* ---
conservative file format guessing:
   1. extension .html or .htm
   2. contains the string "table"
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp;
	char b[256];

	ext = strrchr(fn, '.');
	if (!ext) return 0;	/* no extension */
	if (MwStrcasecmp(ext, ".html") && MwStrcasecmp(ext, ".htm"))
		return 0;	/* wrong extension */
	if ((fp = fopen(fn, "r")) == NULL) return 0;	/* can't open */
	while (fgets(b, sizeof b, fp)) {
		if (strstr(b, "table") || strstr(b, "TABLE")) {
			fclose(fp);
			return 1;
		}
	}
	fclose(fp);
	return 0;	/* yep, sure is ;-) */
}

/* ---
*/
void fileio_html_init(void)
{
	register_format(load, save, myformat, "Hypertext Markup Language (*.html)");
}

