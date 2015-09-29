/*
   Pathetic Writer
   Copyright (C) 1997-2002  Ulric Eriksson <ulric@siag.nu>

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "../common/common.h"
#include "pw.h"
#include <Mowitz/MwUtils.h>

static void save_line(FILE *fp, MwRichchar *line)
{
	int c;
	char b[100];
	if (!line) return;
	while ((c = line->c)) {
		MwToCchar(b, c);
		fprintf(fp, b);
		line++;
	}
}

static int save(char *fn, buffer *buf)
/* Returns: 0 if successful, otherwise 1 */
{
	FILE *fp;
	int sty;
	int s = 0;	/* first sheet only */
	int i, ll = line_last_used(buf, s), ls = MW_STY_DEFAULT;
	char *closer = NULL;

	if ((fp = fopen(fn, "w")) == NULL) return 1;

	/* do the header */
	fprintf(fp, "<HTML>\n<HEAD>\n<TITLE>\n");
	/* use first non-blank line as title */
	for (i = 1; i <= ll; i++)
		if (buf->sht[s].text[i].p) break;

	if (i > ll) printf("No title");
	else save_line(fp, buf->sht[s].text[i].p);

	fprintf(fp, "\n</TITLE>\n</HEAD>\n\n<BODY>\n");

	for (i = 1; i <= ll; i++) {
		sty = ret_style(buf, s, i);
		if (sty != ls) {
			if (closer) fprintf(fp, "%s\n", closer);
			closer = NULL;
		}
		switch (sty) {
		case MW_STY_HEADER1:
			if (sty != ls) {
				fprintf(fp, "<H1>\n");
				closer = "</H1>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_HEADER2:
			if (sty != ls) {
				fprintf(fp, "<H2>\n");
				closer = "</H2>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_HEADER3:
			if (sty != ls) {
				fprintf(fp, "<H3>\n");
				closer = "</H3>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_HEADER4:
			if (sty != ls) {
				fprintf(fp, "<H4>\n");
				closer = "</H4>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_HEADER5:
			if (sty != ls) {
				fprintf(fp, "<H5>\n");
				closer = "</H5>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_HEADER6:
			if (sty != ls) {
				fprintf(fp, "<H6>\n");
				closer = "</H6>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_ADDRESS:
			if (sty != ls) {
				fprintf(fp, "<ADDRESS>\n");
				closer = "</ADDRESS>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_PREFORMAT:
			if (sty != ls) {
				fprintf(fp, "<PRE>\n");
				closer = "</PRE>";
			}
			save_line(fp, buf->sht[s].text[i].p);
			break;
		case MW_STY_EMBED:
			fprintf(fp, "<BR><IMG SRC=\"");
			save_line(fp, buf->sht[s].text[i].p);
			fprintf(fp, "\" ALT=\"\"><BR>");
			break;
		default:	/* MW_STY_DEFAULT and all MW_STY_USERx styles */
			if (ret_bop(buf, s, i))
				fprintf(fp, "<P>\n");
			save_line(fp, buf->sht[s].text[i].p);
			break;
		}
		fprintf(fp, "\n");
		ls = sty;
	}
	if (closer) fprintf(fp, "%s\n", closer);
	fprintf(fp, "</BODY>\n</HTML>\n");
	fclose(fp);
	return 0;
}


static enum {START, INATAG, INACHAR, END} state;
static int errflag;
static char tagbuf[256];	/* for &chars; and <tags> */
static int tbi, obi, mute, pre, sty;
static int fmt;
static unsigned int row;
static buffer *buf;
static int sht;

/* Store each paragraph on a long line and break it up afterwards. That
   way we *know* that each line will be a bop, and we only need to change
   the style in tag handlers that deal with such things. Preformatted
   text needs special handling, as usual, but even such text get one
   bop per line.
*/

static void emitchar(int c)
{
	if (mute) return;

	if (c == '\n') {	/* break the current line */
		row++;
		set_bop(buf, sht, row, 1);
		set_style(buf, sht, row, sty);
	} else {
		ins_char(buf, sht, row, line_length(buf, sht, row), c, fmt);
	}
}

static void emitstr(char *p)
{
	while (*p) emitchar(*p++);
}

/* &#xxx; => emitchar(atoi(xxx)) */
static void emitcchar(char *p)
{
	int i;
	i = MwFromCchar(p);
	if (i != -1) emitchar(i);
}

static void tag_ignore(void)
{
	;
}

static void tag_newline(void)
{
	emitchar('\n');
}

static void tag_listitem(void)
{
	emitchar('\n');
	emitchar(' ');
	emitchar('-');
	emitchar(' ');
}

static void tag_descdesc(void)
{
	emitchar('\n');
	emitchar('\t');
}

static void tag_mute(void)
{
	mute = 1;
}

static void tag_unmute(void)
{
	mute = 0;
}

static void tag_pre(void)
{
	sty = MW_STY_PREFORMAT;
	fmt = style_table[sty].format;
	emitchar('\n');
	pre = 1;
}

static void tag_pre_(void)
{
	sty = MW_STY_DEFAULT;
	fmt = style_table[sty].format;
	emitchar('\n');
	pre = 0;
}

static void tag_h1(void)
{
	sty = MW_STY_HEADER1;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_h2(void)
{
	sty = MW_STY_HEADER2;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_h3(void)
{
	sty = MW_STY_HEADER3;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_h4(void)
{
	sty = MW_STY_HEADER4;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_h5(void)
{
	sty = MW_STY_HEADER5;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_h6(void)
{
	sty = MW_STY_HEADER6;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_address(void)
{
	sty = MW_STY_ADDRESS;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_plain(void)
{
	sty = MW_STY_DEFAULT;
	fmt = style_table[sty].format;
	emitchar('\n');
}

static void tag_i(void)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	ft.italic = 1;
	fmt = MwEncodeFormat(~0, &ft);
}

static void tag_i_(void)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	ft.italic = 0;
	fmt = MwEncodeFormat(~0, &ft);
}

static void tag_b(void)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	ft.bold = 1;
	fmt = MwEncodeFormat(~0, &ft);
}

static void tag_b_(void)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	ft.bold = 0;
	fmt = MwEncodeFormat(~0, &ft);
}

static struct {
	char *name;
	void (*action)(void);
} tag[] = {
	{"br", tag_newline},
	{"p", tag_newline},
	{"ol", tag_newline},
	{"/ol", tag_newline},
	{"ul", tag_newline},
	{"li", tag_listitem},
	{"dl", tag_newline},
	{"dt", tag_newline},
	{"dd", tag_descdesc},
	{"table", tag_newline},
	{"/table", tag_newline},
	{"tr", tag_newline},
	{"th", tag_newline},
	{"/p", tag_ignore},
	{"h1", tag_h1},
	{"/h1", tag_plain},
	{"h2", tag_h2},
	{"/h2", tag_plain},
	{"h3", tag_h3},
	{"/h3", tag_plain},
	{"h4", tag_h4},
	{"/h4", tag_plain},
	{"h5", tag_h5},
	{"/h5", tag_plain},
	{"h6", tag_h6},
	{"/h6", tag_plain},
	{"hr", tag_newline},
	{"li", tag_newline},
	{"pre", tag_pre},
	{"/pre", tag_pre_},
	{"address", tag_address},
	{"/address", tag_plain},
	{"i", tag_i},
	{"/i", tag_i_},
	{"b", tag_b},
	{"/b", tag_b_},
	{"head", tag_mute},
	{"/head", tag_unmute},
	{"body", tag_unmute},
	{NULL, NULL}
};

/* any unrecognized tag is ignored */
static void emittag(char *p)
{
	int i;
	for (i = 0; p[i]; i++)
		if (isspace(p[i])) break;
	p[i] = '\0';
	for (i = 0; tag[i].name; i++)
		if (!MwStrcasecmp(p, tag[i].name)) break;
	if (tag[i].name) (*tag[i].action)();
}

static void html_char(int c)
{
	switch (state) {
	case START:
		if (isspace(c)) {
			if (pre) emitchar(c);
			else emitchar(' ');
		} else if (c == '<') {
			tbi = 0;
			state = INATAG;
		} else if (c == '&') {
			tbi = 0;
			state = INACHAR;
		} else if (c == EOF) {
			state = END;
		} else {
			emitchar(c);
		}
		break;
	case INATAG:
		if (c == '>') {
			tagbuf[tbi] = '\0';
			emittag(tagbuf);
			state = START;
		} else if (c == EOF) {
			errflag = 1;
			state = END;
		} else {
			if (tbi+10 < sizeof tagbuf)
				tagbuf[tbi++] = c;
		}
		break;
	case INACHAR:
		if (c == ';') {
			tagbuf[tbi] = '\0';
			emitcchar(tagbuf);
			state = START;
		} else if (c == EOF) {
			errflag = 1;
			tagbuf[tbi] = '\0';
			emitstr(tagbuf);
			tbi = 0;
			state = END;
		} else if (isspace(c) || tbi+10 >= sizeof tagbuf) {
			tagbuf[tbi] = '\0';
			emitstr(tagbuf);
			tbi = 0;
			state = START;
		} else {
			tagbuf[tbi++] = c;
		}
		break;
	default:
		fprintf(stderr, "In html_char(): shouldn't be here!\n");
		errflag = 1;
		state = END;
		break;
	}
}

static int load(char *fn, buffer *b)
{
	FILE *fp;

	fp = fopen(fn, "r");
	if (!fp) return 1;

	state = START;
	errflag = 0;
	mute = 0;
	obi = 0;
	tbi = 0;
	pre = 0;
	row = 1;
	buf = b;
	sht = 0;
	sty = MW_STY_DEFAULT;
	fmt = style_table[sty].format;
	buf->height_interest = 0;
	while (state != END)
		html_char(getc(fp));
	buf->height_interest = 1;
	for (row = 1; row <= line_last_used(buf, sht); row++) {
		rebreak_line(buf, sht, row);
	}
	buf->change = 0;
	fclose(fp);
	return 0;
}


/* ---
conservative file format guessing:
   1. extension .html or .htm
   2. contains the string "<HTML>"
*/

static int myformat(char *fn)
{
        char *ext;
        FILE *fp;
        char b[256];

        ext = strrchr(fn, '.');
        if (!ext) return 0;     /* no extension */
        if (MwStrcasecmp(ext, ".html") && MwStrcasecmp(ext, ".htm"))
                return 0;       /* wrong extension */
        if ((fp = fopen(fn, "r")) == NULL) return 0;    /* can't open */
        while (fgets(b, sizeof b, fp)) {
                if (strstr(b, "<html>") || strstr(b, "<HTML>")) {
                        fclose(fp);
                        return 1;
                }
        }
        fclose(fp);
        return 0;
}

/* ---
*/
void fileio_html_init(void)
{
	register_format(load, save, myformat,
			"Hypertext Markup Language (*.html)");
}

