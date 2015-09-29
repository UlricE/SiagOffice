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
 * fileio_txt.c
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "calc.h"
#include <Mowitz/MwFormat.h>
#include "../common/common.h"

#define START 0
#define INSIDE 1
#define OUTSIDE 2

static char ifs[256];

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

static int save_csv(char *fn, buffer * buf)
{
	int i, j, lastcell, lr;
	FILE *fp;
	int s = 0;	/* only use first sheet */

	strcpy(ifs, ",");
	if (!ask_for_str("Field separator(s):", ifs))
		strcpy(ifs, ",");

	if ((fp = fopen(fn, "w")) == (FILE *) 0)
		return 1;

	lr = line_last_used(buf, s);

	for (i = 1; i <= lr; i++) {
		lastcell = col_last_changed(buf, s, i);
		for (j = 1; j <= lastcell; j++) {
			if (j > 1) fprintf(fp, "%c", ifs[0]);

			if (ret_type(buf, s, i, j) != ERROR) {
				fprintf(fp, "%s",
					ret_pvalue(NULL, buf, s, i, j, -1));
			}

		}
		fprintf(fp, "\n");
	}
	fclose(fp);
	return 0;
}				/* save */

static double value;
static char instring[256];
static int pi;
static int sf = 0;

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

static int row, col;

static void emit(buffer *b, int s, int c)
{
	if (c == '\n') {
		if (pi > 0) print_cell(b, s, row, col, instring);
		pi = 0;
		row++;
		col = 1;
	} else if (strchr(ifs, c)) {
		if (pi > 0) print_cell(b, s, row, col, instring);
		pi = 0;
		col++;
	} else {
		instring[pi++] = c;
	}
	instring[pi] = 0;
}

static int csv_loader(FILE *fp, buffer *buf)
{
	int i, j;
	int s = 0;	/* only use first sheet */
	int state = START, nextc;
	cval value;
	value.number = 0;

	for (i = line_last_used(buf, s); i > 0; i--)
		for (j = col_last_changed(buf, s, i); j > 0; j--) {
			ins_data(buf, siod_interpreter, NULL,
				value, EMPTY, s, i, j);
			ins_format(buf, s, i, j, sf);
		}

	pi = 0;
	row = 1;
	col = 1;

	while ((nextc = getc(fp)) != EOF) {
		if (nextc == '\n') {
			emit(buf, s, nextc);
			state = START;
		} else if (isspace(nextc)) {
			emit(buf, s, nextc);
		} else {
			switch (state) {
			case START:
				if (nextc == '\"') state = INSIDE;
				else if (strchr(ifs, nextc))
					emit(buf, s, nextc);
				else {
					emit(buf, s, nextc);
					state = OUTSIDE;
				}
				break;
			case INSIDE:
				/* silently swallow extra IFS */
				if (strchr(ifs, nextc));
				else if (nextc == '\"') state = OUTSIDE;
				else emit(buf, s, nextc);
				break;
			case OUTSIDE:
				if (strchr(ifs, nextc)) state = START;
				emit(buf, s, nextc);
				break;
			}
		}
	}
	return 0;
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int load_csv(char *fn, buffer *buf)
{
	FILE *fp;
	int n;

	strcpy(ifs, ",");
	if (!ask_for_str("Field separator(s):", ifs)) {
		return 1;
	}

	if ((fp = fopen(fn, "r")) == NULL) {
		return 1;
	}

	n = csv_loader(fp, buf);

	fclose(fp);
	return n;
} /* load */


static int save_txt(char *fn, buffer *buf)
{
   	int i, j, lastcell, lr;
	int s = 0;	/* only first sheet */
	FILE *fp;

	if ((fp = fopen(fn, "w")) == (FILE *) 0)
		return 1;

	lr = line_last_used(buf, s);

	for (i = 1; i <= lr; i++) {
		lastcell = col_last_changed(buf, s, i);
		for (j = 1; j <= lastcell; j++) {
			if (j > 1) fprintf(fp, " ");

			if (ret_type(buf, s, i, j) != ERROR) {
				fprintf(fp, "%s",
					ret_pvalue(NULL, buf, s, i, j, -1));
			}
		}
		fprintf(fp, "\n");
	}
	fclose(fp);
	return 0;
}

static int load_txt(char *fn, buffer *buf)
{
	int i, j;
	int s = 0;
	char *texti;
	FILE *fp;
	char b[256], *p;
	cval value;
	value.number = 0;

	if ((fp = fopen(fn, "r")) == NULL)
		return 1;

	for (i = line_last_used(buf, s); i > 0; i--)
		for (j = col_last_changed(buf, s, i); j > 0; j--) {
			ins_data(buf, siod_interpreter, NULL,
				value, EMPTY, s, i, j);
			ins_format(buf, s, i, j, sf);
		}

	pi = 0;
	i = 1;

	while (fgets(b, 250, fp) != NULL) {
		if ((p = strchr(b, '\n'))) *p = '\0';
		texti = b;
		ins_data(buf, siod_interpreter, texti,
			value, LABEL, s, i, 1);
		ins_format(buf, s, i++, 1, sf);
	}

	fclose(fp);
	return 0;
}

/* ---
conservative file format guessing:
   Never match, text files vary too much
*/

static int myformat(char *fn)
{
	return 0;
}

/* ---
*/
void fileio_txt_init(void)
{
	register_format(load_csv, save_csv, myformat,
		"Comma Separated Value (*.csv)");
	register_format(load_txt, save_txt, myformat,
		"Text (*.txt)");
}

