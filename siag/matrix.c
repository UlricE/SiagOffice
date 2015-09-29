/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2003  Ulric Eriksson <ulric@siag.nu>

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

/* ---
 matrix.c

 This module hides the details of stringpool and matrix handling.

 971225 Use strftime for date and time (Dag Nygren, dag@newtech.fi)
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include "../common/common.h"

#include "calc.h"
#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>


/* ---
Here is a bit of code to make styles more general.

   No longer is the number of different styles limited to 16. Styles can be
   added, copied and redefined at will. The old 16 styles are defined by
   default when the program starts.

Styles must be treated differently depending on the type of data in the
   cell. In the future it will be possible to add other types, such as the
   picket fence patterns used by other spreadsheets.
--- */

#define DEFAULT 0	/* try to be clever; don't use the format */
#define INTEGER 1
#define FLOAT 2
#define TIME 3		/* use strftime to print */
#define INVISIBLE 4	/* don't print at all; format ignored */
#define PERCENT 5	/* multiply by 100, then use float */

int nstyle = 0;
style *style_table = NULL;

/* ---
convert index to name
*/

char *style2name(int i)
{
	if (i < 0 || i >= nstyle) return NULL;
	return style_table[i].name;
}

/* ---
convert name to index
*/

int name2style(char *name)
{
	int i;
	if (!name) return -1;
	for (i = 0; i < nstyle; i++) {
		if (!MwStrcasecmp(name, style_table[i].name)) return i;
	}
	return -1;
}

/* ---
This function defines a new style/format or changes an existing one.
	name	the name of the style
	fmt	the new format string
	type	the type of style (integer, float or time)
   Returns the index of the defined style.
*/

int lookup_style(char *name, char *fmt, int type)
{
	int i = name2style(name);
	char *newname = MwStrdup(name);
	char *newfmt = MwStrdup(fmt);

	if (i == -1) {	/* new style */
		i = nstyle++;
		style_table = MwRealloc(style_table, nstyle*sizeof *style_table);
	} else {	/* redefine existing */
		MwFree(style_table[i].name);
		MwFree(style_table[i].fmt);
	}
	style_table[i].name = newname;
	style_table[i].fmt = newfmt;
	style_table[i].type = type;
	return i;
}

/* ---
Returns a null-terminated, sorted list of all known styles.
   Caller must free.
*/

static int compar(const void *p, const void *q)
{
	return strcmp(*(const char **)p, *(const char **)q);
}

/* ---
*/
char **style_list(int *n)
{
	int i;
	char **list = (char **)MwMalloc((nstyle+1)*sizeof(char *));
	for (i = 0; i < nstyle; i++)
		list[i] = MwStrdup(style_table[i].name);
	list[i] = NULL;
	qsort(list, i, sizeof(char *), compar);
	*n = i;
	return list;
}

/* ---
the styles are saved in a single level, just like the formats.
   Attributes: name, height, next, family, size, bold, italic, uline, fg
*/

void save_styles(FILE *fp, int i)
{
	style st;

	st = style_table[i];
	fprintf(fp, ".style %d\n", i);
	fprintf(fp, "name %s\n", st.name);
	fprintf(fp, "fmt %s\n", st.fmt);
	fprintf(fp, "type %d\n", st.type);
	fprintf(fp, "end\n");
}

/* ---
*/
int load_styles(FILE *fp)
{
	style st = style_table[0];
	char b[1000], name[1000], fmt[1000];
	strcpy(name, st.name);
	st.name = name;
	strcpy(fmt, st.fmt);
	st.fmt = fmt;
	while (fgets(b, sizeof b, fp) && strncmp(b, "end", 3)) {
		MwChomp(b);
		if (!strncmp(b, "name ", 5))
			strcpy(st.name, b+5);
		else if (!strncmp(b, "fmt ", 4))
			strcpy(st.fmt, b+4);
		else if (!strncmp(b, "type ", 5))
			st.type = strtol(b+5, NULL, 10);
		/* ignore anything we don't grok */
	}
	return lookup_style(st.name, st.fmt, st.type);
}

/* end of style code */

/* ---
check if a cell has been allocated
*/

static int is_alloc(buffer *b, int s, int row, int col)
{
	if (row < P_MIN.row || row > BUFFER_ROWS ||
		col < P_MIN.col || col > BUFFER_COLS) return 0;
	if (b->sht[s].matrix == NULL) return 0;
	if (row > b->sht[s].alloc_lines) return 0;
	if (b->sht[s].matrix[row] == NULL) return 0;
	if (col > b->sht[s].alloc_cols[row]) return 0;
	return 1;
}

/* Get and set default formats */
void std_fmt_set(buffer *b, int newfmt)
{
	int s, r, c;
	MwFmt osfmt, nsfmt;
	int oldfmt = b->sf;
	int lastr, lastc;
	MwDecodeFormat(newfmt, ~MW_FMT_BORDERS, &nsfmt);
	newfmt = MwEncodeFormat(~MW_FMT_BORDERS, &nsfmt);
	MwDecodeFormat(oldfmt, ~MW_FMT_BORDERS, &osfmt);
	oldfmt = MwEncodeFormat(~MW_FMT_BORDERS, &osfmt);
	b->sf = newfmt;
	for (s = 0; s < b->nsht; s++) {
		lastr = line_last_used(b, s);
		for (r = 1; r <= lastr; r++) {
			lastc = col_last_used(b, s, r);
			for (c = 1; c <= lastc; c++) {
				if (is_alloc(b, s, r, c)
				    && ret_type(b, s, r, c) == EMPTY
				    && ret_format(b, s, r, c) == oldfmt) {
					ins_format(b, s, r, c, newfmt);
				}
			}
		}
	}
}

int std_fmt_get(buffer *b)
{
	return b->sf;
}

/* ---
   void free_matrix(spread **matrix)
   Frees the memory used by the matrix.
*/

void free_matrix(spread ** matrix)
{
	MwFree((char *) matrix);
}


static spread empty_cell = {
	NULL, 0, {0}, EMPTY, 0
};

/* ---
Free the end of a sheet, starting at row r.

All cells are assumed to be EMPTY, so there is no need to free each cell
individually to avoid memory leaks. Also, we don't need to free the
row table. It will be reallocated when needed and malloc knows the size.
*/

void free_rows(buffer *b, int s, int r)
{
	int lr = b->sht[s].alloc_lines;

	b->sht[s].alloc_lines = r-1;
	while (r <= lr) {
		MwFree(b->sht[s].matrix[r]);
		++r;
	}
}

/* ---
make sure the cell is allocated
*/

static void alloc_cell(buffer *b, int sht, int row, int col)
{
	int i;

	empty_cell.format = std_fmt_get(b);
	if (b->sht[sht].matrix == NULL) {
		b->sht[sht].matrix = (spread **)MwMalloc((row+1)*sizeof(spread *));
		b->sht[sht].alloc_cols = (int *)MwMalloc((row+1)*sizeof(int));
		for (i = 0; i <= row; i++) {
			b->sht[sht].matrix[i] = NULL;
			b->sht[sht].alloc_cols[i] = 0;
		}
		b->sht[sht].alloc_lines = row;
	} else if (row > b->sht[sht].alloc_lines) {
		b->sht[sht].matrix = (spread **)MwRealloc(b->sht[sht].matrix,
							(row+1)*sizeof(spread *));
		b->sht[sht].alloc_cols = (int *)MwRealloc(b->sht[sht].alloc_cols,
							(row+1)*sizeof(int));
		for (i = b->sht[sht].alloc_lines+1; i <= row; i++) {
			b->sht[sht].matrix[i] = NULL;
			b->sht[sht].alloc_cols[i] = 0;
		}
		b->sht[sht].alloc_lines = row;
	}
	if (b->sht[sht].matrix[row] == NULL) {
		b->sht[sht].matrix[row] = (spread *)MwMalloc((col+1)*sizeof(spread));
		for (i = 0; i <= col; i++)
			b->sht[sht].matrix[row][i] = empty_cell;
		b->sht[sht].alloc_cols[row] = col;
	} else if (col > b->sht[sht].alloc_cols[row]) {
		b->sht[sht].matrix[row] = (spread *)MwRealloc(b->sht[sht].matrix[row],
						(col+1)*sizeof(spread));
		for (i = b->sht[sht].alloc_cols[row]+1; i <= col; i++)
			b->sht[sht].matrix[row][i] = empty_cell;
		b->sht[sht].alloc_cols[row] = col;
	}
	if (col > b->sht[sht].longest_line) b->sht[sht].longest_line = col;
}

/* ---
*/

void swap_cells(buffer *buf, int s, int r1, int c1, int r2, int c2)
{
	spread sp;

	alloc_cell(buf, s, r1, c1);
	alloc_cell(buf, s, r2, c2);
	sp = buf->sht[s].matrix[r1][c1];
	buf->sht[s].matrix[r1][c1] = buf->sht[s].matrix[r2][c2];
	buf->sht[s].matrix[r2][c2] = sp;
}

/* ---
*/
int ins_data(buffer *b, int interpreter, char *texti, cval v, short t,
	     int s, int row, int col)
{
	int change = FALSE;
	char *p = NULL;
	register spread *mrowcol;	/* faster than matrix[row][col] */

	if (texti == NULL) texti = "";
	if (strlen(texti) == 0) t = EMPTY;

	/* these limits are now very large */
	if (row > BUFFER_ROWS || col > BUFFER_COLS) return FALSE;

	alloc_cell(b, s, row, col);
	mrowcol = b->sht[s].matrix[row] + col;
	if (mrowcol->text != texti || mrowcol->type != t)
		change = TRUE;
	mrowcol->interpreter = interpreter;
	if (mrowcol->text != texti) {	/* we could be restoring the same string */
		if (mrowcol->text) MwFree(mrowcol->text);
		mrowcol->text = MwStrdup(texti);
	}


	if (t == STRING) {
		if (v.text) p = MwStrdup(v.text);
		else p = MwStrdup("");
	}

	if (mrowcol->type == STRING)	/* free the old value */
		MwFree(mrowcol->value.text);
	
	if (t == STRING)
		mrowcol->value.text = p;
	else
		mrowcol->value.number = v.number;
	mrowcol->type = t;
	return change;
}				/* ins_data */

/* ---
*/
int ins_format(buffer *b, int s, int row, int col, int format)
{
	MwFmt fmt;
	int nsfmt;

	if (row > BUFFER_ROWS || col > BUFFER_COLS) {
		std_fmt_set(b, format);
		return FALSE;
	}
	MwDecodeFormat(format, ~0, &fmt);
	nsfmt = MwEncodeFormat(~MW_FMT_BORDERS, &fmt);

	if ( (fmt.borders & MW_BORDER_MASK) || is_alloc(b, s, row, col) ) {
		alloc_cell(b, s, row, col);
		b->sht[s].matrix[row][col].format = format;
	}
	if (!(fmt.borders & MW_BORDER_MASK)
	    && ret_type(b, s, row, col) == EMPTY
	    && std_fmt_get(b) != nsfmt) {
		std_fmt_set(b, nsfmt);
	}

	return 1;
}

/* ---
*/
char *ret_text(buffer *b, int s, int row, int col)
{
	if (!is_alloc(b, s, row, col)) return NULL;
	return b->sht[s].matrix[row][col].text;
}	/* ret_text */

/* ---
*/
int ret_interpreter(buffer *b, int s, int row, int col)
{
	if (!is_alloc(b, s, row, col)) return 0;
	return b->sht[s].matrix[row][col].interpreter;
}

/* ---
*/
cval ret_val(buffer *b, int s, int row, int col)
{
	register spread *mrow;	/* faster than matrix[row] */

	if (!is_alloc(b, s, row, col)) {
		cval value;
		value.number = 0;
		return value;
	}

	mrow = b->sht[s].matrix[row];
	return mrow[col].value;
}				/* ret_val */

/* ---
*/
char *ret_string(buffer *b, int s, int row, int col)
{
	register spread *mrow;
	char *v;
	if (!is_alloc(b, s, row, col)) return NULL;
	mrow = b->sht[s].matrix[row];
	if (mrow[col].type == STRING) {
		v = mrow[col].value.text;
		return v;
	}
	return NULL;
}

/* ---
*/
short ret_type(buffer *b, int s, int row, int col)
{
	register spread *mrow;	/* faster than matrix[row] */
	if (!is_alloc(b, s, row, col)) return EMPTY;
	mrow = b->sht[s].matrix[row];
	return mrow[col].type;
}				/* ret_type */

/* ---
*/
int ret_format(buffer *b, int s, int row, int col)
{
	if (!is_alloc(b, s, row, col)) return std_fmt_get(b);
	return b->sht[s].matrix[row][col].format;
}

/* ---
*/
char *ret_pvalue(char *buf, buffer *b, int s, int row, int col, int mode)
{
	static char p[1024];
	int type;
	double f;
	int d;
	time_t ttime;
	struct tm *ltime;
	char *fmt;
	MwFmt mfmt;

	f = 0.0;

	if (buf == NULL) buf = p;
	buf[0] = '\0';
	if (row > BUFFER_ROWS || col > BUFFER_COLS) {
		return NULL;
	}
	type = ret_type(b, s, row, col);

	if (mode < 0) {
		MwDecodeFormat(ret_format(b, s, row, col), MW_FMT_STYLE, &mfmt);
		mode = mfmt.style;
	}

	if (type == EMPTY || style_table[mode].type == INVISIBLE) {
		return buf;
	}
	if (type == STRING) {
		return strncpy(buf, ret_string(b, s, row, col), 1000);
	}
	if (type == ERROR) {
		return strcpy(buf, "ERROR");
	}
	if (type == LABEL || type == EMBED || type == MTEXT) {
		return strncpy(buf, ret_text(b, s, row, col), 1000);
	}

	/* we have now exhausted all options except visible numbers */
	f = ret_val(b, s, row, col).number;
	if (style_table[mode].type == STY_DEFAULT) {
		/* default means we get to guess */
		d = (long)f;
		if (ABS(d-f) < .001) mode = STY_INTEGER, fmt = "%d";
		else if (f > 10000000) mode = STY_FLOAT, fmt = "%g";
		else mode = STY_FLOAT, fmt = "%.2f";
	} else {
		fmt = style_table[mode].fmt;
		mode = style_table[mode].type;
	}
	switch (mode) {
	case STY_INTEGER:
		d = (long)f;
		sprintf(buf, fmt, d);
		break;
	case STY_FLOAT:
		sprintf(buf, fmt, f);
		break;
	case STY_TIME:
		ttime = (time_t) f;
		ltime = localtime(&ttime);

		strftime(buf, sizeof(p), fmt, ltime);

		break;
	case STY_PERCENT:
		sprintf(buf, fmt, 100*f);
		break;
	default:
		strcpy(buf, "Bogus format");
	}
	return buf;
}

/* ---
*/
int line_last_used(buffer *b, int s)
{
	register int r;
	for (r = b->sht[s].alloc_lines;
		r > P_MIN.row && b->sht[s].matrix[r] == NULL;
		r--);
	return r;
}

/* ---
*/
int col_last_used(buffer *b, int s, int row)
{
	register int i;
	if (row < P_MIN.row || row > b->sht[s].alloc_lines) return 0;
	if (b->sht[s].alloc_cols == NULL) return 0;

	for (i = b->sht[s].alloc_cols[row];
	     i > P_MIN.col && ret_type(b, s, row, i) == EMPTY; i--);
	return i;
}

/* ---
*/
void downshift_matrix(buffer *b, int s, int row)
{
	int i;

	alloc_cell(b, s, 1, 1);	/* magic */

	/* free the last line, if there is anything in it */
	if (b->sht[s].alloc_lines > BUFFER_ROWS && b->sht[s].matrix[BUFFER_ROWS])
		MwFree(b->sht[s].matrix[BUFFER_ROWS]);

	/* extend the line allocation in the matrix */
	if (b->sht[s].alloc_lines >= row && b->sht[s].alloc_lines < BUFFER_ROWS) {
		b->sht[s].alloc_lines++;
		b->sht[s].matrix = (spread **)MwRealloc(b->sht[s].matrix,
						(b->sht[s].alloc_lines+1)*sizeof(spread *));
		b->sht[s].alloc_cols = (int *)MwRealloc(b->sht[s].alloc_cols,
						(b->sht[s].alloc_lines+1)*sizeof(int));
	}

	/* extend the height table */
	if (b->sht[s].used_lines >= row && b->sht[s].used_lines < BUFFER_ROWS) {
		b->sht[s].used_lines++;
		b->sht[s].height = (int *)MwRealloc(b->sht[s].height, (b->sht[s].used_lines+1)*sizeof(int));
	}

	/* move all the stuff down one line */
	for (i = b->sht[s].alloc_lines; i > row; i--) {
		b->sht[s].matrix[i] = b->sht[s].matrix[i - 1];
		b->sht[s].alloc_cols[i] = b->sht[s].alloc_cols[i - 1];
	}
	for (i = b->sht[s].used_lines; i > row; i--)
		b->sht[s].height[i] = b->sht[s].height[i - 1];

	/* make the first line point to nothing */
	if (b->sht[s].alloc_lines >= row) {
		b->sht[s].matrix[row] = NULL;
		b->sht[s].alloc_cols[row] = 0;
	}
	if (b->sht[s].used_lines >= row)
		b->sht[s].height[row] = 20;

	update_all_references(b, s, row, P_MIN.col, BUFFER_ROWS, BUFFER_COLS, 1, 0);

	b->recalc = b->change = TRUE;

	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (row <= b->sht[s].plugin[i].row) b->sht[s].plugin[i].row++;
	}
}

/* ---
*/
void upshift_matrix(buffer *b, int s, int row)
{
	int i;

	alloc_cell(b, s, 1, 1);	/* magic */

	/* free the first line, if there is anything in it */
	if (b->sht[s].alloc_lines > row && b->sht[s].matrix[row])
		MwFree(b->sht[s].matrix[row]);

	/* we don't bother to shrink the matrix or height table */

	/* move everything up one line */
	for (i = row; i < b->sht[s].alloc_lines; i++) {
		b->sht[s].matrix[i] = b->sht[s].matrix[i + 1];
		b->sht[s].alloc_cols[i] = b->sht[s].alloc_cols[i + 1];
	}
	for (i = row; i < b->sht[s].used_lines; i++)
		b->sht[s].height[i] = b->sht[s].height[i + 1];

	/* make the last line point to nothing */
	if (b->sht[s].alloc_lines > row) {
		b->sht[s].matrix[b->sht[s].alloc_lines] = NULL;
		b->sht[s].alloc_cols[b->sht[s].alloc_lines] = 0;
	}
	if (b->sht[s].used_lines > row)
		b->sht[s].height[b->sht[s].used_lines] = 20;

	update_all_references(b, s, row, P_MIN.col, BUFFER_ROWS, BUFFER_COLS, -1, 0);

	b->recalc = b->change = TRUE;

	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (row <= b->sht[s].plugin[i].row) b->sht[s].plugin[i].row--;
	}
}

/* ---
*/
void rightshift_matrix(buffer *b, int s, int col)
{
	int i, j;
	empty_cell.format = std_fmt_get(b);

	alloc_cell(b, s, 1, 1);	/* magic */

	/* extend the width table if necessary */
	if (b->sht[s].used_cols >= col && b->sht[s].used_cols < BUFFER_COLS) {
		b->sht[s].used_cols++;
		b->sht[s].width = (int *)MwRealloc(b->sht[s].width, (b->sht[s].used_cols+1)*sizeof(int));
		for (i = b->sht[s].used_cols; i > col; i--)
			b->sht[s].width[i] = b->sht[s].width[i-1];
		b->sht[s].width[col] = 80;
	}

	/* process all the allocated lines */
	for (i = 1; i <= b->sht[s].alloc_lines; i++) {
		if (b->sht[s].matrix[i]) {
			/* check if it needs to grow */
			if (b->sht[s].alloc_cols[i] >= col) {
				b->sht[s].alloc_cols[i]++;
				b->sht[s].matrix[i] = (spread *)MwRealloc(b->sht[s].matrix[i],
						(b->sht[s].alloc_cols[i]+1)*sizeof(spread));
			}

			/* shift to the right */
			for (j = b->sht[s].alloc_cols[i]; j > col; j--) {
				b->sht[s].matrix[i][j] = b->sht[s].matrix[i][j - 1];
			}

			/* and make the leftmost column empty */
			if (b->sht[s].alloc_cols[i] > col) {
				b->sht[s].matrix[i][col] = empty_cell;
			}
		}
	}

	update_all_references(b, s, P_MIN.row, col, BUFFER_ROWS, BUFFER_COLS, 0, 1);

	b->recalc = b->change = TRUE;

	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (col <= b->sht[s].plugin[i].col) b->sht[s].plugin[i].col++;
	}
}

/* ---
*/
void leftshift_matrix(buffer *b, int s, int col)
{
	int i, j;

	empty_cell.format = std_fmt_get(b);
	/* do not shrink the width table */

	alloc_cell(b, s, 1, 1);	/* magic */

	for (j = col; j < b->sht[s].used_cols; j++)
		b->sht[s].width[j] = b->sht[s].width[j + 1];
	if (b->sht[s].used_cols >= col)
		b->sht[s].width[col] = 80;

	for (i = 1; i <= b->sht[s].alloc_lines; i++) {
		if (b->sht[s].matrix[i]) {
			/* do not free unused columns */

			/* shift to the left */
			for (j = col; j < b->sht[s].alloc_cols[i]; j++) {
				b->sht[s].matrix[i][j] = b->sht[s].matrix[i][j + 1];
			}

			/* and make the rightmost empty */
			if (b->sht[s].alloc_cols[i] >= col)
				b->sht[s].matrix[i][b->sht[s].alloc_cols[i]] = empty_cell;
		}
	}

	update_all_references(b, s, P_MIN.row, col, BUFFER_ROWS, BUFFER_COLS, 0, -1);

	b->recalc = b->change = TRUE;

	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (col <= b->sht[s].plugin[i].col) b->sht[s].plugin[i].col--;
	}
}

void fill_area(buffer *b, int s, int r1, int c1, int r2, int c2)
{
	int r, c;
	double d1 = 0.0, d2, dr = 0.0, dc = 0.0;
	int af;
	int type = ret_type(b, s, r1, c1);
	int intp = ret_interpreter(b, s, r1, c1);
	char *text = ret_text(b, s, r1, c1);
	cval value = ret_val(b, s, r1, c1);
	int tr = ret_type(b, s, r1+1, c1);
	int tc = ret_type(b, s, r1, c1+1);

	af = (IS_NUMBER(type)
	      && (r1 == r2 || IS_NUMBER(tr))
	      && (c1 == c2 || IS_NUMBER(tc)));

	if (IS_NUMBER(type)) {
		d1 = value.number;
		/* figure out row delta */
		if (IS_NUMBER(tr)) {
			d2 = ret_val(b, s, r1+1, c1).number;
			dr = d2-d1;
		}
		if (IS_NUMBER(tc)) {
			d2 = ret_val(b, s, r1, c1+1).number;
			dc = d2-d1;
		}
	}
	undo_save(b, s, r1, c1, r2, c2);
	for (r = r1; r <= r2; r++) {
		for (c = c1; c <= c2; c++) {
			if (af) {	/* autofill */
				char p[1024];
				cval dd;
				dd.number = d1+(r-r1)*dr+(c-c1)*dc;
				sprintf(p, "%g", dd.number);
				ins_data(b, C_interpreter, p, dd,
					CONSTANT, s, r, c);
			} else {
				ins_data(b, intp, text, value,
					type, s, r, c);
			}
			ins_format(b, s, r, c, ret_format(b, s, r1, c1));
			b->recalc = b->change = pr_scr_flag = TRUE;
		}
	}
}

char *pack_string(buffer *b, int s, int r1, int c1, int r2, int c2)
{
	int r, c, lastr, lastc, i, m;
	char *p, *data;

	m = 32000;
	data = MwMalloc(m);
	lastr = line_last_used(b, s);
	if (lastr > r2) lastr = r2;
	i = 0;
	for (r = r1; r <= lastr; r++) {
		lastc = col_last_used(b, s, r);
		if (lastc > c2) lastc = c2;
		for (c = c1; c <= lastc; c++) {
			p = ret_pvalue(NULL, b, s, r, c, -1);
			if (i+strlen(p)+10 > m) {
				m = i+strlen(p)+32000;
				data = MwRealloc(data, m);
			}
			strcpy(data+i, p);
			i += strlen(p);
			if (c < c2) data[i++] = ',';
		}
		data[i++] = '\n';
	}
	data[i] = '\0';
	return data;
}

void unpack_string(buffer *b, char *data, int s, int row, int col)
{
	char *p;
	int n = 0;
	cval value;
	value.number = 0;

	for (p = data; *p; p++)
		if (*p == '\n') n++;

	undo_save(b, s, row, col, row+n, col);

	while ((p = strchr(data, '\n')) != NULL) {
		*p = '\0';
		ins_data(b, siod_interpreter, data,
			value, LABEL, s, row, col);
		row++;
		data = p+1;
	}
	if (*data) {
		ins_data(b, siod_interpreter, data,
			value, LABEL, s, row, col);
	}
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

/* ---
Allocate a block of memory of sufficient size and encode the contents
   of a specified rectangular area into it. Caller must free.
*/

char *pack_area(buffer *b, int s, int r1, int c1, int r2, int c2,
	unsigned int *size)
{
	int r, c, i;
	int intp;
	char *data;

	/* use fixed size buffer for now */
	*size = 32000;
	data = MwMalloc(*size);
	sprintf(data, "%d %d", r2-r1+1, c2-c1+1);
	i = strlen(data)+1;

	for (r = r1; r <= r2 && r <= line_last_used(b, s); r++) {
		for (c = c1; c <= c2 && c <= col_last_used(b, s, r); c++) {
			switch (ret_type(b, s, r, c)) {
			case EMPTY:
				sprintf(data+i, "#");
				break;
			case LABEL:
				sprintf(data+i, "\"%s",
					ret_text(b, s, r, c));
				break;
			case CONSTANT:
				sprintf(data+i, "=%s",
					ret_text(b, s, r, c));
				break;
			case EMBED:
				sprintf(data+i, "m%s",
					ret_text(b, s, r, c));
				break;
			default:
				intp = ret_interpreter(b, s, r, c);
				sprintf(data+i, "+%s,%s",
					interpreter2name(intp),
					ret_text(b, s, r, c));
				break;
			}
			i += strlen(data+i)+1;
			if (i+2000 > *size) {
				*size += 32000;
				data = MwRealloc(data, *size);
				return NULL;
			}
		}
		/* use newline to mark end of line */
		sprintf(data+i, "\n");
		i += strlen(data+i)+1;
	}
	/* use null character to mark end of area */
	*(data+i) = '\0';
	return data;
}

/* ---
Unpack a previously encoded memory block into the position given.
*/

void unpack_area(buffer *b, char *data, int s, int r1, int c1)
{
	int r2, c2, r, c, rows, cols;
	int intp;
	char *comma, *texti;
	cval value;
	char name[256];
	int len;

	if (!data) return;
	sscanf(data, "%d %d", &rows, &cols);

	data += strlen(data)+1;
	r2 = r1+rows-1;
	c2 = c1+cols-1;
	/* blank out all cells in the target */
	for (r = r1; r <= r2 && r <= line_last_used(b, s); r++) {
		for (c = c1; c <= c2 && c <= col_last_used(b, s, r); c++) {
			ins_data(b, siod_interpreter,
				NULL, value, EMPTY, s, r, c);
		}
	}
	r = r1;
	c = c1;
	while (*data) {
		value.number = 0;
		switch (*data++) {
		case '"':
			texti = data;
			ins_data(b, siod_interpreter,
				texti, value, LABEL, s, r, c++);
			break;
		case '=':
			texti = data;
			value.number = strtod(texti, NULL);
			ins_data(b, siod_interpreter,
				texti, value, CONSTANT, s, r, c++);
			break;
		case '+':
			comma = strchr(data, ',');
			if (!comma) {
				fprintf(stderr,
					"unpack_area: no interpreter\n");
				return;
			}
			len = comma-data;
			strncpy(name, data, len);
			name[len] = '\0';
			intp = name2interpreter(name);
			if (intp < 0) intp = siod_interpreter;
			data = comma+1;
			ins_data(b, intp, data, value,
				EXPRESSION, s, r, c++);
			break;
		case '\n':
			r++;
			c = c1;
			break;
		case '#':
			c++;
			break;
		default:
			fprintf(stderr, "unpack_area: bogus data\n");
			fprintf(stderr, "\t%s\n", data-1);
			return;
		}
		data += strlen(data)+1;
	}
}

static struct {
	char *data;
	int s;
	int r1, c1;
} undo_buffer;

/* ---
Saves a rectangular area of the buffer. Called before any command
   that changes the buffer and should be undoable. Returns 0 for
   success and 1 for failure.
*/

int undo_save(buffer *b, int s, int r1, int c1, int r2, int c2)
{
	int size;

	MwFree(undo_buffer.data);
	undo_buffer.s = s;
	undo_buffer.r1 = r1;
	undo_buffer.c1 = c1;
	undo_buffer.data = pack_area(b, s, r1, c1, r2, c2, &size);
	return (undo_buffer.data == NULL);
}

/* ---
Puts back whatever was last saved by undo_save. Returns 0 for
   success and 1 for failure.
*/

int undo_restore(buffer *b)
{
	if (undo_buffer.data == NULL) return 1;
	unpack_area(b, undo_buffer.data, undo_buffer.s, undo_buffer.r1, undo_buffer.c1);
	undo_buffer.data = NULL;
	return 0;
}

