/*
   Pathetic Writer
   Copyright (C) 1997-2003  Ulric Eriksson <ulric@siag.nu>

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
 * matrix.c
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../common/plugin.h"

#include "pw.h"

int max_lines = BUFFER_ROWS;
int max_columns = BUFFER_COLS;


/* ---
Style code for PW. The styles are quite different from the ones in Siag,
but the principles are the same: Styles are encoded to integers so that
they can be handled by the general format code.
--- */

int nstyle;
style *style_table = NULL;

/* ---
*/
char *style2name(int i)
{
	if (i < 0 || i >= nstyle) return NULL;
	return style_table[i].name;
}

/* ---
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

static struct bp_style {
	MwFmt fmt;	/* the format we haven't initialized */
	int n;		/* index into style_table */
} *bp_styles = NULL;
static int nbp_styles = 0;	/* size of backlog */

static void add_bp_style(int n, MwFmt fmt)
{
	int i = nbp_styles++;
	bp_styles = MwRealloc(bp_styles, nbp_styles*sizeof *bp_styles);
	bp_styles[i].fmt = fmt;
	bp_styles[i].fmt.family = MwStrdup(fmt.family);
	bp_styles[i].fmt.fg = MwStrdup(fmt.fg);
	bp_styles[i].fmt.bg = MwStrdup(fmt.bg);
	bp_styles[i].n = n;
}

/* ---
*/
void do_bp_styles(void)
{
	int i;

	for (i = 0; i < nbp_styles; i++) {
		style_table[bp_styles[i].n].format =
			MwEncodeFormat(~0, &bp_styles[i].fmt);
		MwFree(bp_styles[i].fmt.family);
		MwFree(bp_styles[i].fmt.fg);
		MwFree(bp_styles[i].fmt.bg);
	}
	MwFree(bp_styles);
}

/* ---
A struct style has the fields name, format index, format, next.
The format has the fields family, size, bold, italic, uline, strike, fg, bg, style.

I omit the format index because we will compute it here.
The style in the format is useless here.
Finally, we don't use the bg colour.

The function still takes an awful lot of arguments.

A dilemma: this function is called from register-style before the ui
has been initialised. But we need to look up colours and fonts, which
won't work until we have a valid Display.

Why not run this *after* init_windows? Because documents are also
loaded before init_windows, so we must have valid styles. Besides,
load_pw also calls lookup_style.

My solution: when ok2print is false, don't look up fonts and colours
but save them in a separate array. Then look them up by calling
the function patch_styles from init_windows.
*/

int lookup_style(char *name, char *next,
		char *family, int size, int bold, int italic,
		int uline, int strike, char *fg)
{
	MwFmt fmt;
	style s;
	int i;

	s.name = MwStrdup(name);

	/* get the default format, so all fields are valid */
	MwDecodeFormat(0, ~0, &fmt);
	/* then change the ones we are interested in */
	fmt.family = family;
	fmt.size = size;
	fmt.bold = bold;
	fmt.italic = italic;
	fmt.uline = uline;
	fmt.strike = strike;
	fmt.fg = fg;

	/* now figure out where to put it */
	i = name2style(name);
	if (i == -1) {	/* new style */
		i = nstyle++;
		style_table = MwRealloc(style_table, nstyle*sizeof *style_table);
	} else {	/* redefine existing */
		MwFree(style_table[i].name);
	}

	if (ok2print) {		/* UI is initialised */
		s.format = MwEncodeFormat(~0, &fmt);
	} else {		/* do it later, then */
		s.format = 0;
		add_bp_style(i, fmt);
	}

	style_table[i] = s;
	if ((style_table[i].follower = name2style(next)) == -1)
		style_table[i].follower = 0;	/* always valid */
	return i;
}

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
*/
int style_height(int s)
{
	MwRichchar p = {'X', 0};
	p.fmt = style_table[s].format;
	return MwRcStrheight(&p, 1);
}

/* ---
this should work exactly like the format code in fonts.c, because I don't
know what fields will be added or removed in the future.

the styles are saved in a single level, just like the formats.
Attributes: name, height, next, family, size, bold, italic, uline, strike, fg
*/

void save_styles(FILE *fp, int i)
{
	MwFmt fmt;
	style st;

	st = style_table[i];
	MwDecodeFormat(st.format, ~0, &fmt);
	fprintf(fp, ".style %d\n", i);
	fprintf(fp, "name %s\n", st.name);
	fprintf(fp, "next %d\n", st.follower);
	fprintf(fp, "family %s\n", fmt.family);
	fprintf(fp, "size %d\n", fmt.size);
	fprintf(fp, "bold %d\n", fmt.bold);
	fprintf(fp, "italic %d\n", fmt.italic);
	fprintf(fp, "uline %d\n", fmt.uline);
	fprintf(fp, "strike %d\n", fmt.strike);
	fprintf(fp, "fg %s\n", fmt.fg);
	fprintf(fp, "end\n");
}

/* ---
*/
int load_styles(FILE *fp)
{
	style st = style_table[0];
	MwFmt fmt;

	char name[1000], family[1000], fg[1000], b[1000];
	MwDecodeFormat(0, ~0, &fmt);
	strcpy(name, st.name);
	st.name = name;
	strcpy(family, fmt.family);
	fmt.family = family;
	strcpy(fg, fmt.fg);
	fmt.fg = fg;
	while (fgets(b, sizeof b, fp) && strncmp(b, "end", 3)) {
		MwChomp(b);
		if (!strncmp(b, "name ", 5))
			strcpy(st.name, b+5);
		else if (!strncmp(b, "next ", 5))
			st.follower = name2style(b+5);
		else if (!strncmp(b, "family ", 7))
			strcpy(fmt.family, b+7);
		else if (!strncmp(b, "size ", 5))
			fmt.size = strtol(b+5, NULL, 10);
		else if (!strncmp(b, "bold ", 5))
			fmt.bold = strtol(b+5, NULL, 10);
		else if (!strncmp(b, "italic ", 7))
			fmt.italic = strtol(b+7, NULL, 10);
		else if (!strncmp(b, "uline ", 6))
			fmt.uline = strtol(b+6, NULL, 10);
		else if (!strncmp(b, "strike ", 7))
			fmt.strike = strtol(b+7, NULL, 10);
		else if (!strncmp(b, "fg ", 3))
			strcpy(fmt.fg, b+3);
		/* ignore anything we don't grok */
	}
	return lookup_style(st.name, style2name(st.follower),
			fmt.family, fmt.size, fmt.bold, fmt.italic,
			fmt.uline, fmt.strike, fmt.fg);
}

/* end of style code */


/* ---
   Frees the memory used by the matrix.
*/

void free_text(rich_text *c)
{
	MwFree(c);
}

static MwRichchar empty_char = {
	'\0', 0
};


/* ---
make sure the char is allocated
note that this function changes the buffer
If row is greater than any previously used line, new lines are allocated
up to and including row. The new lines are initialized to be empty.
*/

void alloc_line(buffer *b, int sht, int row)
{
	int i = b->sht[sht].used_lines;
	if (row <= b->sht[sht].alloc_lines) {
		return;
	}
	b->sht[sht].alloc_lines = b->sht[sht].used_lines = MAX(b->sht[sht].used_lines, row);
	b->sht[sht].text = (rich_text *)MwRealloc(b->sht[sht].text,
			(b->sht[sht].alloc_lines+1)*sizeof(rich_text));
	i++;	/* don't overwrite the last used line */
	while (i <= b->sht[sht].alloc_lines) {
		b->sht[sht].text[i].height = style_height(0);
		b->sht[sht].text[i].sty = 0;
		b->sht[sht].text[i].adj = MW_HADJ_LEFT;
		b->sht[sht].text[i].bop = 1;
		b->sht[sht].text[i++].p = MwRcStrdup(&empty_char);
	}
	b->sht[sht].used_lines = b->sht[sht].alloc_lines;
}

/* ---
kludge to keep things from breaking fatally
any positions after row r are reset to (r,0)
NB: this function does not take care of point
*/

void position_kludge(buffer *b, int sht, int r)
{
	window *w = w_list;
	position p = make_position(r, 0);

	do {
		if (w->buf == b) {
			sheet *s = w->buf->sht;
			if (s[sht].blku.row >= r) s[sht].blku = p;
			if (s[sht].blkl.row >= r) s[sht].blkl = p;
		}
		w = w->next;
	} while (w != w_list);

	if (b->sht[sht].mark_pos.row >= r) b->sht[sht].mark_pos = p;
}

/* ---
this one only resets positions that don't exist
it does take care of everything, including point
*/

void position_kludge2(void)
{
	window *w = w_list;
	buffer *b;
	unsigned int r;
	int s;
	sheet *st;

	do {
		s = w->sht;
		b = w->buf;
		st = b->sht;
		r = line_last_used(b, s);
		if (r < 1) r = 1;
		st[s].blku.row = MIN(st[s].blku.row, r);
		st[s].blku.col = MIN(st[s].blku.col,
				col_last_used(b, s, st[s].blku.row));
		st[s].blkl.row = MIN(st[s].blkl.row, r);
		st[s].blkl.col = MIN(st[s].blkl.col,
				col_last_used(b, s, st[s].blkl.row));
		st[s].point_pos.row = MIN(st[s].point_pos.row, r);
		st[s].point_pos.col = MIN(st[s].point_pos.col,
				col_last_used(b, s, st[s].point_pos.row));
		w = w->next;
	} while (w != w_list);
	b = b_list;
	do {
		for (s = 0; s < b->nsht; s++) {
			r = line_last_used(b, s);
			if (r < 1) r = 1;
			b->sht[s].mark_pos.row = MIN(b->sht[s].mark_pos.row, r);
			b->sht[s].mark_pos.col = MIN(b->sht[s].mark_pos.col,
					col_last_used(b, s, b->sht[s].mark_pos.row));
		}
		b = b->next;
	} while (b != b_list);
}

/* ---
shift the next line down, make it the follower's style,
copy the remainder of the line and truncate this line
Nope; make it the *same* style and not bop. (990518)
*/

int split_line(buffer *buf, int s, int row, int col)
{
	position_kludge(buf, s, row);

	alloc_line(buf, s, row+1);
	if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
		if (col == 0) downshift_text(buf, s, row);
		else downshift_text(buf, s, row+1);
		return 1;
	}

	downshift_text(buf, s, row+1);	/* make room for the new line */
	buf->sht[s].text[row+1].sty = buf->sht[s].text[row].sty;
	buf->sht[s].text[row+1].adj = buf->sht[s].text[row].adj;
	buf->sht[s].text[row+1].bop = 0;
	buf->sht[s].text[row+1].p = MwRcStrdup(buf->sht[s].text[row].p+col);
	if (MwRcStrlen(buf->sht[s].text[row].p) > col)
		buf->sht[s].text[row].p[col] = empty_char;	/* truncate */
	/* check line heights for both lines */
	check_line_height(buf, s, row);
	check_line_height(buf, s, row+1);
	buf->change = TRUE;
	return 1;
}

/* ---
*/
int join_lines(buffer *buf, int s, int row)
{
	MwRichchar *p;
	position_kludge(buf, s, row);

	if (row >= buf->sht[s].used_lines) return 0;	/* nothing to join */

	/* special case if either line is embedded object */
	if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
		/* can only join with empty line */
		if (line_length(buf, s, row+1) == 0) {
			upshift_text(buf, s, row+1);
			return 1;
		} else return 0;	/* can't do that */
	}

	if (buf->sht[s].text[row+1].sty == MW_STY_EMBED) {
		if (line_length(buf, s, row) == 0) {
			upshift_text(buf, s, row);
			return 1;
		} else return 0;
	}

	p = buf->sht[s].text[row].p;
	buf->sht[s].text[row].p = MwRcStrins(p, buf->sht[s].text[row+1].p, MwRcStrlen(p));
	MwFree(p);
	upshift_text(buf, s, row+1);
	return 1;
}

/* ---
*/
int ins_text(buffer *buf, int s, position pos, unsigned char *t, int fmt)
{
	MwRichchar *p;
	MwRichchar *q;
	if (ret_style(buf, s, pos.row) == MW_STY_EMBED) return 0;	/* nope */

	alloc_line(buf, s, pos.row);
	p = buf->sht[s].text[pos.row].p;
	q = MwRcMakerich(t, fmt);
	buf->sht[s].text[pos.row].p = MwRcStrins(p, q, pos.col);
	if (buf->height_interest)
		check_line_height(buf, s, pos.row);
	MwFree(p);
	MwFree(q);
	return 1;
}

/* ---
*/
int ins_char(buffer *buf, int sht, int r, int c, int s, int fmt)
{
	unsigned char b[2];
	b[0] = s;
	b[1] = '\0';
	return ins_text(buf, sht, make_position(r, c), b, fmt);
}

/* ---
Make a plaintext copy of row r int the string p.
The string must be freed by caller
*/

unsigned char *peek_line(buffer *b, int s, int r)
{
	if (b == NULL) return NULL;
	if (r > b->sht[s].used_lines) return NULL;
	if (b->sht[s].text == NULL) return NULL;
	return MwRcMakeplain(b->sht[s].text[r].p);
}

/* ---
*/
int peek_char(buffer *b, int s, int r, int c)
{
	int result;
	unsigned char *p = peek_line(b, s, r);
	result = p[c];
	MwFree(p);
	return result;
}

/* ---
Figure out where to break a line
*/

static int find_suitable_space(buffer *b, int s, int r)
{
	int c = 0;
	int space = 0;	/* current "suitable" space */
	int len = line_length(b, s, r);

	while (c < len && line_width(b, s, r, c) <=
			b->paper_width-b->left_margin-b->right_margin) {
		if (isspace(peek_char(b, s, r, c))) space = c;
		c++;
	}
	return space;
}

/* ---
Checks if the line is too long for the paper width and breaks it if
necessary. Continues with the next line.

New and simplified algorithm, using bops: Join all the lines of the
entire paragraph, then split until done.
*/

int rebreak_line(buffer *buf, int s, int row)
{
	int maxwidth = buf->paper_width-buf->left_margin-buf->right_margin;
	int lastrow = buf->sht[s].alloc_lines;

	while (row < lastrow && !ret_bop(buf, s, row+1)) {
		ins_char(buf, s, row+1, 0, ' ', ret_format(buf, s, row, 0));
		join_lines(buf, s, row);
		pr_scr_flag = TRUE;
	}

	while (line_width(buf, s, row, line_length(buf, s, row)) > maxwidth) {
		int col = find_suitable_space(buf, s, row);
		if (col <= 0) break;	/* none found */
		split_line(buf, s, row++, col);
		del_char(buf, s, row, 0);
		pr_scr_flag = TRUE;
	}
	return row;
}

extern void embed_unload(char *);	/* FIXME */

/* ---
returns number of deleted characters
*/

int del_char(buffer *buf, int s, int row, int col)
{
	if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
		if (col == 0) {		/* remove the object and the line */
			char *p = (char *)MwRcMakeplain(buf->sht[s].text[row].p);
			embed_unload(p);	/* can't be NULL */
			MwFree(p);
			upshift_text(buf, s, row);
			return 1;
		} else return 0;
	}

	MwRcStrcpy(buf->sht[s].text[row].p+col, buf->sht[s].text[row].p+col+1);
	buf->change = TRUE;
	return 1;
}

/* ---
returns number of deleted characters
*/

int del_text(buffer *buf, int s, position p, int length)
{
	int i;

	for (i = 0; i < length; i++)
		if (!del_char(buf, s, p.row, p.col)) break;
	return i;
}

/* ---
*/
int del_lines(buffer *buf, int s, int row, int count)
{
	int i;

	/* don't delete lines that aren't there */
	if (row+count-1 > buf->sht[s].alloc_lines) count = buf->sht[s].alloc_lines-row+1;

	/* delete count lines, starting at row */
	for (i = 0; i < count; i++) {
		if (buf->sht[s].text[row+i].sty == MW_STY_EMBED) {
			char *p = (char *)MwRcMakeplain(buf->sht[s].text[row].p);
			embed_unload(p);
			MwFree(p);
		}
		MwFree(buf->sht[s].text[row+i].p);
	}

	/* shift the rest of the lines up */
	for (i = row; i+count <= buf->sht[s].alloc_lines; i++) {
		buf->sht[s].text[i] = buf->sht[s].text[i+count];
	}

	/* clear the last count lines */
	for (; i <= buf->sht[s].alloc_lines; i++) {
		buf->sht[s].text[i].p = NULL;
	}
	buf->sht[s].alloc_lines -= count;
	buf->sht[s].used_lines -= count;
	if (count) buf->change = TRUE;
	return count;
}

/* ---
*/
int ins_format(buffer *buf, int s, int row, int c1, int c2, int format)
{
	MwRichchar *line;
	alloc_line(buf, s, row);
	line = buf->sht[s].text[row].p;
	while (c1 < c2 && c1 < MwRcStrlen(line)) {
		line[c1++].fmt = format;
	}
	return 1;
}

/* ---
*/
int ret_format(buffer *buf, int s, int row, int col)
{
	MwRichchar *line;
	if (row > buf->sht[s].used_lines) return style_table[0].format;
	line = buf->sht[s].text[row].p;
	if (!line || line[0].c == '\0') {
		/* empty line, try to get from previous line(s) */
		int pr = row-1;
		while (pr >= 1 && ret_style(buf, s, pr) == ret_style(buf, s, row)) {
			line = buf->sht[s].text[pr].p;
			col = MwRcStrlen(line);
			if (col) return line[col-1].fmt;
			pr--;
		}
		/* give up and return the default format for the style */
		return style_table[buf->sht[s].text[row].sty].format;
	}
	if (col >= MwRcStrlen(line)) col = MwRcStrlen(line)-1;
	return line[col].fmt;
}

/* ---
*/
void set_style(buffer *b, int s, int row, int sty)
{
	alloc_line(b, s, row);
	b->sht[s].text[row].sty = sty;
	b->sht[s].text[row].height = style_height(sty);
}

/* ---
*/
int ret_style(buffer *b, int s, int row)
{
	if (row > b->sht[s].used_lines) return MW_STY_DEFAULT;
	return b->sht[s].text[row].sty;
}

/* ---
*/
void set_bop(buffer *b, int s, int row, int bop)
{
	alloc_line(b, s, row);
	b->sht[s].text[row].bop = bop;
}

/* ---
*/
int ret_bop(buffer *b, int s, int row)
{
	if (row > b->sht[s].used_lines) return 1;
	return b->sht[s].text[row].bop;
}

/* ---
*/
int ret_hadj(buffer *b, int s, int row)
{
	if (row > b->sht[s].used_lines) return MW_HADJ_LEFT;
	return b->sht[s].text[row].adj & MW_HADJ_MASK;
}

/* ---
*/
int line_last_used(buffer *buf, int s)
{
	return buf->sht[s].used_lines;
}

/* ---
this means strlen
*/

int col_last_used(buffer *buf, int s, int row)
{
	return line_length(buf, s, row);	/* hmm */
}

/* ---
*/
void downshift_text(buffer *b, int s, int row)
{
	int r;
	int i;
	b->sht[s].used_lines++;
	alloc_line(b, s, b->sht[s].used_lines);
	for (r = b->sht[s].used_lines; r > row; r--) {
		b->sht[s].text[r] = b->sht[s].text[r-1];
	}
	b->sht[s].text[r].sty = 0;
	b->sht[s].text[r].height = style_height(0);
	b->sht[s].text[r].adj = MW_HADJ_LEFT;
	b->sht[s].text[r].p = NULL;
	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (b->sht[s].plugin[i].row >= row) b->sht[s].plugin[i].row++;
	}
	b->change = pr_scr_flag = TRUE;
}

/* ---
*/
void upshift_text(buffer *b, int s, int row)
{
	int r;
	int i, j;
	b->sht[s].used_lines--;
	for (r = row; r <= b->sht[s].used_lines; r++) {
		b->sht[s].text[r] = b->sht[s].text[r+1];
	}
	/* this line is still allocated. We clear it just in case */
	b->sht[s].text[r].sty = 0;
        b->sht[s].text[r].height = style_height(0);
        b->sht[s].text[r].adj = MW_HADJ_LEFT;
	b->sht[s].text[r].p = NULL;
	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (b->sht[s].plugin[i].row == row) {
			plugin_stop(b->sht[s].plugin[i].ph);
			MwFree(b->sht[s].plugin[i].name);
			b->sht[s].nplugin--;
			for (j = i; j < b->sht[s].nplugin; j++)
				b->sht[s].plugin[j] = b->sht[s].plugin[j+1];
		} else if (b->sht[s].plugin[i].row > row) {
			b->sht[s].plugin[i].row--;
		}
	}
	b->change = pr_scr_flag = TRUE;
}

/* ---
Return the page a row is on. Currently broken: always returns 1.
*/

int row2page(buffer *b, int s, int row)
{
	return 1;
}

/* ---
Return the row a page starts on. Currently broken: always returns 1.
*/

int page2row(buffer *b, int s, int page)
{
	return 1;
}

char *pack_string(buffer *buf, int s,
		int r1, int c1, int r2, int c2)
{
	unsigned int dummy;
	return pack_string_area(buf, s, r1, c1, r2, c2, &dummy);
}

/* ---
*/
char *pack_string_area(buffer *buf, int s,
		int r1, int c1, int r2, int c2, unsigned int *size)
{
	int n = 0, m = 0;
	char *data;

	data = MwMalloc(1);

	if (r2 > line_last_used(buf, s)) r2 = line_last_used(buf, s);
	while (r1 < r2 || (r1 == r2 && c1 <= c2)) {
		int c = peek_char(buf, s, r1, c1);
		if (n >= m) {
			m += 1000;
			data = MwRealloc(data, m+1);
		}
		data[n++] = c?c:'\n';
		if (c1 < line_length(buf, s, r1)) c1++;
		else r1++, c1 = 0;
	}
	data[n] = '\0';
	*size = n;
	return data;
}

/* ---
this one only does plaintext for now
*/

char *pack_area(buffer *buf, int s,
		int r1, int c1, int r2, int c2, unsigned int *size)
{
	if (r1 == -1 || r2 == -1 || c1 == -1 || c2 == -1) return NULL;
	return pack_string_area(buf, s, r1, c1, r2, c2, size);
}

/* ---
*/
void unpack_string_area(buffer *buf, char *data, int s, int row, int col)
{
	char *p;

	while (*data) {		/* NUL marks end of data */
		p = strchr(data, '\n');		/* look for eol */
		if (p) {	/* found one; copy data, then split line */
			*p = '\0';
			ins_text(buf, s, make_position(row, col),
				(unsigned char *)data,
				ret_format(buf, s, row, col));
			split_line(buf, s, row, col+strlen(data));
			row++;
			col = 0;
			data = p+1;	/* start of next line */
		} else {	/* last line; copy data, no split */
			ins_text(buf, s, make_position(row, col),
				(unsigned char *)data,
				ret_format(buf, s, row, col));
			data += strlen(data);	/* end of data */
		}
	}
}

/* ---
*/
void unpack_area(buffer *buf, char *data, int s, int row, int col)
{
	return unpack_string_area(buf, data, s, row, col);
}

