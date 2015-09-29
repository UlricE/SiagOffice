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

/* ---
   fileio_ps.c

   Produces data suitable for printing on a Postscript device. I have made
   several assumptions to simplify things:
   - paper size is A4 = 595x842 points
   - margin on all sides = 72 points = 1 inch
   - 1 pixel = 1 point means no scaling is necessary
   - PS font has same geometry as X font
   - no background pattern
   - don't draw the grid
   - The lpr command is used for printing

   This works for me, but will probably break (more or less) for anyone
   not using my printer (a NEC S62P) with A4 paper.

   971101: use properties to customize the output.
	ps_paper_width		paper width in points
	ps_paper_height		paper height in points
	ps_top_margin		top margin in points
	ps_left_margin		left margin in points
	ps_right_margin		right margin in points
	ps_bottom_margin	bottom margin in points
	ps_page_header		page header in special format
	ps_page_footer		page footer in special format

   The header/footer format is as follows:
	&n	expands to buffer name
	&p	expands to page number
	&	expands to %
	any other character is simply copied as it is

   980802: print plugins. Before terminating each page, print every plugin
	that is completely or partially displayed on that page.

981210: Let's print in landscape mode. To do this, we need to:
 - use the same paper size as before
 - use the same bounding box as before
 - 90 rotate (rotate 90 degrees)
 - 0 -pw translate, where pw is the paper width

981213: More properties.
	ps_orientation		portrait or landscape
	ps_protection		respect protected rows/columns (yes or no)

990424: Use the same machinery as PW, i.e. abandon the properties that
	were previously used.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>

#include <unistd.h>

#include <Mowitz/MwFormat.h>
#include "../common/common.h"
#include "../common/plugin.h"
#include <Mowitz/MwUtils.h>

#include "calc.h"

char *psformat;

static int pageno;
static int ps_paper_width;
static int ps_paper_height;
static int ps_prot_row = 1, ps_prot_col = 1;

static void print_cell(FILE *fp, buffer *buf,
		int s, int row, int col, int x_base, int y_base)
{
	char b[1024], *p;
	int x_pos, y_pos, text_width;
	int f = ret_format(buf, s, row, col);
	MwFmt fmt;
	MwRichchar *rcp;
	int cw, ch;
	int fd;
	int text_height;

	cw = cell_width(buf, s, col);
	ch = cell_height(buf, s, row);
	MwDecodeFormat(f, ~0, &fmt);
	if (fmt.borders & MW_BORDER_MASK) {
		MwPsSetColor(fp, 0, 0, 0);
		fprintf(fp, "newpath\n");
		if (fmt.borders & MW_BORDER_BOTTOM) {
			fprintf(fp, "%d %d moveto\n", x_base, y_base);
			fprintf(fp, "%d %d lineto\n",
				x_base + cw, y_base);
		}
		if (fmt.borders & MW_BORDER_RIGHT) {
			if (!(fmt.borders & MW_BORDER_BOTTOM))
				fprintf(fp, "%d %d moveto\n",
				  x_base + cw, y_base);
			fprintf(fp, "%d %d lineto\n",
				x_base + cw, y_base + ch);
		}
		if (fmt.borders & MW_BORDER_TOP) {
			if (!(fmt.borders & MW_BORDER_RIGHT))
				fprintf(fp, "%d %d moveto\n",
					x_base + cw, y_base + ch);
			fprintf(fp, "%d %d lineto\n",
				x_base, y_base + ch);
		}
		if (fmt.borders & MW_BORDER_LEFT) {
			if (!(fmt.borders & MW_BORDER_TOP))
				fprintf(fp, "%d %d moveto\n",
				 x_base, y_base + ch);
			fprintf(fp, "%d %d lineto\n",
				x_base, y_base);
		}
		fprintf(fp, "stroke\n");
	}

	b[0] = '\0';
	if (ret_type(buf, s, row, col) != EMPTY)
		MwPsSetFont(fp, f);

	ret_pvalue(b, buf, s, row, col, -1);

	if (b[0]) {	/* don't print empty cells */
		rcp = MwRcMakerich(b, f);
		text_width = MwRcStrwidth(rcp, -1);
		text_height = MwRcStrheight(rcp, -1);

		switch (fmt.hadj & MW_HADJ_MASK) {
		case MW_HADJ_CENTER:
			x_pos = (cw - text_width) / 2;
			break;
		case MW_HADJ_RIGHT:
			x_pos = cw - text_width - 5;
			break;
		default:
			x_pos = 5;
		}

		fd = text_height/4;	/* font descent heuristic */
		switch (fmt.vadj & MW_VADJ_MASK) {
		case MW_VADJ_BOTTOM:
			y_pos = 5;
			break;
		case MW_VADJ_TOP:
			y_pos = ch - text_height;
			break;
		default:	/* MW_VADJ_CENTER */
			y_pos = (ch - text_height) / 2 + fd;
		}

		fprintf(fp, "newpath\n");
		fprintf(fp, "%d %d moveto\n", x_base + x_pos, y_base + y_pos);

	/* print letters and digits as is, the rest in octal */
		fprintf(fp, "(");
		for (p = b; *p; p++) {
			int c = *p & 0xFF;
			if (isalnum(c))
				putc(c, fp);
			else
				fprintf(fp, "\\%03o", c);
		}
		fprintf(fp, ")\n");
		fprintf(fp, "show\n");
		MwFree(rcp);
	}
}

/* ---
How to print the whole document without wasting paper

	The document can in theory contain up to 1000 rows by 1000 columns,
	but clearly in any real-world spreadsheet most of that area
	is unused and we would be printing hundreds of blank pages.
	The easy way is to check all the used rows, find the longest one
	and print the resulting square. This calls for a function print_area
	which takes the top left and bottom right coordinates as arguments
	and prints everything inbetween. The same function can
	also be used to print the block.
*/

static int page_height(buffer *buf, int s, int r)
{
	int y_base, i;

	y_base = ps_paper_height - buf->top_margin;
	/* subtract the protected rows */
	for (i = 1; i < ps_prot_row; i++)
		y_base -= cell_height(buf, s, i);
	for (i = r; y_base > buf->bottom_margin; i++)
		y_base -= cell_height(buf, s, i);
	return i-r;
}

static int page_width(buffer *buf, int s, int c)
{
	int x_base, j;

	x_base = buf->left_margin;
	/* subtract the protected cells */
	for (j = 1; j < ps_prot_col; j++)
		x_base += cell_width(buf, s, j);
	for (j = c;
	     x_base+cell_width(buf, s, j) < ps_paper_width - buf->right_margin;
	     j++)
		x_base += cell_width(buf, s, j);
	return j-c;
}

static void expand(FILE *fp, int x, int y, char *from, char *name, int number)
{
        char b[1024], c[1024];
        int i;
	time_t t;
	struct tm *lt;

        unsigned int text_height, text_width;

        /* set the font */
        MwPsSetFont(fp, 0);

        /* first make the real header/footer string */

	/* expand the time */
	time(&t);
	lt = localtime(&t);
	strftime(c, sizeof c, from, lt);

	from = c;	/* new format string */

        i = 0;
        while (*from && (i < sizeof b)) {
                if (*from == '&') {
                        from++;
                        if (*from == '\0') break;
			else switch (*from) {
			case 'n':
                                strcpy(b+i, name);
                                i += strlen(b+i);
				break;
			case 'p':
                                sprintf(b+i, "%d", number);
                                i += strlen(b+i);
				break;
			default:
                        	b[i++] = *from;
				break;
			}
                } else b[i++] = *from;
                from++;
        }
	b[i] = '\0';

        /* next center it around (x,y) */

        text_height = ps_font_height(0);
        text_width = ps_text_width(0, b);
        fprintf(fp, "newpath\n");
	MwPsSetColor(fp, 0, 0, 0);
        fprintf(fp, "%d %d moveto\n", x - text_width/2, y - text_height/2);

        /* finally print the string */
        fprintf(fp, "(");
        for (i = 0; b[i]; i++) {
                int c = b[i] & 0xFF;
                if (isalnum(c))
                        putc(c, fp);
                else
                        fprintf(fp, "\\%03o", c);
        }
        fprintf(fp, ")\n");
        fprintf(fp, "show\n");
}

static void page_header(FILE *fp, buffer *b)
{
	char *ps_page_header = b->header;
	if (pageno == b->first_page_number && b->header_on_first == 0)
		return;

	if (!ps_page_header) ps_page_header = "&n";	/* buffer name */
	expand(fp, ps_paper_width/2,
		ps_paper_height-b->header_margin,
		ps_page_header, b->name, pageno);
}

static void page_footer(FILE *fp, buffer *b)
{
	char *ps_page_footer = b->footer;
	if (pageno == b->first_page_number && b->header_on_first == 0)
		return;

	if (!ps_page_footer) ps_page_footer = "&p";	/* page number */
	expand(fp, ps_paper_width/2,
		b->footer_margin,
		ps_page_footer,
		b->name, pageno);
}


/* ---
First all protected cells are printed, then as many as will fit on
the remaining width of the paper (not less than one).
Returns the next cell in turn to be printed.
*/

static int print_row(FILE *fp, buffer *buf, int s,
		int width, int y_base, int row, int top_col)
{
	int col;
	int x_base = buf->left_margin;
	for (col = 1; col < ps_prot_col; col++) {
		if (ret_type(buf, s, row, col) == EMBED) {
			ps_embed_print(fp, ret_text(buf, s, row, col),
				x_base, y_base);
			continue;
		}

		print_cell(fp, buf, s, row, col, x_base, y_base);
		x_base += cell_width(buf, s, col);
	}
	for (col = top_col;
	     col == top_col || x_base + cell_width(buf, s, col)
			<= ps_paper_width - buf->right_margin;
	     col++) {
		if (ret_type(buf, s, row, col) == EMBED) {
			ps_embed_print(fp, ret_text(buf, s, row, col),
				x_base, y_base);
			continue;
		}

		print_cell(fp, buf, s, row, col, x_base, y_base);
		x_base += cell_width(buf, s, col);
	}
	return col;
}

/* ---
First all protected rows are printed, then as many as will fit on
the remaining height of the paper (not less than one).
Returns the next row in turn to be printed.
*/

static int print_page(FILE *fp, buffer *buf, int s,
		int fromr, int fromc, int tor, int toc)
{
	int row;
	int i, y_base;
	int x, y;
	int width, height;

	fprintf(fp, "gsave\n");
	if (buf->orientation == LANDSCAPE) {
		fprintf(fp, "90 rotate\n");
		fprintf(fp, "0 %d translate\n", -ps_paper_height);
	}

	page_header(fp, buf);
	page_footer(fp, buf);

	y_base = ps_paper_height - buf->top_margin;
	for (row = 1;
	     row < ps_prot_row && y_base > buf->bottom_margin;
	     row++) {
		y_base -= cell_height(buf, s, row);
		print_row(fp, buf, s, width, y_base, row, fromc);
	}
	for (row = fromr;
	     row == fromr || y_base > buf->bottom_margin;
	     row++) {
		y_base -= cell_height(buf, s, row);
		print_row(fp, buf, s, width, y_base, row, fromc);
	}

	/* print plugins here! */
/* FIXME: this positioning code will be broken with protected cells */
	for (i = 0; i < buf->sht[s].nplugin; i++) {
		int topx, topy, botx, boty, plx, ply;
		buffer_global_coords(buf, s, fromr, fromc, &topx, &topy);
		botx = topx+ps_paper_width-buf->left_margin-buf->right_margin;
		boty = topy+ps_paper_height-buf->top_margin-buf->bottom_margin;
		buffer_global_coords(buf, s, buf->sht[s].plugin[i].row,
			buf->sht[s].plugin[i].col, &plx, &ply);
		plugin_size_get(buf->sht[s].plugin[i].ph, &width, &height);
		if (plx+width < topx || plx > botx) continue;
		if (ply+height < topy || ply > boty) continue;
		x = buf->left_margin+plx-topx;
		y = ps_paper_height-buf->top_margin-(ply-topy)-height;
		/* and let the plugin do its thing */
		plugin_print(buf->sht[s].plugin[i].ph, fp, x, y, 1);
	}

	fprintf(fp, "grestore\n");
	fprintf(fp, "showpage\n");
	return row;
}

static int save(char *fn, buffer *buf)
{
	int fromr, fromc, tor, toc;
	int i, clu;
	int s;
	FILE *fp;
	int r, c;
	int pages;
	time_t t;

	fp = fopen(fn, "w");

	/* print this before the actual data to display */
	fprintf(fp, "%%!PS-Adobe-2.0\n");
	fprintf(fp, "%%%%Creator: %s\n", version);
	fprintf(fp, "%%%%Title: Siag\n");	/* todo: print better title */
	t = time(NULL);
	fprintf(fp, "%%%%CreationDate: %s", ctime(&t));
	fprintf(fp, "%%%%Pages: (atend)\n");
	fprintf(fp, "%%%%PageOrder: Ascend\n");
	fprintf(fp, "%%%%BoundingBox: %d %d %d %d\n",
		0, 0, buf->paper_width, buf->paper_height);
	if (buf->orientation == LANDSCAPE) {
		fprintf(fp, "%%%%Orientation: Landscape\n");
		ps_paper_width = buf->paper_height;
		ps_paper_height = buf->paper_width;
	} else {
		fprintf(fp, "%%%%Orientation: Portrait\n");
		ps_paper_width = buf->paper_width;
		ps_paper_height = buf->paper_height;
	}
	fprintf(fp, "%%%%DocumentPaperSizes: %s\n", buf->paper_name);
	fprintf(fp, "%%%%EndComments\n");

	/* Use ISO-Latin1 encoding */
	fprintf(fp, "%%%%BeginProlog\n");
	MwPsMakeFonts(fp);
	fprintf(fp, "%%%%EndProlog\n");

	/* print all the pages */
	pages = 0;
	pageno = buf->first_page_number;
	for (s = 0; s < buf->nsht; s++) {
		if (buf->respect_protection) {
			ps_prot_row = buf->sht[s].prot.row;
			ps_prot_col = buf->sht[s].prot.col;
		} else {
			ps_prot_row = 1;
			ps_prot_col = 1;
		}
		fromr = ps_prot_row;
		fromc = ps_prot_col;
		tor = line_last_used(buf, s);
		toc = 1;
		for (i = 1; i <= tor; i++) {
			clu = col_last_used(buf, s, i);
			if (clu > toc) toc = clu;
		}
		for (i = 0; i < buf->sht[s].nplugin; i++) {
			tor = MAX(tor, buf->sht[s].plugin[i].row);
			toc = MAX(toc, buf->sht[s].plugin[i].col);
		}

		for (r = fromr; r <= tor; r += page_height(buf, s, r)) {
			for (c = fromc;
			     c <= toc;
			     c += page_width(buf, s, c)) {
				pages++;
				MwPsSetFont(fp, -1);
				fprintf(fp, "%%%%Page: %d %d\n", pages, pages);

				print_page(fp, buf, s, r, c, tor, toc);
				pageno++;
			}
		}
	}

	/* this goes after */
	fprintf(fp, "%%%%Trailer\n");
	fprintf(fp, "%%%%Pages: %d\n", pages);
	fprintf(fp, "%%%%EOF\n");
	fclose(fp);
	return 0;
}

#define PS_MAGIC "%!PS"

/* ---
conservative file format guessing:
   1. extension .ps
   2. first line starts with "%!PS"
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp;
	char b[100];

	ext = strrchr(fn, '.');
	if (!ext) return 0;	/* no extension */
	if (MwStrcasecmp(ext, ".ps"))
		return 0;	/* wrong extension */
	if ((fp = fopen(fn, "r")) == NULL)
		return 0;	/* can't open */
	if (fgets(b, sizeof b, fp) && !strncmp(b, PS_MAGIC, strlen(PS_MAGIC))) {
		fclose(fp);
		return 1;
	}
	fclose(fp);
	return 0;
}

/* ---
*/
void fileio_ps_init(void)
{
	register_format(NULL, save, myformat, psformat = "Postscript (*.ps)");
}

