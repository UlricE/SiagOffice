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

/* ---
   fileio_ps.c
 
   Produces data suitable for printing on a Postscript device. I have made
   several assumptions to simplify things:
   - PS font gets geometry from AFM file
   - no background pattern
   - The lpr command is used for printing

--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include "../pw/pw.h"
#include "../common/common.h"
#include <Mowitz/Mowitz.h>
#include "../common/plugin.h"

extern int font_size[8];

char *psformat;

static int pageno;
static int ps_paper_width, ps_paper_height;

/* ---
Sizing the output won't be a problem here, except we don't print
all the allocated but unused lines
*/

static int page_height(buffer *buf, int s, int r)
{
	int y_base, i;

	y_base = ps_paper_height - buf->top_margin;
	for (i = r; y_base > buf->bottom_margin; i++)
		y_base -= cell_height(buf, s, i);
	return (i-r);
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
        fprintf(fp, "NP\n");
	MwPsSetColor(fp, 0, 0, 0);
        fprintf(fp, "%d %d M\n", x - text_width/2, y - text_height/2);

        /* finally print the string */
        fprintf(fp, "(");
        for (i = 0; b[i]; i++) {
                int c = b[i] & 0xFF;
                if (isalnum(c))
                        putc(c, fp);
                else
                        fprintf(fp, "\\%03o", c);
        }
        fprintf(fp, ")A\n");
}

static void page_header(FILE *fp, buffer *buf)
{
	char *ps_page_header = buf->header;
	if (pageno == buf->first_page_number && buf->header_on_first == 0)
		return;

	if (!ps_page_header) ps_page_header = "&n";	/* buffer name */
	expand(fp, ps_paper_width/2,
		ps_paper_height-buf->header_margin,
                ps_page_header,
		buf->name, pageno);
}

static void page_footer(FILE *fp, buffer *buf)
{
        char *ps_page_footer = buf->footer;
	if (pageno == buf->first_page_number && buf->header_on_first == 0)
		return;

        if (!ps_page_footer) ps_page_footer = "&p";     /* page number */
        expand(fp, ps_paper_width/2,
		buf->footer_margin,
                ps_page_footer,
		buf->name, pageno);
}

static void print_segment(FILE *fp, float *x_base, int y_base, int y_pos,
		MwRichchar *line, int length,
		int extra_space, int no_of_blanks, int tabmode, int row)
{
	int i;
	float tw;
	MwRichchar c;

	tw = MwRcStrwidth(line, length);
	switch (tabmode) {
	case 'r':
		*x_base -= tw;
		break;
	case 'c':
		*x_base -= tw/2;
		break;
	default:	/* left or full */
		break;
	}
	fprintf(fp, "NP\n");
	fprintf(fp, "%.2f %d M\n", *x_base, y_base+y_pos);

	for (i = 0; i < length; i++) {
		float width;
		int fmt;
		MwFmt ft;

		c = line[i];
		if (isspace(c.c)) c.c = ' ';

		fmt = line[i].fmt;
		MwPsSetFont(fp, fmt);
		MwDecodeFormat(fmt, ~0, &ft);
		if (ft.vadj == MW_VADJ_TOP) {
			fprintf(fp, "%d %d R\n", 0, 6);
		} else if (ft.vadj == MW_VADJ_BOTTOM) {
			fprintf(fp, "%d %d R\n", 0, -6);
		}
		if (ft.uline) {
			fprintf(fp, "C ");
		}
		if (ft.strike) {
			fprintf(fp, "C ");
		}

	/* print letters and digits as is, the rest in octal */
		if (isalnum(c.c)) {
			fprintf(fp, "(%c)A", c.c);
		} else {
			fprintf(fp, "(\\%03o)A", c.c);
		}

		if (ft.strike) {
			fprintf(fp, " S");
		}
		if (ft.uline) {
			fprintf(fp, " U");
		}
		if (ft.vadj == MW_VADJ_TOP) {
			fprintf(fp, " %d %d R", 0, -6);
		} else if (ft.vadj == MW_VADJ_BOTTOM) {
			fprintf(fp, " %d %d R", 0, 6);
		}
		fprintf(fp, "\n");

		if (c.c == ' ' && extra_space > 0 && no_of_blanks > 0) {
			float x = extra_space/no_of_blanks;
			*x_base += x;
			extra_space -= x;
			no_of_blanks--;
			fprintf(fp, "%.2f %d M\n", *x_base, y_base+y_pos);
		}
		width = MwRcWidth(c);

		*x_base += width;
	}
}

static void print_line(FILE *fp, buffer *buf, int y_base, int s, int row)
{
	int lm = buf->left_margin;
	int rm = buf->right_margin;
	int pw = buf->paper_width;
	float tw;
	int nb;
	float x_base = 0;
	int y_pos = 5;
	MwRichchar *line;
	MwRichchar c;
	int ss, nt, hadj, i;
	MwTabstop mt, *tt = MwGetTabs(buf->sht[s].tabs);

	line = buf->sht[s].text[row].p;

	if (MwRcStrlen(line) == 0) return;
	x_base = lm;

	if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
		char *p = (char *)MwRcMakeplain(line);
		ps_embed_print(fp, p, x_base, y_base+y_pos);
		MwFree(p);
		return;	/* don't print the text */
	}

	hadj = ret_hadj(buf, s, row);
	if (hadj == MW_HADJ_CENTER) {
		x_base = lm+(pw-lm-rm)/2;
		print_segment(fp, &x_base, y_base, y_pos,
			line, MwRcStrlen(line), 0, 0, 'c', row);
	} else if (hadj == MW_HADJ_RIGHT) {
		x_base = pw-rm;
		print_segment(fp, &x_base, y_base, y_pos,
			line, MwRcStrlen(line), 0, 0, 'r', row);
	} else {
		mt.x = 0;
		mt.j = 'l';
		ss = nt = 0;
		while (line[nt].c && line[nt].c != '\t') nt++;
		while (line[nt].c == '\t') {
			print_segment(fp, &x_base, y_base, y_pos,
				line+ss, nt-ss/*MwRcStrlen(line)*/, 0, 0, mt.j, row);
			mt = MwNextTab(tt, x_base-lm);
			x_base = mt.x+lm;
			ss = ++nt;
			while (line[nt].c && line[nt].c != '\t') nt++;
		}
		tw = 0;
		nb = 0;
		if (hadj == MW_HADJ_FULL && !ret_bop(buf, s, row+1)) {
			for (i = 0; line[i].c; i++) {
				c = line[i];
				if (isspace(c.c)) nb++;
				tw += MwRcWidth(c);
			}
			tw = pw-lm-rm-tw;
		}

		print_segment(fp, &x_base, y_base, y_pos,
			line+ss, nt-ss, tw, nb, mt.j, row);
	}
	MwFree(tt);
}

static int print_page(FILE *fp, buffer *buf, int s, int fromr, int tor)
{
	int i, y_base;

	fprintf(fp, "GS\n");
	if (buf->orientation == LANDSCAPE) {
		fprintf(fp, "90 rotate\n");
		fprintf(fp, "0 %d TR\n", -ps_paper_height);
	}

	page_header(fp, buf);
	page_footer(fp, buf);

	y_base = ps_paper_height - buf->top_margin;
	for (i = fromr;
	     i <= buf->sht[s].used_lines && y_base > buf->bottom_margin;
	     i++) {
		y_base -= cell_height(buf, s, i);
		print_line(fp, buf, y_base, s, i);
	}

	/* print plugins here! */
	for (i = 0; i < buf->sht[s].nplugin; i++) {
		int x, y, topx, topy, boty, plx, ply;
		int width, height;
		buffer_global_coords(buf, s, fromr, 0, &topx, &topy);
		boty = topy+ps_paper_height-buf->top_margin-buf->bottom_margin;
		buffer_global_coords(buf, s, buf->sht[s].plugin[i].row,
                        buf->sht[s].plugin[i].col, &plx, &ply);
                plugin_size_get(buf->sht[s].plugin[i].ph, &width, &height);
                if (ply+height < topy || ply > boty) continue;
                x = buf->left_margin;
		switch (ret_hadj(buf, s, buf->sht[s].plugin[i].row)) {
		case MW_HADJ_CENTER:
			x += (ps_paper_width-
				buf->left_margin-buf->right_margin-width)/2;
			break;
		case MW_HADJ_RIGHT:
			x += (ps_paper_width-
				buf->left_margin-buf->right_margin-width);
			break;
		default:
			break;
		}
                y = ps_paper_height-buf->top_margin-(ply-topy)-height;
                /* and let the plugin do its thing */
                plugin_print(buf->sht[s].plugin[i].ph, fp, x, y, 1);
        }

	fprintf(fp, "GR\n");
	fprintf(fp, "showpage\n");
	return 0;
}

static int save(char *fn, buffer *buf)
{
	FILE *fp;
	int r;
	int pages;
	time_t t;
	int s;
	int fromr, tor;

	fp = fopen(fn, "w");

	fprintf(fp, "%%!PS-Adobe-2.0\n");
	fprintf(fp, "%%%%Creator: %s\n", version);
	fprintf(fp, "%%%%Title: %s\n", buf->name);
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

	fprintf(fp,
		"/A {show} def\n"
		"/M {moveto} def\n"
		"/R {rmoveto} def\n"
		"/C {currentpoint} def\n"
		"/GS {gsave} def\n"
		"/TR {translate} def\n"
		"/NP {newpath} def\n"
		"/LT {lineto} def\n"
		"/ST {stroke} def\n"
		"/GR {grestore} def\n"
		"/S {GS C 0 4 TR NP M LT ST GR} def\n" /* strikethrough */
		"/U {GS C 0 -2 TR NP M LT ST GR} def\n"); /* underline */

	fprintf(fp, "%%%%EndProlog\n");

	pages = 0;
	pageno = buf->first_page_number;
	for (s = 0; s < buf->nsht; s++) {
		fromr = 1;
		tor = line_last_used(buf, s);
		for (r = fromr; r <= tor; r += page_height(buf, s, r)) {
			pages++;
			MwPsSetFont(fp, -1); /* force setfont on top of page */
			fprintf(fp, "%%%%Page: %d %d\n", pages, pages);
			print_page(fp, buf, s, r, tor);
			pageno++;
		}
	}
	fprintf(fp, "%%%%Trailer:\n");
	fprintf(fp, "%%%%Pages: %d\n", pages);
	fprintf(fp, "%%%%EOF\n");
	fclose(fp);
	return 0;
}

/* ---
Conservative file format guessing: always negative (can't load)
*/

static int myformat(char *fn)
{
	return 0;
}

/* ---
*/
void fileio_ps_init(void)
{
	register_format(NULL, save, myformat, psformat = "Postscript (*.ps)");
}

