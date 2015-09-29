/*
   Egon Animator
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
 * fileio_ps.c
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <Mowitz/Mowitz.h>

#include "../egon/egon.h"
#include "../common/common.h"
#include "../xcommon/xcommon.h"
#include "xegon.h"

#define PS_MAGIC "%!PS"

char *psformat;

static int pageno;
static int ps_paper_width, ps_paper_height;

static void expand(FILE *fp, int x, int y, char *from, char *name, int number)
{
	char b[1024], c[1024];
	int i;
	time_t t;
	struct tm *lt;

	unsigned int txt_height, txt_width;

	MwPsSetFont(fp, 0);
	time(&t);
	lt = localtime(&t);
	strftime(c, sizeof c, from, lt);
	from = c;
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
	txt_height = MwFontHeight(0);
	txt_width = MwFontWidth(0, b);
	fprintf(fp, "newpath\n");
	MwPsSetColor(fp, 0, 0, 0);
	fprintf(fp, "%d %d moveto\n", x-txt_width/2, y-txt_height/2);
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

	if (!ps_page_header) ps_page_header = "&n";
	expand(fp, ps_paper_width/2,
		ps_paper_height-buf->header_margin,
		ps_page_header, buf->name, pageno);
}

static void page_footer(FILE *fp, buffer *buf)
{
	char *ps_page_footer = buf->footer;
	if (pageno == buf->first_page_number && buf->header_on_first == 0)
		return;

	if (!ps_page_footer) ps_page_footer = "&n";
	expand(fp, ps_paper_width/2,
		buf->footer_margin,
		ps_page_footer, buf->name, pageno);
}

static void print_page(FILE *fp, buffer *buf, int s)
{
	Display *display;
	Colormap colormap;
	Pixmap pm;
	Widget shell, w;
	XImage *ximage;
	XColor color;
	int width = buf->width, height = buf->height, depth = 8;
	int x, y, col;
	unsigned long pixel;
	shell = XtVaCreatePopupShell("shell",
		transientShellWidgetClass, topLevel,
		(char *)0);
	w = XtVaCreateManagedWidget("animator",
		mwAnimatorWidgetClass, shell,
		XtNheight, height,
		XtNwidth, width,
		XtNanimatorCast, buf->sht[s].cast,
		XtNanimatorNow, 0,
		XtNanimatorBgPixmap, buf->sht[s].bg,
		XtNgradient, buf->sht[s].bgrad,
		(char *)0);
	XtRealizeWidget(shell);
	pm = MwAnimatorPixmap(w, 0);
	display = XtDisplay(w);
	colormap = XDefaultColormapOfScreen(XtScreen(w));
	ximage = XGetImage(display, pm, 0, 0,
		width, height, ~0, ZPixmap);
	fprintf(fp, "gsave\n");
	if (buf->orientation == LANDSCAPE) {
		fprintf(fp, "90 rotate\n");
		fprintf(fp, "0 %d translate\n", -ps_paper_height);
	}
	page_header(fp, buf);
	page_footer(fp, buf);

	/* center the image */
	fprintf(fp, "%d %d translate\n",
		buf->left_margin+(ps_paper_width-width
			-buf->left_margin-buf->right_margin)/2,
		buf->bottom_margin+(ps_paper_height-height
			-buf->top_margin-buf->bottom_margin)/2);

	fprintf(fp, "/inch {72 mul} def\n");
	fprintf(fp, "%d %d scale\n", width, height);
	fprintf(fp, "/line %d string def\n", 3*width);
	fprintf(fp, "%d %d %d\n", width, height, depth);
	fprintf(fp, "[ %d %d %d %d %d %d ]\n", width, 0, 0, -height, 0, height);
	fprintf(fp, "{currentfile line readhexstring pop}\n");
	fprintf(fp, "false 3 colorimage\n");

	/* print all the pixels */
	col = 0;

	for (y = 0; y < height; y++) {
		for (x = 0; x < width; x++) {
			pixel = XGetPixel(ximage, x, y);
			color.pixel = pixel;
			MwQueryColor(display, None, &color);
			fprintf(fp, "%02hx%02hx%02hx",
				(color.red / 256) & 255,
				(color.green / 256) & 255,
				(color.blue / 256) & 255);
			col += 6;
			if (col > 72) {
				fprintf(fp, "\n");
				col = 0;
			}
		}
	}

	if (col) fprintf(fp, "\n");
	fprintf(fp, "%%\n");
	fprintf(fp, "showpage\n");
	fprintf(fp, "grestore\n");
	XDestroyImage(ximage);
	XFreePixmap(display, pm);
	XtDestroyWidget(shell);
}

static int save(char *fn, buffer *buf)
{
	FILE *fp = fopen(fn, "w");
	int pages;
	int s = 0;
	time_t t;
	if (!fp) return 1;
	fprintf(fp, "%%!PS-Adobe-2.0\n");
	fprintf(fp, "%%%%Creator: Egon Animator\n");
	fprintf(fp, "%%%%Title: egon\n");
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
	fprintf(fp, "/A {show} def\n");
	fprintf(fp, "%%%%EndProlog\n");

	pages = 0;
	pageno = buf->first_page_number;
	for (s = 0; s < buf->nsht; s++) {
		pages++;
		MwPsSetFont(fp, -1);	/* force setfont */
		fprintf(fp, "%%%%Page: %d %d\n", pages, pages);
		print_page(fp, buf, s);
		pageno++;
	}
	fclose(fp);
	return 0;
}

/* ---
format guessing:
   1. extension .ps
   2. Starts with "%!PS"
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".ps") &&
		(fp = fopen(fn, "r")) &&
		fgets(b, sizeof b, fp) &&
		!strncmp(b, PS_MAGIC, strlen(PS_MAGIC)));
	if (fp) fclose(fp);
	return result;
}

/* ---
*/
void fileio_ps_init(void)
{
	register_format(NULL, save, myformat, psformat = "Postscript (*.ps)");
}

