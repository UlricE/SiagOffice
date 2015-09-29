/*
    Copyright (C) 1996-2001  Ulric Eriksson <ulric@siag.nu>

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
embed.c

The bogus, deprecated functions designed to allow embedding of
black-and white X11 bitmaps. Do not touch.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>

#include <Mowitz/Mowitz.h>
#include "embed.h"

typedef struct s_embed_object {
	char *filename;
	unsigned int width, height;
	Pixmap bitmap;
	struct s_embed_object *next;
} embed_object;

static Display *display;
static Screen *screen;
static Pixel fg, bg;
static GC gc;

static embed_object *eo_list;

static embed_object *tag2object(char *tag)
{
	embed_object *o;

	for (o = eo_list; o; o = o->next)
		if (!strcmp(tag, o->filename)) return o;
	return NULL;
}

int embed_init(Widget w)
{
	unsigned long valuemask = 0;
	XGCValues values;

	display = XtDisplay(w);
	screen = XtScreen(w);
	gc = XCreateGC(display, XtWindow(w), valuemask, &values);
	fg = BlackPixelOfScreen(screen);
	XSetForeground(display, gc, fg);
	bg = WhitePixelOfScreen(screen);
	XSetBackground(display, gc, bg);
	eo_list = NULL;

	return EMBED_OK;
}

/* 970909: Changed XReadBitmapFileData to XReadBitmapFile. Fixes X11R5. */
char *embed_load(char *filename)
{
	unsigned int width, height;
	int x_hot, y_hot;
	embed_object *o;
	int i;
	Pixmap bmp; /* for X11R5 */

	/* if the object is already loaded, unload and reload */
	embed_unload(filename);

	i = XReadBitmapFile(display, XDefaultRootWindow(display),
		filename, &width, &height, &bmp, &x_hot, &y_hot);
	if (i != BitmapSuccess) return NULL;

	o = MwMalloc(sizeof(embed_object));
	if (!o) return NULL;
	o->filename = MwStrdup(filename);	/* unique and recognizable */
	o->bitmap = XCreatePixmap(display, XDefaultRootWindow(display),
		width, height,
		DefaultDepth(display, DefaultScreen(display)));

	XCopyPlane(display, bmp, o->bitmap, gc, 0, 0, width, height, 0, 0, 1);
	XFreePixmap(display, bmp);
	o->width = width;
	o->height = height;
	o->next = eo_list;
	eo_list = o;

	return o->filename;
}

int embed_unload(char *tag)
{
	embed_object *o = tag2object(tag);
	embed_object *o1 = eo_list;

	if (!o) return EMBED_ERR;
	for (o1 = eo_list; o1; o1 = o1->next) {
		if (o1->next == o) {
			o1->next = o->next;
			XFreePixmap(display, o->bitmap);
			MwFree(o->filename);
			MwFree(o);
			return EMBED_OK;
		}
	}
	return EMBED_ERR;
}

int embed_open(char *tag)
{
	char cmd[1024], b[256];
	char *p = "bitmap %s";
	FILE *fp;
	embed_object *o = tag2object(tag);
	int found = 0;

	if (!o) return EMBED_ERR;

	fp = fopen(tag, "r");
	if (!fp) return EMBED_ERR;

	while (!found && fgets(b, 250, fp)) {
		if (!strncmp(b, "EmBeD", 5)) {
			found = 1;
			strcpy(b, b+5);
			p = strchr(b, '\n');
			if (p) *p = '\0';
		}
	}
	fclose(fp);
	if (!found) {
		fprintf(stderr, "Warning: no object info available\n");
		strcpy(b, "bitmap %s");
	}
	sprintf(cmd, b, tag);
	system(cmd);
	return EMBED_OK;
}

int embed_save(char *tag, char *cmd, Pixmap bitmap)
{
	int x, y;
	unsigned int width, height;
	unsigned int border_width;
	unsigned int depth;
	Window root;
	FILE *fp;

	XGetGeometry(display, bitmap, &root, &x, &y,
		&width, &height, &border_width, &depth);
	XWriteBitmapFile(display, tag, bitmap, width, height, 0, 0);
	fp = fopen(tag, "a");
	fprintf(fp, "/*\n");
	fprintf(fp, "EmBeD%s\n", cmd);
	fprintf(fp, "*/\n");
	fclose(fp);

	return EMBED_OK;
}

int embed_print(FILE *fp, char *tag, int x_base, int y_base)
{
	embed_object *o = tag2object(tag);
	if (!o) return EMBED_ERR;

	fprintf(stderr, "No printing yet\n");
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d %d moveto\n", x_base, y_base);
	fprintf(fp, "%d %d lineto\n", x_base+o->width, y_base);
	fprintf(fp, "%d %d lineto\n", x_base+o->width, y_base+o->height);
	fprintf(fp, "%d %d lineto\n", x_base, y_base+o->height);
	fprintf(fp, "closepath\n");
	fprintf(fp, "stroke\n");
	return EMBED_ERR;
}

int embed_size(char *tag, unsigned int *width, unsigned int *height)
{
	embed_object *o = tag2object(tag);
	if (!o) return EMBED_ERR;

	if (width) *width = o->width;
	if (height) *height = o->height;
	return EMBED_OK;
}

int embed_draw(Drawable d, int x, int y, char *tag)
{
	embed_object *o = tag2object(tag);

	if (!o) return EMBED_ERR;
	XCopyArea(display, o->bitmap, d, gc, 0, 0, o->width, o->height, x, y);
	return EMBED_OK;
}

