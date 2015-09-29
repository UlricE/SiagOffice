/*
   Egon Animator
   Copyright (C) 2000-2002  Ulric Eriksson <ulric@siag.nu>

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

#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

/* All objects of type string are saved. No attempt is made to preserve
the layout. Objects of other types are ignored. Form feed between sheets.
*/
static int save(char *fn, buffer *buf)
{
	FILE *fp = fopen(fn, "w");
	MwAniObject *o = NULL;
	int s;

	if (!fp) return 1;

	for (s = 0; s < buf->nsht; s++) {
		if (s) fputs("\f\n", fp);
		for (o = buf->sht[s].cast; o; o = o->next) {
			if (o->type == MW_ANI_STRING) {
				fprintf(fp, "%s\n", o->string);
			}
		}
	}
	fclose(fp);
	return 0;
}

static void fixup_sizes(MwAniObject *o, int w, int h, int y)
{
	MwAniObject *p;
	MwAniScript *s;
	float f, g;
	MwFmt fmt;
	int x;

	/* We know that the existing strings have size 140 decipoints.
	We also know that they are 20 pixels apart. Calculate a suitable
	scaling factor by dividing h (window height in pixels) with
	y (height we have used). */
	f = (h-20.0)/(float)y;	
	for (p = o; p; p = p->next) {
		x = 10+10*strlen(p->string);	/* let's be sloppy */
		g = (w-20.0)/(float)x;
		if (g < f) f = g;
	}
	for (p = o; p; p = p->next) {
		MwDecodeFormat(p->fmt, ~0, &fmt);
		fmt.size *= f;
		p->fmt = MwEncodeFormat(~0, &fmt);
		s = p->script;	/* we know that there is only one tick */
		s->y *= f;
	}
}

static int load(char *fn, buffer *buf)
{
	FILE *fp;
	MwAniObject *o = NULL;
	MwAniScript *s = NULL;
	char b[1024], name[100];
	int sht = 0;
	int y = 20;
	int n = 1;
	int f;
	MwFmt fmt;

	if ((fp = fopen(fn, "r")) == NULL) return 1;

	MwDecodeFormat(0, ~0, &fmt);	/* put valid fields in fmt */
	fmt.size = 140;
	fmt.fg = "white";
	f = MwEncodeFormat(~0, &fmt);

	buf->sht[sht].delta = 100;
	buf->sht[sht].duration = 4000;
	buf->sht[sht].now = 0;
	buf->width = 600;
	buf->height = 400;
	buf->sht[sht].cast = NULL;
	buf->state = MW_ANI_STOP;
	buf->sht[sht].bg = NULL;
	buf->change = FALSE;
	buf->sht[sht].plugin = NULL;
	buf->sht[sht].nplugin = 0;

	while (fgets(b, 1024, fp) != NULL) {
		if (b[0] == '\f') {
			fixup_sizes(buf->sht[sht].cast,
					buf->width, buf->height, y);
			sht++;
			if (sht >= buf->nsht) {
				buffer_add_sheet(buf, sht);
			}
			y = 20;
		} else {
			MwChomp(b);
			if (only_space(b)) {
				y += 20;
				continue;
			}
			if (buf->sht[sht].cast == NULL) {
				o = (MwAniObject *)MwMalloc(sizeof(MwAniObject));
				buf->sht[sht].cast = o;
			} else {
				o->next = (MwAniObject *)MwMalloc(sizeof(MwAniObject));
				o = o->next;
			}
			o->next = NULL;
			o->type = MW_ANI_STRING;
			o->fmt = f;
			sprintf(name, "String%d", n++);
			o->name = MwStrdup(name);
			o->string = MwStrdup(b);
			s = (MwAniScript *)MwMalloc(sizeof(MwAniScript));
			o->script = s;
			s->next = NULL;
			s->time = 0;
			s->x = 10;
			s->y = y;
			s->width = 500;
			s->height = 20;
			s->visible = TRUE;
			y += 20;
		}
	}
	fixup_sizes(buf->sht[sht].cast, buf->width, buf->height, y);
	fclose(fp);
	return 0;
}


/* ---
format guessing:
   1. extension .txt
   2. not empty
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".txt") &&
		(fp = fopen(fn, "r")) &&
		fgets(b, sizeof b, fp));
	if (fp) fclose(fp);
	return result;
}

/* ---
*/
void fileio_txt_init(void)
{
	register_format(load, save, myformat, "Text (*.txt)");
}

