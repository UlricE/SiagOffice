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

/* ---
fileio_egon.c

981022 New font code
990422 Make backups

   Brief description of the file format:

   This is a completely new file format, similar to the one used in
   Siag and PW. It includes support for structured files and plugins.
   Images are stored together with the rest of the document.

   #comment			A comment; ignored
   .delta t			Time between frames in ms
   .duration t			Total playing time in ms
   .geometry w h		Visible width and height
   .background fn		Name of background image file
   .object type x y w h v c f s	Initial object appearance
   .tick time x y w h v		Object appearance at time

	x y = position
	w h = size
	v = visible (1 or 0)
	c = colour (numeric code)
	f = font (numeric code)
	s = text
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "egon.h"
#include "user_interface.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../common/plugin.h"

int make_backups = 1;

/* ---
returns the name of a component in a structured file. Caller must free
*/

static char *file_name(buffer *buf, char *n)
{
        char b[1024];
        plugin_basedir(b, buf->name);
        strcat(b, "/");
        strcat(b, n);
        return MwStrdup(b);
}

/* ---
use structured format to save images and plugins. This file saves the
flat part.

Returns: 0 if successful, otherwise 1
*/

static int save_flat(char *fn, buffer * buf)
{
	MwAniObject *o;
	MwAniScript *s;
	char *string;
	FILE *fp = fopen(fn, "w");
	char b[1024];
	char *used_fmt = MwCalloc(MwFormatCount+1, 1);
	int fmt;
	int sht;

	if (!fp) return 1;

	plugin_basedir(b, buf->name);

	fprintf(fp, "# Creator: %s\n", version);
	fprintf(fp, "# %s\n", buf->name);
	fprintf(fp, ".geometry %d %d\n", buf->width, buf->height);
	for (sht = 0; sht < buf->nsht; sht++) {
		fprintf(fp, ".sheet %d %s\n", sht, buf->sht[sht].name);
		fprintf(fp, ".delta %d\n", buf->sht[sht].delta);
		fprintf(fp, ".duration %d\n", buf->sht[sht].duration);
		if (buf->sht[sht].bg)
			fprintf(fp, ".background %s\n", buf->sht[sht].bg);
		for (o = buf->sht[sht].cast; o; o = o->next) {
			fprintf(fp, "# Object %s\n", o->name);
			/* strip off directory part of image files in
			   a structured file */
			if (o->type == MW_ANI_PIXMAP &&
					!strncmp(o->string, b, strlen(b)))
				string = strrchr(o->string, '/')+1;
			else
				string = o->string;
			fmt = o->fmt;
			if (!used_fmt[fmt]) {
				MwSaveFormats(fp, fmt);
				used_fmt[fmt] = 1;
			}
			fprintf(fp, ".object %d %s %d %s\n",
				o->type, o->name, o->fmt, string?string:"");
			for (s = o->script; s; s = s->next)
				fprintf(fp, ".tick %d %d %d %d %d %d\n",
					s->time, s->x, s->y,
					s->width, s->height, s->visible);
		}
	}
	fclose(fp);
	MwFree(used_fmt);
	return 0;
}				/* save */

/* ---
Load the flat part of a file.

Returns: 0 if successful, otherwise 1
*/

static int load_flat(char *fn, buffer *buf)
{
	FILE *fp;
	char instring[1024], name[256], string[1024];
	MwAniObject *o = NULL;
	MwAniScript *s = NULL;
	char b[1024];
	int *fmts = NULL;
	int nfmt = -1;
	int f;
	int sht = 0;

	if ((fp = fopen(fn, "r")) == NULL)
		return 1;

	plugin_basedir(b, buf->name);

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

	while (fgets(instring, 1024, fp) != NULL) {
		if (instring[0] == '#') {
			continue;
		} else if (!strncmp(instring, ".sheet ", 7)) {
			char name[1000];
			sscanf(instring, ".sheet %d %[^\n]", &sht, name);
			if (sht >= buf->nsht) {
				buffer_add_sheet(buf, sht);
			}
			buffer_rename_sheet(buf, sht, name);
		} else if (!strncmp(instring, ".delta ", 7)) {
			sscanf(instring, ".delta %d", &buf->sht[sht].delta);
		} else if (!strncmp(instring, ".duration ", 10)) {
			sscanf(instring, ".duration %d", &buf->sht[sht].duration);
		} else if (!strncmp(instring, ".geometry ", 10)) {
			sscanf(instring, ".geometry %d %d",
			&buf->width, &buf->height);
		} else if (!strncmp(instring, ".background ", 12)) {
			sscanf(instring, ".background %[^\n]", b);
			buf->sht[sht].bg = MwStrdup(b);
		} else if (!strncmp(instring, ".ft ", 4)) {
			int n;
			sscanf(instring, ".ft%d", &n);
			if (n > nfmt) {
				nfmt = n;
				fmts = MwRealloc(fmts,
						(nfmt+1)*sizeof *fmts);
			}
			fmts[n] = MwLoadFormats(fp);
		} else if (!strncmp(instring, ".object ", 8)) {
			if (buf->sht[sht].cast == NULL) {
				o = (MwAniObject *)MwMalloc(sizeof(MwAniObject));
				buf->sht[sht].cast = o;
			} else {
				o->next = (MwAniObject *)MwMalloc(sizeof(MwAniObject));
				o = o->next;
			}
			o->next = NULL;
			o->script = NULL;
			name[0] = string[0] = '\0';
			if (nfmt == -1) {	/* old */
				int color, font;
				int n = sscanf(instring,
					".object %d %s %d %d %[^\n]",
					&o->type, name, &color, &font, string);
				if (n == 4) {
					string[0] = '\0';
					n++;
				}
				if (n != 5) {
					fprintf(stderr, "Wrong arg count\n");
					fclose(fp);
					return 1;
				}
				f = MwFmtOldToNew(color<<MW_COLOR_SHIFT|font);
			} else {
				int n = sscanf(instring,
					".object %d %s %d %[^\n]",
					&o->type, name, &f, string);
				if (n != 4) {
					fprintf(stderr, "Wrong arg count\n");
					fclose(fp);
					return 1;
				}
				f = fmts[f];
			}
			o->fmt = f;
			o->name = MwStrdup(name);
			if (o->type == MW_ANI_PIXMAP && string[0] != '/') {
				sprintf(instring, "%s/%s", b, string);
				o->string = MwStrdup(instring);
			} else {
				o->string = MwStrdup(string);
			}
		} else if (!strncmp(instring, ".tick ", 6)) {
			if (o->script == NULL) {
				s = (MwAniScript *)MwMalloc(sizeof(MwAniScript));
				o->script = s;
			} else {
				s->next = (MwAniScript *)MwMalloc(sizeof(MwAniScript));
				s = s->next;
			}
			s->next = NULL;
			sscanf(instring,
				".tick %d %d %d %d %d %d",
				&s->time, &s->x, &s->y, &s->width, &s->height,
				&s->visible);
		}
	}

	fclose(fp);
	return 0;
} /* load_flat */

#define EGON_MAGIC "# Creator: Egon Animator"

/* ---
file format guessing:
   1. extension .egon
   2. Starts with "# Creator: Egon Animator"
*/

static int flatfile(char *fn)
{
        char b[100];
        FILE *fp = fopen(fn, "r");
        if (!fp) return 0;
        if (fgets(b, sizeof b, fp) &&
                        !strncmp(b, EGON_MAGIC, strlen(EGON_MAGIC))) {
                fclose(fp);
                return 1;
        }
        return 0;
}

static int save(char *fn, buffer *buf)
{
        char cmd[1024], bak[1024];
        char *dir, *b, *p;
        int i, result;
	MwAniObject *o = NULL;
	int sht;
	int structured = 0;

	if (make_backups) {
		/* make backup copy */
		sprintf(bak, "%s.BAK", fn);
		rename(fn, bak);
	}

	for (sht = 0; sht < buf->nsht; sht++) {
		for (o = buf->sht[sht].cast; o; o = o->next) {
			if (o->type == MW_ANI_PIXMAP) {
				structured = 1;
				break;
			}
		}
		if (buf->sht[sht].nplugin) {
			structured = 1;
			break;
		}
	}

        if (!structured) {
	        /* no images or plugins, save as usual */
                return save_flat(fn, buf);
        }

        /* save all the plugins */
        dir = file_name(buf, "");
        p = strrchr(dir, '/');
        if (p) *p = '\0';
        mkdir(dir, 0700);
	for (sht = 0; sht < buf->nsht; sht++) {
	        for (i = 0; i < buf->sht[sht].nplugin; i++) {
	                b = file_name(buf, buf->sht[sht].plugin[i].name);
	                plugin_save(buf->sht[sht].plugin[i].ph, b);
	                MwFree(b);
	        }
	}
        /* save the main file */
        b = file_name(buf, "INDEX.egon");
        result = save_flat(b, buf);
        MwFree(b);
        /* tar up the lot */
        sprintf(cmd, "(cd %s;tar cf - *)>%s", dir, fn);
        system(cmd);
        MwFree(dir);
        return result;
}

/* ---
old files don't have the MAGIC but we still want to load them.
So first try flat, then tar, then flat again.
*/

static int load(char *fn, buffer *buf)
{
        char cmd[1024], b[1024];
        char *p;
        int n;

        if (flatfile(fn) || !tryuntar(fn, "INDEX.egon"))
                return load_flat(fn, buf);

        p = plugin_basedir(b, buf->name);
        sprintf(cmd,
                "mkdir -p %s;"
                "cat %s|(cd %s;tar xf -)",
                p, fn, p);
        system(cmd);
        p = file_name(buf, "INDEX.egon");
        n = load_flat(p, buf);
        MwFree(p);
        return n;
}

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".egon") &&
		(fp = fopen(fn, "r")) &&
		fgets(b, sizeof b, fp) &&
		!strncmp(b, EGON_MAGIC, strlen(EGON_MAGIC)));
	if (fp) fclose(fp);
	return result || tryuntar(fn, "INDEX.egon");
}

/* ---
*/
void fileio_egon_init(void)
{
	register_format(load, save, myformat, "Egon Animator (*.egon)");
}

