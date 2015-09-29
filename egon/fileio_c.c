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
fileio_c.c


   Brief description of the file format:

   This is an animation saved as C source code, so it can be compiled
   into a program. The program then only needs be linked with the
   animator module to get animation capabilities.

   None of the window or buffer information is saved, only the objects,
   scripts and properties.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

/* ---
Returns: 0 if successful, otherwise 1
*/

static int save(char *fn, buffer * buf)
{
	MwAniObject *o;
	MwAniScript *s;
	FILE *fp = fopen(fn, "w");
	char basename[256], *p;
	int i, j;

	if (!fp) return 1;

	fprintf(fp, "/* %s */\n", buf->name);
	fprintf(fp, "/* Creator: %s */\n", version);
	/* fix up a basename which can be used in C code */
	strcpy(basename, buf->name);
	p = basename;
	if (!isalpha(*p)) *p = '_';
	for (; *p; p++)
		if (!isalnum(*p)) *p = '_';

	fprintf(fp, "int %s_delta = %d;\n", basename, buf->sht[0].delta);
	fprintf(fp, "int %s_duration = %d;\n", basename, buf->sht[0].duration);
	fprintf(fp, "int %s_width = %d;\n", basename, buf->width);
	fprintf(fp, "int %s_height = %d;\n", basename, buf->height);
	if (buf->sht[0].bg)
		fprintf(fp, "char *%s_background = \"%s\";\n", basename, buf->sht[0].bg);
	else
		fprintf(fp, "char *%s_background = NULL;\n", basename);

	/* save the scripts for all the objects */
	i = 0;
	for (o = buf->sht[0].cast; o; o = o->next) {
		j = 0;
		fprintf(fp, "MwAniScript %s_script_%d[] = {\n", basename, i);
		for (s = o->script; s; s = s->next)  {
			if (s->next) {	/* not the last one */
				fprintf(fp, "\t{%d, %d, %d, %d, %d, %d, &%s_script_%d[%d]},\n",
					s->time, s->x, s->y, s->width, s->height,
					s->visible,
					basename, i, j+1);
			} else {	/* last tick */
				fprintf(fp, "\t{%d, %d, %d, %d, %d, %d, NULL}};\n",
					s->time, s->x, s->y, s->width, s->height,
					s->visible);
			}
			j++;
		}
		i++;
	}

	/* then save the objects */
	fprintf(fp, "MwAniObject %s_objects[] = {\n", basename);
	i = 0;
	for (o = buf->sht[0].cast; o; o = o->next) {
		fprintf(fp, "/* Object %s */\n", o->name);
		if (o->next) {	/* not the last one */
			fprintf(fp, "\t{%d, \"%s\", %s_script_%d, %d, \"%s\", &%s_objects[%d]},\n",
				o->type, o->name, basename, i,
				o->fmt, o->string?o->string:"",
				basename, i+1);
		} else {	/* last object */
			fprintf(fp, "\t{%d, \"%s\", %s_script_%d, %d, \"%s\", NULL}};\n",
				o->type, o->name, basename, i,
				o->fmt, o->string?o->string:"");
		}
		i++;
	}
	fclose(fp);
	return 0;
}				/* save */

#define C_MAGIC "/* "

/* ---
conservative file format guessing:
   1. extension .c
   2. Starts with a C-style comment, which I can't type here because
      clueless GCC will give me a warning even though it's in quotes
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".c") &&
		(fp = fopen(fn, "r")) &&
		fgets(b, sizeof b, fp) &&
		!strncmp(b, C_MAGIC, strlen(C_MAGIC)));
	if (fp) fclose(fp);
	return result;
}

/* ---
Register this file format.
*/

void fileio_c_init(void)
{
	register_format(NULL, save, myformat, "C source (*.c)");
}

