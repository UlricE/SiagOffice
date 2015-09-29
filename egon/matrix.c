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
matrix.c

This module hides the details of stringpool and matrix handling.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>

#include "egon.h"

#include <Mowitz/MwUtils.h>
#include "../common/common.h"

static char *types[] = {
	"None", "Line", "Rectangle", "Arc", "Ellipse", "Pixmap",
	"String", "Point", "Filled Rectangle", "Filled Arc",
	"Filled Ellipse", NULL};

/* ---
*/
char *type2name(int type)
{
	return types[type];
}

/* ---
*/
int name2type(char *name)
{
	int i;
	for (i = 0; types[i]; i++) {
		if (!MwStrcasecmp(types[i], name)) return i;
	}
   	return -1;
}

/* ---
   void free_matrix(spread **matrix)
   Frees the memory used by the matrix.
*/

void free_matrix(MwAniObject *matrix)
{
	/* It would be nice if this actually did something */
}

/* ---
*/
MwAniObject *last_object(sheet *b)
{
	MwAniObject *o;
	if (!b) return NULL;
	o = b->cast;
	if (!o) return o;
	while (o->next) o = o->next;
	return o;
}

/* ---
*/
MwAniScript *last_script(MwAniObject *o)
{
	MwAniScript *s;
	if (!o) return NULL;
	s = o->script;
	if (!s) return s;
	while (s->next) s = s->next;
	return s;
}

/* ---
*/
int ins_format(buffer *buf, MwAniObject *o, MwAniScript *s, int format)
{
	if (!buf || !o || !s) return 0;

	o->fmt = format;
	return 1;
}

/* ---
*/
int ret_format(buffer *buf, MwAniObject *o, MwAniScript *s)
{
	if (!buf || !o || !s) return 0;

	return o->fmt;
}

