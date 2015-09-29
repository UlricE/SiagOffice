/*
   Siag, Scheme In A Grid
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
 * fileio_abs.c
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "calc.h"
#include <Mowitz/MwFormat.h>
#include "../common/common.h"

/*
Leading and trailing white space removed
Opening and closing quotes removed
Doubled quotes replaced with single
Replace ^ with **
Uppercase characters lowercased
All done in place, overwriting original string
*/
static void unmsify(char *p)
{
	char *q = p;

	/* remove leading white space, if any */
	while (*p && isspace(*p)) p++;

	/* remove opening quote */
	if (*p == '"') p++;

	while (*p) {
		if (*p == '"') {
			p++;
			if (*p != '"') break;	/* closing quote */
		}
		if (*p == '^') {
			*q++ = '*';
			*q = '*';
		} else if (isupper(*p)) {
			*q = tolower(*p);
		} else {
			*q = *p;
		}
		p++;
		q++;
	}
	*q = '\0';
}

static int abs_loader(FILE *fp, buffer *buf)
{
	char a[1024], b[1024];
	int s = 0, ns;
	int nr, nc;
	cval value;
	value.number = 0;

	while (fgets(b, sizeof b, fp)) {
		if (sscanf(b, "Cells(%d,%d).Formula=%[^\r\n]",
			   &nr, &nc, a) == 3) {
			unmsify(a);
			if (a[0] == '\'') {
				ins_data(buf, siod_interpreter, a+1,
					value, LABEL, s, nr, nc);
			} else if (a[0] == '=') {
				ins_data(buf, C_interpreter, a+1,
					value, EXPRESSION, s, nr, nc);
			}
		} else if (sscanf(b, "Worksheets(%d).Name = %[^\r\n]",
			   &ns, a) == 2) {
			unmsify(a);
			ns--;	/* start with 0 */
			if (ns >= buf->nsht)
				buffer_add_sheet(buf, ns);
			buffer_rename_sheet(buf, ns, a);
		} else if (sscanf(b, "Worksheets(%d).Activate", &ns) == 1) {
			s = ns-1;	/* start with 0 */
		}
	}
	return 0;
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int load_abs(char *fn, buffer *buf)
{
	FILE *fp;
	int n;

	fp = fopen(fn, "r");
	if (fp == NULL) return 1;

	n = abs_loader(fp, buf);

	fclose(fp);
	return n;
} /* load */


#define ABS_MAGIC "'====="

/* ---
conservative file format guessing
*/

static int myformat(char *fn)
{
	FILE *fp;
	char b[100];
	fp = fopen(fn, "r");
	if (fp && fgets(b, sizeof b, fp) &&
	    !strncmp(b, ABS_MAGIC, strlen(ABS_MAGIC))) {
		fclose(fp);
		return 1;
	}
	return 0;
}

/* ---
*/
void fileio_abs_init(void)
{
	register_format(load_abs, NULL, myformat,
		"ABScript (*.abs)");
}

