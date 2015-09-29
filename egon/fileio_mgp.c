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
 * fileio_mgp.c
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

#define MGP_MAGIC "%"

/* ---
format guessing:
   1. extension .mgp
   2. Starts with "%"
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".mgp") &&
		(fp = fopen(fn, "r")) &&
		fgets(b, sizeof b, fp) &&
		!strncmp(b, MGP_MAGIC, strlen(MGP_MAGIC)));
	if (fp) fclose(fp);
	return result;
}

/* ---
*/
void fileio_mgp_init(void)
{
	register_format(NULL, NULL, myformat, "MagicPoint (*.mgp)");
}

