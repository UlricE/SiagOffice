/*
   Siag, Scheme In A Grid
   Copyright (C) 1996  Ulric Eriksson <ulric@siag.nu>

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
 * fileio_scm.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "calc.h"

static int load_scm(char *fn, buffer *buf)
{
	char cmd[1024];

	set_window_buffer(w_list, buf);
	sprintf(cmd, "(load \"%s\")", fn);
	execute(cmd);
	return 0;
}

/* ---
conservative file format guessing:
   Never match, it's too dangerous
*/

static int myformat(char *fn)
{
	return 0;
}

/* ---
*/
void fileio_scm_init(void)
{
	register_format(load_scm, NULL, myformat,
		"Scheme Code (*.scm)");
}

