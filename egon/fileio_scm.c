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
 * fileio_scm.c
 */

/* ---
   Brief description of the file format:

   This is plain Scheme code which can be loaded by Egon Animator to
   recreate the animation sequence. It can also be inserted in an
   existing buffer to add new actors.
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

/* ---
Returns: 0 if successful, otherwise 1
*/

static int load(char *fn, buffer *buf)
{
	char cmd[1024];

	w_list->buf = buf;
	sprintf(cmd, "(load \"%s\")", fn);
	execute(cmd);
	return 0;
} /* load */

/* ---
file format guessing: never match
*/

static int myformat(char *fn)
{
	return 0;
}

/* ---
*/
void fileio_scm_init(void)
{
	register_format(load, NULL, myformat, "Scheme Code (*.scm)");
}

