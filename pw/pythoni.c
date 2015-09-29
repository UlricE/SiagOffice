/*
   Siag, Scheme In A Grid
   Copyright (C) 1999-2002  Ulric Eriksson <ulric@siag.nu>

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

#include "../config.h"
#ifdef HAVE_LIBPYTHON

/*
 * pythoni.c
 */

#include "Python.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>

#include "pw.h"

static int siag_row, siag_col;
static buffer *siag_buffer;
static int siag_sht;

static int python_interpreter;


static void exec_python(char *expr)
{

	if (ok2print)
		hide_cur(w_list);
	siag_row = get_point(w_list).row;
	siag_col = get_point(w_list).col;
	siag_buffer = w_list->buf;
	siag_sht = w_list->sht;
	PyRun_SimpleString(expr);
	if (ok2print)
		show_cur(w_list);
}


/* ---
*/
int init_python_parser(void)
{
	Py_Initialize();
	return python_interpreter = register_interpreter("Python",
			NULL, exec_python);
}
#else
int init_python_parser(void)
{
        return -1;
}
#endif	/* PYTHON */

