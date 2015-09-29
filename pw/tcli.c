/*
   Pathetic Writer
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

#include "../config.h"
#ifdef HAVE_LIBTCL

/*
 * tcli.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>

#include "pw.h"

#include <tcl.h>

static Tcl_Interp *interp = NULL;

static int tcl_interpreter;

static void exec_tcl(char *expr)
{
	if (ok2print)
		hide_cur(w_list);
	Tcl_Eval(interp, expr);
	if (ok2print)
		show_cur(w_list);
}

static int texec_expr(ClientData clientData, Tcl_Interp *interp,
			int argc, char *argv[])
{
	if (argc < 3) return TCL_ERROR;
	exec_expr(name2interpreter(argv[1]), argv[2]);
	Tcl_SetResult(interp, NULL, TCL_STATIC);
	return TCL_OK;
}

/* ---
*/
int init_tcl_parser(Tcl_Interp *i)
{
	interp = i;
	Tcl_CreateCommand(interp, "exec-expr", texec_expr, NULL, NULL);
	return tcl_interpreter = register_interpreter("Tcl",
				NULL, exec_tcl);
}

#else
int tcl_dummy;
#endif	/* TCL */

