/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2002  Ulric Eriksson <ulric@siag.nu>

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
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>


#include "../common/common.h"

#include "calc.h"

#include <tcl.h>

static Tcl_Interp *interp = NULL;

static int siag_row, siag_col;
static cval siag_result;
static buffer *siag_buffer;
static int siag_sht;
static double value;
static int tcl_interpreter;


static int guess_type(char *p)
{
	char *endp;
	char b[256];
	if (!strcmp(p, "")) return EMPTY;
	strncpy(b, p, 255);
	b[255] = '\0';
	value = strtod(b, &endp);
	if (only_space(endp)) return EXPRESSION;
	return STRING;
}

/* ---
This same function also does strings
*/

static cval parse_tcl_expr(buffer *b, char *expr, int s, int row, int col)
{
	cval nothing;
	char p[80];
	int result;
	nothing.number = 0;
	if (interp == NULL) {	/* not initialized */
		siag_type = ERROR;
		return nothing;
	}

	siag_row = row;
	siag_col = col;
	siag_buffer = b;
	siag_sht = s;
	sprintf(p, "%d", siag_row);
	Tcl_SetVar(interp, "R", p, 0);
	sprintf(p, "%d", siag_col);
	Tcl_SetVar(interp, "C", p, 0);

	result = Tcl_Eval(interp, expr);

	if (result != TCL_OK) {
		errorflag = 1;
		siag_type = ERROR;
		return nothing;
	}

	errorflag = 0;
	siag_type = guess_type(interp->result);
	if (siag_type == STRING) {
		siag_result.text = interp->result;
	} else {
		siag_result.number = value;
	}
	return siag_result;
}

static void exec_tcl(char *expr)
{
	char p[80];
	if (ok2print)
		hide_cur(w_list);
	siag_row = get_point(w_list).row;
	siag_col = get_point(w_list).col;
	siag_buffer = buffer_of_window(w_list);
	siag_sht = w_list->sht;
	sprintf(p, "%d", siag_row);
	Tcl_SetVar(interp, "R", p, 0);
	sprintf(p, "%d", siag_col);
	Tcl_SetVar(interp, "C", p, 0);
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

static int get_row(ClientData clientdata, Tcl_Interp *interp,
			Tcl_Value *args, Tcl_Value *resultPtr)
{
	resultPtr->intValue = siag_row;
	resultPtr->type = TCL_INT;
	return TCL_OK;
}

static int get_col(ClientData clientdata, Tcl_Interp *interp,
			Tcl_Value *args, Tcl_Value *resultPtr)
{
	resultPtr->intValue = siag_col;
	resultPtr->type = TCL_INT;
	return TCL_OK;
}

static int get_cell(ClientData clientdata, Tcl_Interp *interp,
			Tcl_Value *args, Tcl_Value *resultPtr)
{
	int r = args[0].intValue;
	int c = args[1].intValue;
	int type = ret_type(siag_buffer, siag_sht, r, c);

	if (IS_NUMBER(type))
		resultPtr->doubleValue = ret_val(siag_buffer,
						siag_sht, r, c).number;
		else resultPtr->doubleValue = 0;

	resultPtr->type = TCL_DOUBLE;
	return TCL_OK;
}

static int sum(ClientData clientdata, Tcl_Interp *interp,
			Tcl_Value *args, Tcl_Value *resultPtr)
{
	double sum;
	int r, c, startr, startc, endr, endc;
	startr = args[0].intValue;
	startc = args[1].intValue;
	endr = args[2].intValue;
	endc = args[3].intValue;
	if (startr > endr) {
		r = startr;
		startr = endr;
		endr = r;
	}
	if (startc > endc) {
		c = startc;
		startc = endc;
		endc = c;
	}
	sum = 0;
	for (r = startr; r <= endr; r++)
		for (c = startc; c <= endc; c++)
			sum += ret_val(siag_buffer, siag_sht, r, c).number;
	resultPtr->doubleValue = sum;
	resultPtr->type = TCL_DOUBLE;
	return TCL_OK;
}

/* ---
*/
int init_tcl_parser(Tcl_Interp *i)
{
	Tcl_ValueType types[4] = {TCL_INT, TCL_INT, TCL_INT, TCL_INT};
	interp = i;
	Tcl_CreateCommand(interp, "exec-expr", texec_expr, NULL, NULL);
	Tcl_CreateMathFunc(interp, "row", 0, types, get_row, NULL);
	Tcl_CreateMathFunc(interp, "col", 0, types, get_col, NULL);
	Tcl_CreateMathFunc(interp, "get_cell", 2, types, get_cell, NULL);
	Tcl_CreateMathFunc(interp, "sum", 4, types, sum, NULL);
	return tcl_interpreter = register_interpreter("Tcl",
				parse_tcl_expr, exec_tcl,
				NULL);
}
#else
int tcl_is_not_used;
#endif	/* TCL */

