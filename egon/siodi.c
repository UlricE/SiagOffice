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
 * railway.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "egon.h"
#include "../siod/siod.h"
#include "../common/common.h"

extern void init_ss(void);
extern void init_ndbm(void);
extern void init_regex(void);
extern void init_tar(void);

/* from slib.c */
extern char *repl_c_string_arg;
extern long repl_c_string_flag;
extern LISP repl_c_string_read(void);

static int siag_row, siag_col;

static LISP get_row(void)
{
	double drow = siag_row;
	return flocons(drow);
}

static LISP get_col(void)
{
	double dcol = siag_col;
	return flocons(dcol);
}

static LISP lexec_expr(LISP intp, LISP expr)
{
	exec_expr(name2interpreter(get_c_string(intp)), get_c_string(expr));
	return NIL;
}

static void siag_puts(char *p)
{
}

static void siag_print(LISP p)
{
}

/* ---
*/
void execute_siod(char *s)
{
	struct repl_hooks h;
	long retval;
	int want_sigint = 0;
	static int want_init = 1;
	if (ok2print)
		hide_cur(w_list);

	h.repl_puts = siag_puts;
	h.repl_read = repl_c_string_read;
	h.repl_eval = NULL;
	h.repl_print = siag_print;
	repl_c_string_arg = s;	/* let's hope repl_driver */
	repl_c_string_flag = 0;	/* is non-destructive */
	siag_row = 1;
	siag_col = 1;
	if (strcmp(s, "(no-op)"))
		retval = repl_driver(want_sigint, want_init, &h);
	want_init = 0;		/* only once... */

	if (ok2print)
		show_cur(w_list);
}

static LISP set_pr_scr(void)
{
	pr_scr_flag = TRUE;
	return NIL;
}


/* ---
*/
void init_position()
{
	init_subr_0("set-pr-scr", set_pr_scr);
}

/* dummy args for SIOD */
static char *siod_argv[] = {
  "siod",
  "-h20000:10",	/* 100000:10 */
  "-g0",
  "-o1000",
  "-s200000",	/* 200000 */
  "-n2048"};
static int siod_argc = sizeof siod_argv / sizeof siod_argv[0];

/* ---
*/
int init_parser(int argc, char **argv)
{
	print_welcome();
	process_cla(siod_argc, siod_argv, 1);
	process_cla(argc, argv, 1);
	print_hs_1();
	init_storage();
	init_subrs();
	init_trace();
	init_slibu();
	init_ss();
	init_ndbm();
	init_regex();
	init_tar();
	init_subr_0("row", get_row);
	init_subr_0("col", get_col);
	init_subr_2("exec-expr", lexec_expr);

	return register_interpreter("SIOD", NULL, execute_siod);
}

