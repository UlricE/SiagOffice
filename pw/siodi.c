/*
   Pathetic Writer
   Copyright (C) 1997, 1998  Ulric Eriksson <ulric@siag.nu>

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
 * siodi.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "pw.h"
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
	;
}

static void siag_print(LISP p)
{
	;
}

/* ---
*/
void execute_siod(char *s)
{
	/* mostly stolen from repl_c_string in slib.c */
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
	siag_row = get_point(w_list).row;
	siag_col = get_point(w_list).col;
	retval = repl_driver(want_sigint, want_init, &h);
	want_init = 0;		/* only once... */

	if (ok2print)
		show_cur(w_list);
}

static LISP lget_point(void)
{
	return MAKE_POSITION(get_point(w_list));
}

static LISP lget_mark(void)
{
	return MAKE_POSITION(get_mark(w_list));
}

static LISP lget_blku(void)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	return MAKE_POSITION(st[s].blku);
}

static LISP lget_blkl(void)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	return MAKE_POSITION(st[s].blkl);
}

static LISP lset_point(LISP point)
{
	set_point(w_list, make_position(POSITION_ROW(point),
					POSITION_COL(point)));
	return NIL;
}

static LISP lset_mark(LISP mark)
{
	set_mark(w_list, make_position(POSITION_ROW(mark),
					POSITION_COL(mark)));
	return NIL;
}

static LISP line_last_used_fun(void)
{
	int r = line_last_used(w_list->buf, w_list->sht);
	return flocons(r);
}

static LISP col_last_used_fun(LISP row)
{
	int r = get_c_long(row);
	int c = col_last_used(w_list->buf, w_list->sht, r);
	return flocons(c);
}

static LISP max_lines_fun(void)
{
	return flocons(max_lines);
}

static LISP max_columns_fun(void)
{
	return flocons(max_columns);
}

static LISP set_pr_scr(void)
{
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
void init_position(void)
{
	init_subr_0("get-point", lget_point);
	init_subr_0("get-mark", lget_mark);
	init_subr_0("get-blku", lget_blku);
	init_subr_0("get-blkl", lget_blkl);
	init_subr_1("set-point", lset_point);
	init_subr_1("set-mark", lset_mark);
	init_subr_0("line-last-used", line_last_used_fun);
	init_subr_1("col-last-used", col_last_used_fun);
	init_subr_0("max-lines", max_lines_fun);
	init_subr_0("max-columns", max_columns_fun);
	init_subr_0("set-pr-scr", set_pr_scr);
}

/* dummy args for SIOD */
static char *siod_argv[] = {
  "siod",
  "-h20000:10",	/* 100000:10 */
  "-g0",
  "-o1000",
  "-s100000",	/* 200000 */
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

	/*XtAppAddActions(app_context, actions, XtNumber(actions));*/
	return register_interpreter("SIOD", NULL, execute_siod);
}

