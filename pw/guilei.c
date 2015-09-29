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
#ifdef HAVE_LIBGUILE

/*
 * guilei.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>

#include "pw.h"

#include <guile/gh.h>

static int siag_row, siag_col;

static int guile_interpreter;

static SCM get_row()
{
	return gh_int2scm(siag_row);
}

static SCM get_col()
{
	return gh_int2scm(siag_col);
}

static SCM lexec_expr(SCM intp, SCM expr)
{
	exec_expr(name2interpreter(SCM_CHARS(intp)), SCM_CHARS(expr));
	return SCM_EOL;
}

/* Using Guile */

static SCM wrapper(void *data, SCM jmpbuf)
{
	char *scheme_code = (char *)data;
	return gh_eval_str(scheme_code);
}

static SCM catcher(void *data, SCM tag, SCM throw_args)
{
	char b[256];

	strcpy(b, "ERROR: ");
	strncat(b, (char *)data, 200);
	llpr(b);
	return SCM_BOOL_F;
}

/* ---
*/
void execute_guile(char *cmd)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	if (ok2print)
		hide_cur(w_list);
	siag_row = st[s].point_pos.row;
	siag_col = st[s].point_pos.col;
	gh_catch(SCM_BOOL_T,
			(scm_catch_body_t)wrapper, cmd,
			(scm_catch_handler_t)catcher, cmd);
	if (ok2print)
		show_cur(w_list);
}

/* ---
*/
int init_guile_parser(void)
{
	gh_new_procedure0_0("row", get_row);
	gh_new_procedure0_0("col", get_col);
	gh_new_procedure2_0("exec-expr", lexec_expr);

	return guile_interpreter = register_interpreter("Guile",
				NULL, execute_guile);
}

#else
int init_guile_parser(void)
{
        return -1;
}
#endif	/* GUILE */

