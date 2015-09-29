/*
   Siag, Scheme In A Grid
   Copyright (C) 2002  Ulric Eriksson <ulric@siag.nu>

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
#ifdef HAVE_LIBRUBY

/*
 * rubyi.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>

#include <Mowitz/MwUtils.h>
#include "../common/common.h"

#include "pw.h"

#include <ruby.h>

static int siag_row, siag_col;
static int siag_sht;

static int ruby_interpreter;

static VALUE get_row(VALUE obj)
{
	return INT2FIX(siag_row);
}

static VALUE get_col(VALUE obj)
{
	return INT2FIX(siag_col);
}

/* ---
*/
void execute_ruby(char *s)
{
	VALUE v;
	if (ok2print)
		hide_cur(w_list);
	siag_row = get_point(w_list).row;
	siag_col = get_point(w_list).col;
	siag_sht = w_list->sht;
	v = rb_eval_string(s);
	if (ok2print)
		show_cur(w_list);
}

/* ---
*/
int init_ruby_parser(void)
{
	ruby_init();

	rb_define_global_function("get_row", get_row, 0);
	rb_define_global_function("get_col", get_col, 0);

	return ruby_interpreter = register_interpreter("Ruby",
				NULL, execute_ruby);
}
#else
int init_ruby_parser(void)
{
	return -1;
}
#endif	/* RUBY */

