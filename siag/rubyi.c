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

#include "calc.h"

#include <ruby.h>

static int siag_row, siag_col;
static cval siag_result;
static buffer *siag_buffer;
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

static VALUE x_get_cell(VALUE obj, VALUE row, VALUE col, VALUE bname)
{
	int r, c;
	char *p;
	buffer *buf;
	int n, s;

	if (!FIXNUM_P(row)) {
		fprintf(stderr, "wta(1st) to get-cell\n");
		return Qnil;
	}
	if (!FIXNUM_P(col)) {
		fprintf(stderr, "wta(2nd) to get-cell\n");
		return Qnil;
	}
	r = (0.5 + rb_num2dbl(row));
	c = (0.5 + rb_num2dbl(col));
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS)
		return Qnil;
	if (NIL_P(bname)) {
		buf = siag_buffer;
		s = siag_sht;
	} else if (TYPE(bname) == T_STRING) {
		buf = find_sheet_by_name(rb_str2cstr(bname, &n), siag_buffer, &s);
		if (buf == NULL) return Qnil;
	} else {
		return Qnil;
	}

	switch (ret_type(buf, s, r, c)) {
	case STRING:
		p = ret_string(buf, s, r, c);
		return rb_str_new2(p);
	case LABEL:
		p = ret_text(buf, s, r, c);
		return rb_str_new2(p);
	default:
		return INT2NUM(ret_val(buf, s, r, c).number);
	}
}

static VALUE get_cell(VALUE obj, VALUE row, VALUE col)
{
	return x_get_cell(Qnil, row, col, Qnil);
}

static VALUE x_get_string(VALUE obj, VALUE row, VALUE col, VALUE bname)
{
	int n, s, r, c;
	char *p;
	buffer *buf;

	if (!FIXNUM_P(row)) {
		fprintf(stderr, "wta(1st) to get-string\n");
		return Qnil;
	}
	if (!FIXNUM_P(col)) {
		fprintf(stderr, "wta(2nd) to get-string\n");
		return Qnil;
	}
	r = (0.5+rb_num2dbl(row));
	c = (0.5+rb_num2dbl(col));
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS)
		return Qnil;
	if (NIL_P(bname)) {
		buf = siag_buffer;
		s = siag_sht;
	} else if (TYPE(bname) == T_STRING) {
		buf = find_sheet_by_name(rb_str2cstr(bname, &n), siag_buffer, &s);
		if (buf == NULL) return Qnil;
	}
	else return Qnil;

	if (ret_type(buf, s, r, c) == ERROR) p = "";
	else p = ret_pvalue(NULL, buf, s, r, c, -1);
	return rb_str_new2(p);
}

static VALUE get_string(VALUE obj, VALUE row, VALUE col)
{
	return x_get_string(Qnil, row, col, Qnil);
}

#if 0
static VALUE get_type(VALUE bname, VALUE row, VALUE col)
{
	buffer *buf;
	int s;

	if (NIL_P(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(rb_str2cstr(bname), w_list->buf, &s);
	}

	return gh_double2scm(ret_type(buf, s, rb_num2dbl(row), rb_num2dbl(col)));
}

static VALUE get_text(VALUE row, VALUE col)
{
	char *text = ret_text(buffer_of_window(w_list), w_list->sht,
		NUM2INT(row), NUM2INT(col));
	if (!text) text = "";

	return rb_str_new2(text);
}

static VALUE siag_time(void)
{
	double t = time(NULL);
	return INT2NUM(t);
}

static VALUE lexec_expr(VALUE intp, VALUE expr)
{
	exec_expr(name2interpreter(rb_str2cstr(intp)), rb_str2cstr(expr));
	return Qnil;
}
#endif

static void siag_print(VALUE v)
{
	int n;
	switch (TYPE(v)) {
	case T_FIXNUM:
	case T_FLOAT:
		siag_type = EXPRESSION;
		siag_result.number = rb_num2dbl(v);
		break;
	case T_TRUE:
		siag_type = EXPRESSION;
		siag_result.number = 1;
		break;
	case T_FALSE:
		siag_type = EXPRESSION;
		siag_result.number = 0;
		break;
	case T_STRING:
		siag_type = STRING;
		siag_result.text = rb_str2cstr(v, &n);
		break;
	default:
		siag_type = ERROR;
		siag_result.number = 0, errorflag = 1;
	}
}

#if 0
/* ---
*/
static VALUE wrapper(void *data, VALUE jmpbuf)
{
  char *scheme_code = (char *)data;
  char *expansion = expand_references(siag_buffer, scheme_code);
  VALUE res = gh_eval_str(expansion);
  if (expansion != scheme_code) MwFree(expansion);
  return res;
}

static VALUE catcher(void *data, VALUE tag, VALUE throw_args)
{
	char b[256];

	strcpy(b, "ERROR: ");
	strncat(b, (char *)data, 200);
	llpr(b);
	return VALUE_BOOL_F;
}
#endif

/* ---
This same function also does strings
*/

cval parse_ruby_expr(buffer *b, char *expr, int s, int row, int col)
{
	VALUE v;

	siag_row = row;
	siag_col = col;
	siag_buffer = b;
	siag_sht = s;
	errorflag = 0;
	siag_type = ERROR;
	v = rb_eval_string(expr);
	siag_print(v);
	return siag_result;
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
	siag_buffer = buffer_of_window(w_list);
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
	rb_define_global_function("x_get_cell", x_get_cell, 3);
	rb_define_global_function("get_cell", get_cell, 2);
	rb_define_global_function("x_get_string", x_get_string, 3);
	rb_define_global_function("get_string", get_string, 2);

	return ruby_interpreter = register_interpreter("Ruby",
				parse_ruby_expr, execute_ruby,
				NULL);
}
#else
int init_ruby_parser(void)
{
	return -1;
}
#endif	/* RUBY */

