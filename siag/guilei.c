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

#include <Mowitz/MwUtils.h>
#include "../common/common.h"

#include "calc.h"

#include <guile/gh.h>

static int siag_row, siag_col;
static cval siag_result;
static buffer *siag_buffer;
static int siag_sht;

static int guile_interpreter;

static SCM get_row(void)
{
	return gh_int2scm(siag_row);
}

static SCM get_col(void)
{
	return gh_int2scm(siag_col);
}

static SCM x_get_cell(SCM row, SCM col, SCM bname)
{
	int r, c;
	char *p;
	buffer *buf;
	int s;

	if (!gh_exact_p(row))
		scm_wta(row, "wta(1st) to get-cell", NULL);
	if (!gh_exact_p(col))
		scm_wta(col, "wta(2nd) to get-cell", NULL);
	r = (0.5 + gh_scm2double(row));
	c = (0.5 + gh_scm2double(col));
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS)
		return SCM_EOL;
	if (SCM_NULLP(bname)) {
		buf = siag_buffer;
		s = siag_sht;
	} else if (gh_string_p(bname)) {
		buf = find_sheet_by_name(SCM_CHARS(bname), siag_buffer, &s);
		if (buf == NULL) return SCM_EOL;
	}
	else return SCM_EOL;

	switch (ret_type(buf, s, r, c)) {
	case STRING:
		p = ret_string(buf, s, r, c);
		return scm_makfrom0str(p);
	case LABEL:
		p = ret_text(buf, s, r, c);
		return scm_makfrom0str(p);
	default:
		return gh_double2scm(ret_val(buf, s, r, c).number);
	}
}

static SCM get_cell(SCM row, SCM col)
{
	return x_get_cell(row, col, SCM_EOL);
}

static SCM x_get_string(SCM row, SCM col, SCM bname)
{
	int s, r, c;
	char *p;
	buffer *buf;

	if (!gh_exact_p(row))
		scm_wta(row, "wta(1st) to get-string", NULL);
	if (!gh_exact_p(col))
		scm_wta(col, "wta(2nd) to get-string", NULL);
	r = (0.5+gh_scm2double(row));
	c = (0.5+gh_scm2double(col));
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS)
		return SCM_EOL;
	if (SCM_NULLP(bname)) {
		buf = siag_buffer;
		s = siag_sht;
	} else if (gh_string_p(bname)) {
		buf = find_sheet_by_name(SCM_CHARS(bname), siag_buffer, &s);
		if (buf == NULL) return SCM_EOL;
	}
	else return SCM_EOL;

	if (ret_type(buf, s, r, c) == ERROR) p = "";
	else p = ret_pvalue(NULL, buf, s, r, c, -1);
	return scm_makfrom0str(p);
}

static SCM get_string(SCM row, SCM col)
{
	return x_get_string(row, col, SCM_EOL);
}

static SCM get_type(SCM bname, SCM row, SCM col)
{
	buffer *buf;
	int s;

	if (SCM_NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(SCM_CHARS(bname), w_list->buf, &s);
	}

	return gh_double2scm(ret_type(buf, s, gh_scm2double(row), gh_scm2double(col)));
}

static SCM get_text(SCM row, SCM col)
{
	char *text = ret_text(buffer_of_window(w_list), w_list->sht,
		gh_scm2double(row), gh_scm2double(col));
	if (!text) text = "";

	return scm_makfrom0str( text);
}

static SCM siag_sum(SCM start, SCM end)
{
	double sum;
	int r, c, startr, startc, endr, endc;
	if (!gh_exact_p(gh_car(start)))
		scm_wta(gh_car(start), "wta(1st) to sum", NULL);
	if (!gh_exact_p(gh_cdr(start)))
		scm_wta(gh_cdr(start), "wta(1st) to sum", NULL);
	if (!gh_exact_p(gh_car(end)))
		scm_wta(gh_car(end), "wta(2nd) to sum", NULL);
	if (!gh_exact_p(gh_cdr(end)))
		scm_wta(gh_cdr(end), "wta(2nd) to sum", NULL);
	startr = (0.5 + gh_scm2double(gh_car(start)));
	startc = (0.5 + gh_scm2double(gh_cdr(start)));
	endr = (0.5 + gh_scm2double(gh_car(end)));
	endc = (0.5 + gh_scm2double(gh_cdr(end)));
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
	return gh_double2scm(sum);
}

static SCM siag_time(void)
{
	double t = time(NULL);
	return gh_double2scm(t);
}

static SCM lexec_expr(SCM intp, SCM expr)
{
	exec_expr(name2interpreter(SCM_CHARS(intp)), SCM_CHARS(expr));
	return SCM_EOL;
}

static void siag_print(SCM p)
{
	if (gh_number_p(p)) {
		siag_type = EXPRESSION;
		siag_result.number = gh_scm2double(p);
	} else if (gh_string_p(p)) {
		siag_type = STRING;
		siag_result.text = SCM_CHARS(p);
	} else {
		siag_type = ERROR;
		siag_result.number = 0, errorflag = 1;
	}
}

/* Using Guile */

#define BREAKCHARS "() \t\r\n"
#define TEMPLATE "(get-cell %ld %ld)"
#define RANGE "'RANGE %ld %ld %ld %ld"

static char *expand_references(buffer *buf, char *orig)
{
	char *new;
	int rc = ref_counter(buf, orig);
	if (!rc) return orig;

	/* strlen("(get-cell 10000 10000)")-strlen("a1") = 20 */
	new = MwMalloc(strlen(orig)+20*rc+1);
	ref_expander(buf, orig, new, BREAKCHARS, TEMPLATE, RANGE);
	return new;
}

/* ---
move block between (r1,c1) and (r2,c2), direction (rd,cd)
*/

static char *update_Guile_references(buffer *buf, char *expr, int r1, int c1,
				int r2, int c2, int rd, int cd)
{
	char *p;
	int rc = ref_counter(buf, expr);

	if (!rc) return expr;

	/* rough upper bound on new length. A few bytes extra don't matter */
	/* strlen("r100000c100000")-strlen("r1c1") = 10 */
	p = MwMalloc(strlen(expr)+20*rc+1);
	rc = ref_updater(buf, expr, p, BREAKCHARS, r1, c1, r2, c2, rd, cd);
	if (rc) {
		expr = MwStrdup(p);
	}
	MwFree(p);
	return expr;
}

/* ---
*/
static SCM wrapper(void *data, SCM jmpbuf)
{
  char *scheme_code = (char *)data;
  char *expansion = expand_references(siag_buffer, scheme_code);
  SCM res = gh_eval_str(expansion);
  if (expansion != scheme_code) MwFree(expansion);
  return res;
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
This same function also does strings
*/

cval parse_guile_expr(buffer *b, char *expr, int s, int row, int col)
{
	SCM result;

	siag_row = row;
	siag_col = col;
	siag_buffer = b;
	siag_sht = s;
	/* the next two lines seem to contradict each other, but it is
	   necessary to set the type in case siod bails out before
	   we make it to siag_print(). */
	errorflag = 0;
	siag_type = ERROR;
	result = gh_catch(SCM_BOOL_T,
			(scm_catch_body_t)wrapper, expr,
			(scm_catch_handler_t)catcher, expr);
	siag_print(result);
	return siag_result;
}

/* ---
*/
void execute_guile(char *s)
{
	if (ok2print)
		hide_cur(w_list);
	siag_row = get_point(w_list).row;
	siag_col = get_point(w_list).col;
	siag_buffer = buffer_of_window(w_list);
	siag_sht = w_list->sht;
	gh_catch(SCM_BOOL_T,
			(scm_catch_body_t)wrapper, s,
			(scm_catch_handler_t)catcher, s);
	if (ok2print)
		show_cur(w_list);
}

/* ---
*/
int init_guile_parser(void)
{
	gh_new_procedure("row", get_row, 0, 0, 0);
	gh_new_procedure("col", get_col, 0, 0, 0);
	gh_new_procedure("get-cell", get_cell, 2, 0, 0);
	gh_new_procedure("get-string", get_string, 2, 0, 0);
	gh_new_procedure("get-type", get_type, 3, 0, 0);
	gh_new_procedure("get-text", get_text, 2, 0, 0);
	gh_new_procedure("sum", siag_sum, 2, 0, 0);
	gh_new_procedure("x-get-cell", x_get_cell, 3, 0, 0);
	gh_new_procedure("x-get-string", x_get_string, 3, 0, 0);
	gh_new_procedure("time", siag_time, 0, 0, 0);
	gh_new_procedure("exec-expr", lexec_expr, 2, 0, 0);

	return guile_interpreter = register_interpreter("Guile",
				parse_guile_expr, execute_guile,
				update_Guile_references);
}
#else
int init_guile_parser(void)
{
	return -1;
}
#endif	/* GUILE */

