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

/*
 * siodi.c
 */

#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <Mowitz/MwUtils.h>
#include "../common/common.h"

#include "calc.h"
#include "../siod/siod.h"

/* various siod modules */
extern void init_ss(void);
extern void init_ndbm(void);
extern void init_regex(void);
extern void init_tar(void);

/* from slib.c */
extern char *repl_c_string_arg;
extern long repl_c_string_flag;
extern LISP repl_c_string_read(void);

static int siag_sht;
static int siag_row, siag_col;
static cval siag_result;
static buffer *siag_buffer;
/*
;@siag_rowsum(r1, r2)
;@Returns the sum of all cells in the current column from row r1 to r2.
;@
;@siag_colsum
*/
static LISP lsiag_rowsum(LISP c1, LISP c2)
{
	double sum = 0;
	int i;
	long tmp = get_c_long(c2);
	for (i = get_c_long(c1); i <= tmp; i++)
		sum += ret_val(siag_buffer, siag_sht, i, siag_col).number;
	return flocons(sum);
}

/*
;@siag_colsum(c1, c2)
;@Returns the sum of all cells on the current row from column c1 to c2.
;@
;@siag_rowsum
*/
static LISP lsiag_colsum(LISP c1, LISP c2)
{
	double sum = 0;
	int i;
	long tmp = get_c_long(c2);
	for (i = get_c_long(c1); i <= tmp; i++)
		sum += ret_val(siag_buffer, siag_sht, siag_row, i).number;
	return flocons(sum);
}

static LISP lput_property(LISP bname, LISP key, LISP value)
{
	buffer *b;
	char *retval;

	if (NULLP(bname)) b = buffer_of_window(w_list);
	else b = find_buffer_by_name(get_c_string(bname));
	if (!b) {
		llpr("No such buffer");
		return NIL;
	}

	retval = put_property(b, get_c_string(key), get_c_string(value));
	if (retval) return strcons(strlen(retval), retval);
	return NIL;
}

static LISP lget_property(LISP bname, LISP key)
{
	buffer *b;
	char *retval;

	if (NULLP(bname)) b = buffer_of_window(w_list);
	else b = find_buffer_by_name(get_c_string(bname));
	if (!b) {
		llpr("No such buffer");
		return NIL;
	}

	retval = get_property(b, get_c_string(key));

	if (retval) return strcons(strlen(retval), retval);
	return NIL;
}

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

static LISP x_get_cell(LISP row, LISP col, LISP bname)
{
	int r, c;
	char *p;
	buffer *buf;
	int s;

	r = get_c_long(row);
	c = get_c_long(col);
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS)
		return NIL;
	if (NULLP(bname)) {
		buf = siag_buffer;
		s = siag_sht;
	} else if (TYPEP(bname, tc_string)) {
		buf = find_sheet_by_name(bname->storage_as.string.data,
					siag_buffer, &s);
		if (buf == NULL) return NIL;
	}
	else return NIL;

	switch (ret_type(buf, s, r, c)) {
	case STRING:
		p = ret_string(buf, s, r, c);
		return strcons(strlen(p), p);
	case LABEL:
		p = ret_text(buf, s, r, c);
		return strcons(strlen(p), p);
	case EMPTY:
	case ERROR:
		return NIL;
	default:
		return flocons(ret_val(buf, s, r, c).number);
	}
}

static LISP get_cell(LISP row, LISP col)
{
	return x_get_cell(row, col, NIL);
}

static double ret_number(buffer *buf, int s, int r, int c)
{
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS) return 0.0;
	switch (ret_type(buf, s, r, c)) {
	case STRING:
	case LABEL:
	case EMPTY:
	case ERROR:
		return 0;
	default:
		return ret_val(buf, s, r, c).number;
	}
}

static LISP get_number(LISP row, LISP col)
{
	return flocons(ret_number(siag_buffer, siag_sht,
			get_c_long(row), get_c_long(col)));
}

static LISP h_avg(LISP rows)
{
	int nr = get_c_long(rows);
	int i;
	int first, last;
	double sum = 0.0;
	if (nr < 0) {
		first = siag_col+nr;
		last = siag_col-1;
	} else {
		first = siag_col+1;
		last = siag_col+nr;
	}
	for (i = first; i <= last; i++) {
		sum += ret_number(siag_buffer, siag_sht, siag_row, i);
	}
	return flocons(sum/(last-first+1));
}

static LISP x_get_string(LISP row, LISP col, LISP bname)
{
	int r, c;
	int s;
	char *p;
	buffer *buf;

	r = get_c_long(row);
	c = get_c_long(col);
	if (r < 1 || r > BUFFER_ROWS || c < 1 || c > BUFFER_COLS)
		return NIL;
	if (NULLP(bname)) {
		buf = siag_buffer;
		s = siag_sht;
	} else if (TYPEP(bname, tc_string)) {
		buf = find_sheet_by_name(bname->storage_as.string.data,
					siag_buffer, &s);
		if (buf == NULL) return NIL;
	}
	else return NIL;

	if (ret_type(buf, s, r, c) == ERROR) p = "";
	else p = ret_pvalue(NULL, buf, s, r, c, -1);
	return strcons(strlen(p), p);
}

static LISP get_string(LISP row, LISP col)
{
	return x_get_string(row, col, NIL);
}

static LISP get_type(LISP bname, LISP row, LISP col)
{
	buffer *buf;
	int s, t, r, c;

	if (NULLP (bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname),
					w_list->buf, &s);
	}

	r = get_c_long(row);
	c = get_c_long(col);
	t = ret_type(buf, s, r, c);
	return flocons(t);
}

static LISP get_text(LISP row, LISP col)
{
	char *text = ret_text(buffer_of_window(w_list), w_list->sht,
				get_c_long(row), get_c_long(col));
	if (!text) text = "";

	return strcons(strlen(text), text);
}

static LISP siag_sum(LISP start, LISP end)
{
	double sum;
	int r, c, startr, startc, endr, endc;
	startr = get_c_long(car(start));
	startc = get_c_long(cdr(start));
	endr = get_c_long(car(end));
	endc = get_c_long(cdr(end));
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
	return flocons(sum);
}

static LISP siag_time(void)
{
	double t = time(NULL);
	return flocons(t);
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
	if (FLONUMP(p)) {
		siag_type = EXPRESSION;
		siag_result.number = FLONM(p);
	} else if (TYPEP(p, tc_string)) {
		siag_type = STRING;
		siag_result.text = p->storage_as.string.data;
	} else {
		siag_type = ERROR;
		siag_result.number = 0, errorflag = 1;
	}
}



#define BREAKCHARS "() \t\r\n"
#define TEMPLATE "(get-cell %ld %ld)"
#define RANGE "'RANGE %ld %ld %ld %ld"

static LISP get_xref(LISP bname, LISP cell)
{
	char new[1000];
	char *old = get_c_string(cell);
	long row, col;
	buffer *buf;
	if (NULLP(bname)) buf = buffer_of_window(w_list);
	else buf = find_buffer_by_name(get_c_string(bname));
	ref_expander(buf, old, new, BREAKCHARS, "%ld %ld", RANGE);
	sscanf(new, "%ld %ld", &row, &col);
	return x_get_cell(flocons(row), flocons(col), bname);
}

/* ---
expand Visicalc references
*/

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

static char *update_SIOD_references(buffer *buf, char *expr, int r1, int c1,
				int r2, int c2, int rd, int cd)
{
	char *p;
	int rc = ref_counter(buf, expr);

	if (!rc) return expr;

	/* rough upper bound on new length. A few bytes extra don't matter */
	/* strlen("r100000c100000")-strlen("r1c1") = 10 */
	p = MwMalloc(strlen(expr)+10*rc+1);
	rc = ref_updater(buf, expr, p, BREAKCHARS, r1, c1, r2, c2, rd, cd);
	if (rc) {
		expr = MwStrdup(p);
	}
	MwFree(p);
	return expr;
}


static long wrapper(buffer *b, char *expr, int s, int row, int col)
{
	/* mostly stolen from repl_c_string in slib.c */
	struct repl_hooks h;
	long retval;
	int want_sigint = 1;
	static int want_init = 1;
	char *expansion = expand_references(b, expr);

	h.repl_puts = siag_puts;
	h.repl_read = repl_c_string_read;
	h.repl_eval = NULL;
	h.repl_print = siag_print;
	repl_c_string_arg = expansion;
	repl_c_string_flag = 0;
	siag_sht = s;
	siag_row = row;
	siag_col = col;
	siag_buffer = b;
	setvar(cintern("R"), flocons((double)siag_row), NIL);
	setvar(cintern("C"), flocons((double)siag_col), NIL);

	retval = repl_driver(want_sigint, want_init, &h);
	if (expansion != expr) MwFree(expansion);
	want_init = 0;			/* only once... */
	return retval;
}

static cval parse_SIOD_expr(buffer *b, char *expr, int s, int row, int col)
{
	long retval;

	retval = wrapper(b, expr, s, row, col);
	return siag_result;
}

/* ---
   This function was originally in cmds.c, but has been
   moved here because of its similarity to parse_expr.
*/

static void execute_siod(char *s)
{
	long retval;
	int sh;
	buffer *buf;
	int r, c;

	if (ok2print)
		hide_cur(w_list);

	if (w_list) {
		buf = w_list->buf;
		sh = w_list->sht;
		r = get_point(w_list).row;
		c = get_point(w_list).col;
	} else {
		buf = NULL;
		sh = 0;
		r = 1;
		c = 1;
	}
	retval = wrapper(buf, s, sh, r, c);

	if (ok2print)
		show_cur(w_list);
}


/* ---
These functions allow implementation of commands in Scheme
rather than in C with Scheme wrappers
*/

static LISP lget_point(void)
{
	return MAKE_POSITION(get_point(w_list));
}

static LISP lget_mark(void)
{
	return MAKE_POSITION(get_mark(w_list));
}

static LISP lget_top(void)
{
	return MAKE_POSITION(get_top(w_list));
}

static LISP lget_blku(void)
{
	return MAKE_POSITION(block_upper(w_list));
}

static LISP lget_blkl(void)
{
	return MAKE_POSITION(block_lower(w_list));
}

static LISP lset_top(LISP top)
{
	position newtop;
	newtop.row = POSITION_ROW(top);
	newtop.col = POSITION_COL(top);
	set_top(w_list, newtop);
	prot_fixup(w_list);
	return NIL;
}

static LISP lset_prot(LISP prot)
{
	position newprot;
	newprot.row = POSITION_ROW(prot);
	newprot.col = POSITION_COL(prot);
	set_prot(w_list, newprot);
	prot_fixup(w_list);
	return NIL;
}

static LISP lget_prot(void)
{
	return MAKE_POSITION(get_prot(w_list));
}

static LISP lset_point(LISP p)
{
	position newpoint;
	newpoint.row = POSITION_ROW(p);
	newpoint.col = POSITION_COL(p);
	set_point(w_list, newpoint);
	prot_fixup(w_list);
	return NIL;
}

static LISP lset_mark(LISP p)
{
	position newmark;
	newmark.row = POSITION_ROW(p);
	newmark.col = POSITION_COL(p);
	set_mark(w_list, newmark);
	return NIL;
}

static LISP line_last_used_fun(void)
{
	int n = line_last_used(buffer_of_window(w_list), w_list->sht);
	return flocons(n);
}

static LISP col_last_used_fun(LISP row)
{
	int n = get_c_long(row);
	return flocons(col_last_used(buffer_of_window(w_list), w_list->sht, n));
}

static LISP max_lines_fun(void)
{
	return flocons(BUFFER_ROWS);
}

static LISP max_columns_fun(void)
{
	return flocons(BUFFER_COLS);
}

static LISP set_pr_scr(void)
{
	pr_scr_flag = TRUE;
	return NIL;
}

/* Convert (R . I) to R or "R+Ij". Result is number if I = 0, else string */
static LISP imint2ext(LISP v)
{
	double r, i;
	char b[1024];
	r = get_c_double(CAR(v));
	i = get_c_double(CDR(v));
	if (i == 0) return CAR(v);
	if (r == 0) {
		if (i == -1) strcpy(b, "-i");
		else if (i == 1) strcpy(b, "i");
		else sprintf(b, "%gi", i);
	} else {
		if (i == -1) sprintf(b, "%g-i", r);
		else if (i == 1) sprintf(b, "%g+i", r);
		else if (i < 0) sprintf(b, "%g-%gi", r, -i);
		else sprintf(b, "%g+%gi", r, i);
	}
	return strcons(strlen(b), b);
}

/* If v is number, returns (R . 0). If v is string, it is assumed to be
   of the form [+-]?R[+-]I[ij] where R and I are floating point numbers. */
static LISP imext2int(LISP v)
{
	char *p, *q;
	double rv, iv;
	if (FLONUMP(v)) return cons(v, flocons(0));
	p = get_c_string(v);
	if (!strcmp(p, "i") || !strcmp(p, "j")) {
		rv = 0;
		iv = 1;
	} else if (!strcmp(p, "-i") || !strcmp(p, "-j")) {
		rv = 0;
		iv = -1;
	} else {
		rv = strtod(p, &q);
		if (q == p) return NIL;
		if (*q == 'i' || *q == 'j') {
			iv = rv;
			rv = 0;
		} else if (*q == '\0') {
			iv = 0;
		} else if (!strcmp(q, "+i") || !strcmp(q, "+j")) {
			iv = 1;
		} else if (!strcmp(q, "-i") || !strcmp(q, "-j")) {
			iv = -1;
		} else {
			iv = strtod(q, &p);
			if (p == q) return NIL;
			if (*p != 'i' && *p != 'j') return NIL;
		}
	}
	return cons(flocons(rv), flocons(iv));
}

static LISP lset_array_value(LISP row, LISP col, LISP value)
{
	cval val;
	int r = get_c_long(row), c = get_c_long(col);
	char i[100];

	if (FLONUMP(value)) {
		val.number = get_c_double(value);
		sprintf(i, "%g", val.number);
		ins_data(siag_buffer, C_interpreter,
			i, val, MNUMBER, siag_sht, r, c);
	} else if (TYPEP(value, tc_string)) {
		val.text = get_c_string(value);
		ins_data(siag_buffer, C_interpreter,
			val.text, val, MTEXT, siag_sht, r, c);
	} else {
		;
	}
	return value;
}

void get_siod_coords(int *row, int *col, int *sheet, buffer **buf)
{
	*row = siag_row;
	*col = siag_col;
	*sheet = siag_sht;
	*buf = siag_buffer;
}

/* ---
*/
void init_position(void)
{
	init_subr_0("get-point", lget_point);
	init_subr_0("get-mark", lget_mark);
	init_subr_0("get-top", lget_top);
	init_subr_0("get-blku", lget_blku);
	init_subr_0("get-blkl", lget_blkl);
	init_subr_1("set-top", lset_top);
	init_subr_1("set-prot", lset_prot);
	init_subr_0("get-prot", lget_prot);
	init_subr_1("set-point", lset_point);
	init_subr_1("set-mark", lset_mark);
	init_subr_0("line-last-used", line_last_used_fun);
	init_subr_1("col-last-used", col_last_used_fun);
	init_subr_0("max-lines", max_lines_fun);
	init_subr_0("max-columns", max_columns_fun);
	init_subr_0("set-pr-scr", set_pr_scr);
	init_subr_3("put-property", lput_property);
	init_subr_2("get-property", lget_property);
	init_subr_2("siag_rowsum", lsiag_rowsum);
	init_subr_2("siag_colsum", lsiag_colsum);
	init_subr_3("set_array_value", lset_array_value);
}

/* dummy args for SIOD */
static char *siod_argv[] = {
  "siod",
  "-h30000:10",	/* 100000:10 */
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
	init_subr_2("get-cell", get_cell);
	init_subr_2("get_number", get_number);
	init_subr_2("get-string", get_string);
	init_subr_3("get-type", get_type);
	init_subr_2("get-text", get_text);
	init_subr_2("get_xref", get_xref);
	init_subr_1("h_avg", h_avg);
	init_subr_2("sum", siag_sum);
	init_subr_3("x-get-cell", x_get_cell);
	init_subr_3("x-get-string", x_get_string);
	init_subr_0("time", siag_time);
	init_subr_1("imint2ext", imint2ext);
	init_subr_1("imext2int", imext2int);
	init_subr_2("exec-expr", lexec_expr);

	return register_interpreter("SIOD", parse_SIOD_expr, execute_siod,
					update_SIOD_references);
}

