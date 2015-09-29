/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2001  Ulric Eriksson <ulric@siag.nu>

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
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "calc.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

int errorflag;
int siag_type;
int a1_refs = 1;

#define MAX_INTERPRETERS 10

int interpreter_count = 0;

static struct {
	char *name;
	cval (*evalfn)(buffer *, char *, int, int, int);
	void (*execfn)(char *);
	char *(*ref_fn)(buffer *, char *, int, int, int, int, int, int);
} interpreters[MAX_INTERPRETERS];

char *intnames[MAX_INTERPRETERS];

/* ---
*/
char *interpreter2name(int interpreter)
{
	if (interpreter < 0 || interpreter >= interpreter_count)
		return "NONE";
	return interpreters[interpreter].name;
}

/* ---
*/
int name2interpreter(char *name)
{
	int i;
	for (i = 0; i < interpreter_count; i++)
		if (!MwStrcasecmp(name, interpreters[i].name)) return i;
	return -1;
}

/* ---
*/
int register_interpreter(char *name,
			cval (*evalfn)(buffer *, char *, int, int, int),
			void (*execfn)(char *),
			char *(*ref_fn)(buffer *, char *, int, int, int,
					int, int, int))
{
	if (interpreter_count >= MAX_INTERPRETERS) return -1;	/* besetzt */
	interpreters[interpreter_count].evalfn = evalfn;
	interpreters[interpreter_count].execfn = execfn;
	interpreters[interpreter_count].ref_fn = ref_fn;
	interpreters[interpreter_count].name = name;
	intnames[interpreter_count] = MwStrdup(name);
	intnames[interpreter_count+1] = NULL;
	return interpreter_count++;
}

/* ---
*/
void exec_expr(int interpreter, char *expr)
{
	void (*execfn)(char *);

	errorflag = FALSE;
	if (interpreter < 0 || interpreter >= interpreter_count) return;
	execfn = interpreters[interpreter].execfn;
	if (execfn == NULL) {

		return;
	}

	execfn(expr);
}

static int looks_like_scheme(char *p)
{
	char *q;

	for (q = p; *q; q++)
		if (!isspace(*q)) return (*q == '(');
	return 0;
}

void exec_unknown(char *expr)
{
	if (looks_like_scheme(expr)) {
		exec_expr(siod_interpreter, expr);
	} else {
		exec_expr(C_interpreter, expr);
	}
}

/* ---
*/
void va_execute(char *fmt, ...)
{
	va_list ap;
	char cmd[2000];

	va_start(ap, fmt);
	vsprintf(cmd, fmt, ap);
	execute(cmd);
	va_end(ap);
}

/* ---
Expressions starting with a '=' are evaluated as C
This same function also does strings
*/

cval parse_expr(buffer *b, int interpreter,
		char *expr, int s, int row, int col)
{
	cval (*evalfn)(buffer *, char *, int, int, int);
	cval nothing;

	nothing.number = 0;

	/* the next two lines seem to contradict each other, but it is
	   necessary to set the type in case siod bails out before
	   we make it to siag_print(). */
	errorflag = 0;
	siag_type = ERROR;

	if (interpreter >= interpreter_count) return nothing;
	if ((interpreter == siod_interpreter) && (*expr == '=')) {
		expr++;
		interpreter = C_interpreter;
	}

	evalfn = interpreters[interpreter].evalfn;
	if (evalfn == NULL) return nothing;

	return evalfn(b, expr, s, row, col);
}

/* ---
int calc_sheet(buffer *b, int s)
   Recalculate all the cells in matrix.
   Returns 0 if successful, otherwise 1.

 Remarkably enough, this function requires no conversion whatsoever
   to go from long to double.

Also figures out where the real end of buffer is, i.e. updates alloc_lines
and frees any empty lines. Never frees the first line.
*/

static int calc_sheet(buffer *b, int s)
{
	register int row, col;
	int lc;
	int lastr;
	int t;

	cval value;
	value.number = 0;

	errorflag = FALSE;
	lastr = 1;

	/* delete transient matrix and db tables */
	for (row = 1; row <= line_last_used(b, s); row++) {
		lc = col_last_used(b, s, row);
		for (col = 1; col <= lc; col++) {
			t = ret_type(b, s, row, col);
			if (t == MNUMBER || t == MTEXT) {
				ins_data(b, siod_interpreter, NULL,
					value, EMPTY, s, row, col);
				/* don't change the cell's format */
			}
		}
	}
	/* we must call line_last_used for every line, because it is
	   possible that functions have been stuffing cells */
	for (row = 1; row <= line_last_used(b, s); row++) {
		lc = col_last_used(b, s, row);
		for (col = 1; col <= lc; col++) {
			switch (ret_type(b, s, row, col)) {
			case STRING:
			case EXPRESSION:
			case ERROR:
				value = parse_expr(b,
						ret_interpreter(b, s, row, col),
						ret_text(b, s, row, col),
						s, row, col);
				ins_data(b,
					ret_interpreter(b, s, row, col),
					ret_text(b, s, row, col),
					value, siag_type, s, row, col);
				lastr = row;
				break;
			case LABEL:
			case CONSTANT:
			case MTEXT:
			case MNUMBER:
				lastr = row;
				break;
			default:
				;
			}
		}
	}

	free_rows(b, s, lastr+1);
	return 0;
}				/* calc_matrix */

/* ---
*/
int calc_matrix(buffer *b)
{
	int s;

	for (s = 0; s < b->nsht; s++)
		calc_sheet(b, s);
	return 0;
}

/* ---
make a rough upper bound on the number of references
*/

int ref_a1_counter(char *expr)
{
	char *p = expr;
	int cd, c;

	cd = 0;

	while ((c = *p++))
		if (isalpha(c) && *p && (*p == '$' || isdigit(*p))) cd++;
	return cd;
}

/* ---
*/
int ref_counter(buffer *buf, char *expr)
{
	int a1r;
	int cd, rd, c;

	if (buf) a1r = buf->a1_refs;
	else a1r = a1_refs;

	cd = 0;
	rd = 0;

	if (a1r) {
		return ref_a1_counter(expr);
	}

	while ((c = *expr++)) {
		if (c == 'r' || c == 'R') {
			if (*expr == '$' || isdigit(*expr)) rd++;
		} else if (c == 'c' || c == 'C') {
			if (*expr == '$' || isdigit(*expr)) cd++;
		}
	}

	return MIN(cd, rd);
}

/* ---
Portability note: this code assumes that alphabetic characters are
   contiguous and in alphabetic order. This is not required by C, but by
   common sense and by me.
*/

int a1tol(char *p, char **q)
{
	int n = 0;
	int c;

	while ((c = *p++)) {
		if (c >= 'A' && c <= 'Z') {
			n = 26*n+c-'A'+1;
		} else if (c >= 'a' && c <= 'z') {
			n = 26*n+c-'a'+1;
		} else {
			break;
		}
	}
	*q = p-1;
	return n;
}

typedef enum {
	REF_START, REF_STRING, REF_LIMBO
} ref_state;

/*
Convert a cell reference in a string p to row/column coordinates.
Leading white space is skipped. If a reference is found, a pointer
to the first character after the reference is stored in endp. If
no reference was found, a pointer to the head of the string is stored
in endp. Note that endp must not be NULL, unlike strtol.

Here's what a reference looks like:

0. Optional whitespace
1. An optional $
2. A sequence of alphabetic characters
3. Another optional $
4. A sequence of digits
*/
void read_ref(char *p, char **endp, int *r, int *c)
{
	*endp = p;

	/* skip initial whitespace */
	while (*p && isspace(*p)) p++;
	if (*p == '$') p++;
	if (!isalpha(*p)) return;	/* no reference here */
	*c = a1tol(p, &p);
	if (*p == '$') p++;
	if (!isdigit(*p)) return;	/* no row number */
	*r = strtol(p, &p, 10);
	if (*r >= 1 && *r <= BUFFER_ROWS && *c >= 1 && *c <= BUFFER_COLS) {
		*endp = p;
	}
}

/* ---
expand references from src to dest, which must be large enough
brk contains break characters for this interpreter
tmpl contains the expansion template: (get-cell %ld %ld) for Scheme
rng contains the range template, if any: 'RANGE %ld %ld %ld %ld for Scheme

We want to make sure that e.g. A1$ or somesuch is not treated
as a reference, that A1..XX is not treated as a range and so on.
That is pretty straightforward: we require
that each reference is surrounded by break characters (or
that it is first or last in the expression)
*/

void ref_a1_expander(char *src, char *dest, char *brk, char *tmpl, char *rng)
{
	int r1, c1, r2, c2;
	char *p, *q;

	/* Look ma, a state machine made of gotos. The reference stuff
	   is so ad hoc that it is hard to make it very clean. */
Ref_start:
	if (*src == '\0') goto Ref_end;
	if (strchr(brk, *src)) {
		*dest++ = *src++;
		goto Ref_start;
	} else {
		read_ref(src, &p, &r1, &c1);
		if (strchr(brk, *p)) {
			src = p;
			sprintf(dest, tmpl, r1, c1);
			dest += strlen(dest);
			goto Noref_start;
		} else if (p[0] == '.' && p[1] == '.') {
			read_ref(p+2, &q, &r2, &c2);
			if (strchr(brk, *q)) {
				src = q;
				sprintf(dest, rng, r1, c1, r2, c2);
				dest += strlen(dest);
				goto Noref_start;
			} else {
				*dest++ = *src++;
				goto Noref_start;
			}
		} else {
			*dest++ = *src++;
			goto Noref_start;
		}
	}
	/* There is no way to get here */

Noref_start:
	if (*src == '\0') goto Ref_end;
	if (strchr(brk, *src)) {
		*dest++ = *src++;
		goto Ref_start;
	} else {
		*dest++ = *src++;
		goto Noref_start;
	}
	/* There is no way to get here either */

Ref_end:
	*dest = '\0';
}

/* ---
*/
void ref_expander(buffer *buf, char *src, char *dest,
		char *brk, char *tmpl, char *rng)
{
	int a1_refs;
	ref_state state = REF_START;
	char *exp;
	char *start, *end;
	int row, col, r2, c2;
	int c;

	if (buf) a1_refs = buf->a1_refs;
	else a1_refs = 1;

	if (a1_refs) {
		ref_a1_expander(src, dest, brk, tmpl, rng);
		return;
	}

	while ((c = *src++)) {
		*dest++ = c;	/* copy straight through */
		switch (state) {
		case REF_START:
			if ((c == 'r' || c == 'R') && isdigit(*src)) {
				start = src;
				exp = dest-1;
				row = strtol(src, &end, 10);
				if ((end[0] != 'c' && end[0] != 'C') ||
					(end[1] && !isdigit(end[1]))) {
					state = REF_LIMBO;
					break;
				}
				src = end+1;	/* start of col number */
				col = strtol(src, &end, 10);
				if (end == src ||
					(end[0] && end[0] != '.' &&
					!strchr(brk, end[0]))) {
					src = start;	/* backpedal */
					state = REF_LIMBO;
					break;
				}
				src = end;
	/* if we get this far, there really is a reference here */
				if (src[0] != '.' || src[1] != '.' ||
					(src[2] != 'r' && src[2] != 'R') ||
					!isdigit(src[3])) {	/* single */
					sprintf(exp, tmpl, row, col);
					dest = exp+strlen(exp);
					continue;
				}
	/* and now, look for the other end of the range */
				src += 3;	/* skip past "..r" */
				r2 = strtol(src, &end, 10);
				if ((end[0] != 'c' && end[0] != 'C') ||
					(end[1] && !isdigit(end[1]))) {
					src = start;
					state = REF_LIMBO;
					break;
				}
				src = end+1;
				c2 = strtol(src, &end, 10);
				if (end == src ||
					(end[0] && !strchr(brk, end[0]))) {
					src = start;
					state = REF_LIMBO;
					break;
				}
				src = end;
	/* found the other end */
				sprintf(exp, rng, row, col, r2, c2);
				dest = exp+strlen(exp);
			} else if (c == '"') {
				state = REF_STRING;
			} else if (!strchr(brk, c)) {
				state = REF_LIMBO;
			}
			break;
		case REF_LIMBO:
			if (strchr(brk, c)) {
				state = REF_START;
			 }else if (c == '"') {
				state = REF_STRING;
			}
			break;
		case REF_STRING:
			if (c == '"') {
				state = REF_LIMBO;
			}
			break;
		default:	/* shouldn't happen */
			break;
		}
	}
	*dest = '\0';
}

/* ---
move block between (r1,c1) and (r2,c2), direction (rd,cd)
*/

char *a1coord(int n)
{
	static char b[100];
	static char digits[] = " ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	int digit;
	int i = 0, j = 0, c;

	while (n) {
		digit = n % 26;
		if (!digit) digit = 26;
		b[i++] = digits[digit];
		n -= digit;
		n /= 26;
	}
	b[i--] = '\0';
	while (j < i) {
		c = b[i];
		b[i--] = b[j];
		b[j++] = c;
	}
	return b;
}

/* ---
*/
int ref_a1_updater(char *src, char *dest, char *brk,
		int r1, int c1, int r2, int c2, int rd, int cd)
{
	ref_state state = REF_START;
	char *start, *end, *exp;
	int row, col;
	int c, conv = 0;

	while ((c = *src++)) {
		*dest++ = c;
		switch (state) {
		case REF_START:
			if (isalpha(c)) {
				start = src;
				exp = dest-1;
				col = a1tol(src-1, &end);
				if (!isdigit(end[0])) {
					state = REF_LIMBO;
					break;
				}
				src = end;
				row = strtol(src, &end, 10);
				if (end[0] && end[0] != '.' &&
						!strchr(brk, end[0])) {
					src = start;
					state = REF_LIMBO;
					break;
				}
				src = end;
	/* if we get this far, there really is a reference here */
				if (row >= r1 && row <= r2 &&
					col >= c1 && col <= c2) {
					conv++;
					row += rd;
					col += cd;
				}
				sprintf(exp, "%s%d", a1coord(col), row);
				dest = exp+strlen(exp);
				if (src[0] == '.' && src[1] == '.' &&
					isalpha(src[2])) {
					strcat(dest, "..");
					dest += 2;
					src += 2;
				}
			} else if (c == '"') {
				state = REF_STRING;
			} else if (!strchr(brk, c)) {
				state = REF_START;
			}
			break;
		case REF_LIMBO:
			if (strchr(brk, c)) {
				state = REF_START;
			} else if (c == '"') {
				state = REF_STRING;
			}
			break;
		case REF_STRING:
			if (c == '"') {
				state = REF_LIMBO;
			}
			break;
		default:	/* shouldn't happen */
			break;
		}
	}
	*dest = '\0';
	return conv;
}

/* ---
*/
int ref_updater(buffer *buf, char *src, char *dest, char *brk,
		int r1, int c1, int r2, int c2, int rd, int cd)
{
	ref_state state = REF_START;
	char *start, *end, *exp;
	int row, col;
	int c, conv = 0;

	if (buf == NULL || buf->a1_refs) {
		return ref_a1_updater(src, dest, brk, r1, c1, r2, c2, rd, cd);
	}

	while ((c = *src++)) {
		*dest++ = c;	/* copy straight through */
		switch (state) {
		case REF_START:
			if ((c == 'r' || c == 'R') && isdigit(*src)) {
				start = src;
				exp = dest-1;
				row = strtol(src, &end, 10);
				if ((end[0] != 'c' && end[0] != 'C') ||
					(end[1] && !isdigit(end[1]))) {
					state = REF_LIMBO;
					break;
				}
				src = end+1;
				col = strtol(src, &end, 10);
				if (end[0] && end[0] != '.' &&
					!strchr(brk, end[0])) {
					src = start;
					state = REF_LIMBO;
					break;
				}
				src = end;
	/* if we get this far, there really is a reference here */
				if (row >= r1 && row <= r2 &&
					col >= c1 && col <= c2) {
					conv++;
					row += rd;
					col += cd;
				}
				sprintf(exp, "R%dC%d", row, col);
				dest = exp+strlen(exp);
				if (src[0] == '.' && src[1] == '.' &&
					(src[2] == 'r' || src[2] == 'R')) {
					strcat(dest, "..");
					dest += 2;
					src += 2;
				}
			} else if (c == '"') {
				state = REF_STRING;
			} else if (!strchr(brk, c)) {
				state = REF_LIMBO;
			}
			break;
		case REF_LIMBO:
			if (strchr(brk, c)) {
				state = REF_START;
			} else if (c == '"') {
				state = REF_STRING;
			}
			break;
		case REF_STRING:
			if (c == '"') {
				state = REF_LIMBO;
			}
			break;
		default:	/* shouldn't happen */
			break;
		}
	}
	*dest = '\0';
	return conv;
}

/* ---
expand Visicalc references
*/

char *update_references(buffer *buf, int interpreter, char *expr,
		int r1, int c1, int r2, int c2, int rd, int cd)
{
	char *(*ref_fn)(buffer *, char *, int, int, int, int, int, int);

	if (interpreter < 0 || interpreter >= interpreter_count) return expr;
	ref_fn = interpreters[interpreter].ref_fn;
	if (ref_fn == NULL) {
		return expr;
	}
	return ref_fn(buf, expr, r1, c1, r2, c2, rd, cd);
}

/* ---
*/
void update_all_references(buffer *b, int s, int r1, int c1, int r2, int c2,
				int rd, int cd)
{
	int row, col;
	int lr, lc;
	char *old, *new;

	lr = line_last_used(b, s);
	for (row = 1; row <= lr; row++) {
		lc = col_last_used(b, s, row);
		for (col = 1; col <= lc; col++) {
			switch (ret_type(b, s, row, col)) {
				cval value;
			case STRING:
			case EXPRESSION:
				old = ret_text(b, s, row, col);
				new = update_references(
					b,
					ret_interpreter(b, s, row, col),
					old,
					r1, c1, r2, c2, rd, cd);
				if (old != new) {
					value = ret_val(b, s, row, col);
					ins_data(b,
						ret_interpreter(b, s, row, col),
						new,
						value, siag_type, s, row, col);
					MwFree(new);
				}
				break;
			default:
				;
			}
		}
	}
}

/* convert digit number to "text" number
1 => A
26 => 
27 => AA
52 => AZ
53 => BA
*/
char *colnum_text(buffer *buf, int col)
{
	int a1_refs = buf->a1_refs;
        static char b[80];

	if (a1_refs) {
		static char letters[] = " ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		int i = 0, j = 0, c;
		while (col) {
			int digit = col % 26;
			if (!digit) digit = 26;
			b[i++] = letters[digit];
			col -= digit;
			col /= 26;
		}
		b[i--] = '\0';
		while (j < i) {
			c = b[i];
			b[i--] = b[j];
			b[j++] = c;
		}
	} else {
		sprintf(b, "%d", col);
	}
	return b;
}

