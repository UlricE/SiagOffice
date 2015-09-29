/*
   Siag, Scheme In A Grid
   Copyright (C) 2000  Ulric Eriksson <ulric@siag.nu>
 
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
 
#ifdef CCMATH

#include <stdlib.h>
#include <ccmath.h>
#include "../siod/siod.h"
#include "calc.h"

/* helper functions */
/* the problem here is that we don't know the current buffer
   and current sheet. That information is static in siodi.c
*/
static double get_value(buffer *buf, int sheet, int row, int col)
{
	if (row < 1 || row > BUFFER_ROWS || col < 1 || col > BUFFER_COLS)
		return 0.0;

	switch (ret_type(buf, sheet, row, col)) {
	case STRING:
	case LABEL:
		return strtod(ret_string(buf, sheet, row, col), NULL);
	case EMPTY:
	case ERROR:
		return 0.0;
	default:
		return ret_val(buf, sheet, row, col).number;
	}
}

static double *fetch_array(int x1, int y1, int x2, int y2)
{
	int row, col, sheet;
	buffer *buf;
	int x, y;
	int n = (x2-x1+1)*(y2-y1+1);
	double *a = malloc(n*sizeof *a);
	int i = 0;
	if (a == NULL) return NULL;
	get_siod_coords(&row, &col, &sheet, &buf);
	for (x = x1; x <= x2; x++) {
		for (y = y1; y <= y2; y++) {
			a[i++] = get_value(buf, sheet, x, y);
		}
	}
	return a;
}

static void store_array(int x1, int y1, int x2, int y2, double *a)
{
	int row, col, sheet;
	buffer *buf;
	int x, y;
	int i = 0;
	cval val;
	int type = MNUMBER;
	char b[100];
	if (a == NULL) return;
	get_siod_coords(&row, &col, &sheet, &buf);
	for (x = x1; x <= x2; x++) {
		for (y = y1; y <= y2; y++) {
			val.number = a[i++];
			if (x != x1 || y != y1) {
				sprintf(b, "%g", val.number);
				ins_data(buf, C_interpreter,
					b, val, type, sheet, y, x);
			}
		}
	}
}


/* linear algebra */

/*
;@cc_solv(a, b, n)
;@     Solve a general linear system  A*x = b.
; 
;     int solv(double a[],double b[],int n)
;
;       a = array containing system matrix A in row order
;            (altered to L-U factored form by computation)
;
;       b = array containing system vector b at entry and
;           solution vector x at exit
;
;       n = dimension of system
;@
;@
*/
static LISP cc_solv1(LISP a1, LISP a2, LISP b1, LISP b2)
{
	int row, col, sheet;
	buffer *buf;
	int ax1 = get_c_long(CAR(a1)), ay1 = get_c_long(CDR(a1));
	int ax2 = get_c_long(CAR(a2)), ay2 = get_c_long(CDR(a2));
	int bx1 = get_c_long(CAR(b1)), by1 = get_c_long(CDR(b1));
	int bx2 = get_c_long(CDR(b2)), by2 = get_c_long(CDR(b2));
	int n = by2-by1+1;
	double *a = fetch_array(ax1, ay1, ax2, ay2);
	double *b = fetch_array(bx1, by1, bx2, by2);
	if (a == NULL || b == NULL || solv(a, b, n) == 0) return NIL;
	get_siod_coords(&row, &col, &sheet, &buf);
	store_array(row, col, row+n-1, col, b);
	return flocons(b[0]);
}

/* numerical integration */

/* geometry and trigonometry */

/* curve fitting and least squares */

/* roots and optima */

/* fourier analysis */

/* simulation support */

/* statistical functions */

/* special functions */

/* sorts and searches */

/* time series analysis */

/* complex arithmetic */

/* high precision arithmetic */

/* utilities */

/*
;@pwr(y, n)
;@Compute an integral power of a double precision number.
;@
;@
*/
static LISP cc_pwr(LISP ly, LISP ln)
{
	double y = get_c_double(ly);
	int n = get_c_long(ln);
	double z = pwr(y, n);
	return flocons(z);
}

void init_ccmath(void)
{
	init_subr_4("cc_solv1", cc_solv1);	/* cc_solv in ccmath.scm */
	init_subr_2("cc_pwr", cc_pwr);
}

#else

void init_ccmath(void)
{
	;
}

#endif	/* CCMATH */
