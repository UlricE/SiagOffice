/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2000  Ulric Eriksson <ulric@siag.nu>

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "../siod/siod.h"
#include "../config.h"

#ifdef HAVE_ACOSH
/*
;@acosh(x)
;@   ACOSH function calculates the inverse hyperbolic cosine of x; that is
;   the value whose hyperbolic cosine is x. If x is less than 1.0,
;   acosh() returns the NUM! error.
;XL
;@   ACOSH(2) equals 1.31696.
;
;   ACOSH(5.3) equals 2.35183.
;@ACOS , ASINH , DEGREES , RADIANS
*/
static LISP wrap_acosh (LISP x)
{
  /*	if NFLONUMP(x) err("wta(1st) to acosh\n", x);
   */	return flocons(acosh(get_c_double(x)));
}
#endif

#ifdef HAVE_ASINH
/*
;@asinh(x)
;@   ASINH function calculates the inverse hyperbolic sine of x; that is
;   the value whose hyperbolic sine is x.
;XL
;@   ASINH(0.5) equals 0.481212.
; 
;   ASINH(1.0) equals 0.881374.
;@ASIN , ACOSH , SIN , COS , DEGREES , RADIANS
*/
static LISP wrap_asinh (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to asinh\n", x);
	return flocons(asinh(get_c_double(x)));
}
#endif

#ifdef HAVE_ATANH
/*
;@atanh(x)
;@   ATANH function calculates the inverse hyperbolic tangent of x; that
;   is the value whose hyperbolic tangent is x. If the absolute value of
;   x is greater than 1.0, ATANH returns NUM! error. This function is
;   Excel compatible.
;@   ATANH(0.5) equals 0.549306.
;
;   ATANH(0.8) equals 1.098612.
;@ATAN , TAN , SIN , COS , DEGREES , RADIANS                                    
*/
static LISP wrap_atanh (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to atanh\n", x);
	return flocons(atanh(get_c_double(x)));
}
#endif

/*
;@ceil(x)
;@   CEIL function rounds x up to the next nearest integer.
; 
;XL
;@   CEIL(0.4) equals 1.
; 
;   CEIL(-1.1) equals -1.
; 
;   CEIL(-2.9) equals -2.
;@ABS , FLOOR , INT
*/
static LISP wrap_ceil (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to ceil\n", x);
	return flocons(ceil(get_c_double(x)));
}

/*
;@cosh(x)
;@   COSH function returns the hyperbolic cosine of x, which is defined
;   mathematically as (exp(x) + exp(-x)) / 2. x is in radians.
;XL
;@   COSH(0.5) equals 1.127626.
; 
;   COSH(1) equals 1.543081.
;@COS , SIN , SINH , TAN , TANH , RADIANS , DEGREES , EXP
*/
static LISP wrap_cosh (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to cosh\n", x);
	return flocons(cosh(get_c_double(x)));
}

/*
;@fabs(x)
;@   FABS returns the absolute value of the number x.
;@   fabs(1) equals 1.
;
;   fabs(-3.14) equals 3.14.
;@ABS
*/
static LISP wrap_fabs (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to fabs\n", x);
	return flocons(fabs(get_c_double(x)));
}

/*
;@floor(x)
;@The  floor()  function  rounds  x downwards to the nearest
;       integer, returning that value as a double.
;@floor(3.14) equals 3.
;
;floor(-3.14) equals -4.
;@ceil
*/
static LISP wrap_floor (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to floor\n", x);
	return flocons(floor(get_c_double(x)));
}

/*
;@log_10(x)
;@The log10() function returns the base-10 logarithm of x.
;@
;@log
*/
static LISP wrap_log10 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to log_10\n", x);
	return flocons(log10(get_c_double(x)));
}

/*
;@sinh(x)
;@   SINH function returns the hyperbolic sine of x, which is defined
;   mathematically as (exp(x) - exp(-x)) / 2.
;XL
;@   SINH(0.5) equals 0.521095.
;@SIN , COS , COSH , TAN , TANH , DEGREES , RADIANS , EXP
*/
static LISP wrap_sinh (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to sinh\n", x);
	return flocons(sinh(get_c_double(x)));
}

/*
;@tanh(x)
;@   The TANH function returns the hyperbolic tangent of x, which is
;   defined mathematically as sinh(x) / cosh(x).
;XL
;@   TANH(2) equals 0.96402758.
;@TAN , SIN , SINH , COS , COSH , DEGREES , RADIANS
*/
static LISP wrap_tanh (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to tanh\n", x);
	return flocons(tanh(get_c_double(x)));
}

/*
;@erf(x)
;@   ERF function returns the integral of the error function between
;   zero and x.
;XL
;@   ERF(0.4) equals 0.428392355.
;@ERFC
*/
static LISP wrap_erf (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to erf\n", x);
	return flocons(erf(get_c_double(x)));
}

/*
;@erfc(x)
;@   The ERFC function returns the integral of the complimentary error
;   function between the limits 0 and x.
;@
;@erf
*/
static LISP wrap_erfc (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to erfc\n", x);
	return flocons(erfc(get_c_double(x)));
}

/*
;@j_0(x)
;@The  j_0()  and j_1() functions return Bessel functions of x
;       of the first kind of orders 0 and  1,  respectively.
;@
;@j_1, jn, y_0, y_1, yn
*/
static LISP wrap_j0 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to j_0\n", x);
	return flocons(j0(get_c_double(x)));
}

/*
;@j_1(x)
;@The  j_0()  and j_1() functions return Bessel functions of x
;       of the first kind of orders 0 and  1,  respectively.
;@
;@j_0, jn, y_0, y_1, yn
*/
static LISP wrap_j1 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to j_1\n", x);
	return flocons(j1(get_c_double(x)));
}

/*
;@jn(n, x)
;@The
;       jn()  function  returns  the  Bessel  function of x of the
;       first kind of order n.
;@
;@j_0, j_1, y_0, y_1, yn
*/
static LISP wrap_jn(LISP n, LISP x)
{
	return flocons(jn(get_c_long(n), get_c_double(x)));
}

/*
;@lgamma(x)
;@The  lgamma()  function  returns  the  log of the absolute
;       value of the Gamma function.
;@
;@infnan
*/
static LISP wrap_lgamma (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to lgamma\n", x);
	return flocons(lgamma(get_c_double(x)));
}

/*
;@y_0(x)
;@The y_0() and y_1() functions return Bessel functions  of  x
;       of  the  second kind of orders 0 and 1, respectively.
;@
;@j_0, j_1, jn, y_1, yn
*/
static LISP wrap_y0 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to y_0\n", x);
	return flocons(y0(get_c_double(x)));
}

/*
;@y_1(x)
;@The y_0() and y_1() functions return Bessel functions  of  x
;       of  the  second kind of orders 0 and 1, respectively.
;@
;@j_0, j_1, jn, y_0, yn
*/
static LISP wrap_y1 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to y_1\n", x);
	return flocons(y1(get_c_double(x)));
}

/*
;@yn(n, x)
;@The yn() function returns the Bessel function of x of the
; second kind of order n.
;@
;@j_0, j_1, jn, y_0, y_1, yn
*/
static LISP wrap_yn(LISP n, LISP x)
{
	return flocons(yn(get_c_long(n), get_c_double(x)));
}

#ifdef HAVE_LOG1P
/*
;@log1p(x)
;@log1p(x) returns a value equivalent to `log (1 +  x)'.  It
;       is computed in a way that is accurate even if the value of
;       x is near zero.
;@
;@exp log
*/
static LISP wrap_log1p (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to log1p\n", x);
	return flocons(log1p(get_c_double(x)));
}
#endif

/*
;@hypot(x, y)
;@The hypot() function returns the sqrt(x*x + y*y).  This is
;       the length of the hypotenuse  of  a  right-angle  triangle
;       with sides of length x and y, or the distance of the point
;       (x, y) from the origin.
;@
;@sqrt
*/
static LISP wrap_hypot(LISP x, LISP y)
{
	if NFLONUMP(x) err("wta(1st) to hypot\n", x);
	if NFLONUMP(y) err("wta(2nd) to hypot\n", y);
	return flocons(hypot(get_c_double(x), get_c_double(y)));
}

/*
;@pow_2(x)
;@
;@
;@
*/
static LISP wrap_pow2 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to pow_2\n", x);
	return flocons(pow(2.0, get_c_double(x)));
}

/*
;@pow_10(x)
;@
;@
;@
*/
static LISP wrap_pow10 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to pow_10\n", x);
	return flocons(pow(10.0, get_c_double(x)));
}

#ifdef HAVE_EXPM1
/*
;@expm_1(x)
;@expm_1(x) returns a value equivalent to `exp (x) -  1'.  It
;       is computed in a way that is accurate even if the value of
;       x is near zero--a case where `exp (x) - 1' would be  inaccurate
;  due  to subtraction of two numbers that are nearly
;       equal.
;@
;@exp log
*/
static LISP wrap_expm1 (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to expm_1\n", x);
	return flocons(expm1(get_c_double(x)));
}
#endif

#ifdef HAVE_CBRT
/*
;@cbrt(x)
;@The  cbrt()  function  returns  the  cube root of x.  This
;       function cannot fail; every representable real value has a
;       representable real cube root.
;@
;@sqrt, pow
*/
static LISP wrap_cbrt (LISP x)
{
	if NFLONUMP(x) err("wta(1st) to cbrt\n", x);
	return flocons(cbrt(get_c_double(x)));
}
#endif

#ifdef HAVE_DREM
/*
;@drem(x, y)
;@The  drem()  function computes the remainder of dividing x
;       by y.  The return value is x - n * y, where n is the  quo-
;       tient  of  x  / y, rounded to the nearest integer.  If the
;       quotient is 1/2, it is rounded to the even number.
;@
;@fmod
*/
static LISP wrap_drem(LISP x, LISP y)
{
	if NFLONUMP(x) err("wta(1st) to drem\n", x);
	if NFLONUMP(y) err("wta(2nd) to drem\n", y);
	return flocons(drem(get_c_double(x), get_c_double(y)));
}
#endif

extern int do_roman(char *, char *);

/*
;@roman(x)
;@Converts between roman and decimal numbers. If x is a number or
; a string where the first character is a digit, converts to roman.
; Otherwise converts to number.
;@
;@
*/
static LISP wrap_roman(LISP x)
{
	double d;
	char *p, b[1024], c[1024];

	if (FLONUMP(x)) {
		d = get_c_long(x);
		sprintf(b, "%ld", (long)d);
		p = b;
	} else {
		p = get_c_string(x);
	}
	if (do_roman(p, c)) {
		if (isdigit(c[0])) return flocons(strtol(c, NULL, 10));
		else return strcons(-1, c);
	}
	return NIL;
}

/* ---
*/
void init_mathwrap(void)
{
#ifdef HAVE_ACOSH
	init_subr_1("acosh", wrap_acosh);
#endif
#ifdef HAVE_ASINH
	init_subr_1("asinh", wrap_asinh);
#endif	/* hpux */
#ifdef HAVE_ATANH
	init_subr_1("atanh", wrap_atanh);
#endif
	init_subr_1("ceil", wrap_ceil);
	init_subr_1("cosh", wrap_cosh);
	init_subr_1("fabs", wrap_fabs);
	init_subr_1("floor", wrap_floor);
	init_subr_1("log_10", wrap_log10);
	init_subr_1("log10", wrap_log10);
	init_subr_1("sinh", wrap_sinh);
	init_subr_1("tanh", wrap_tanh);
	init_subr_1("erf", wrap_erf);
	init_subr_1("erfc", wrap_erfc);
	init_subr_1("j_0", wrap_j0);
	init_subr_1("j0", wrap_j0);
	init_subr_1("j_1", wrap_j1);
	init_subr_1("j1", wrap_j1);
	init_subr_2("jn", wrap_jn);
	init_subr_1("lgamma", wrap_lgamma);
	init_subr_1("y_0", wrap_y0);
	init_subr_1("y0", wrap_y0);
	init_subr_1("y_1", wrap_y1);
	init_subr_1("y1", wrap_y1);
	init_subr_2("yn", wrap_yn);
#ifdef HAVE_LOG1P
	init_subr_1("log1p", wrap_log1p);
#endif
	init_subr_2("hypot", wrap_hypot);
	init_subr_1("pow_2", wrap_pow2);
	init_subr_1("pow2", wrap_pow2);
	init_subr_1("pow_10", wrap_pow10);
	init_subr_1("pow10", wrap_pow10);
#ifdef HAVE_EXPM1
	init_subr_1("expm_1", wrap_expm1);
	init_subr_1("expm1", wrap_expm1);
#endif
#ifdef HAVE_CBRT
	init_subr_1("cbrt", wrap_cbrt);
#endif
#ifdef HAVE_DREM
	init_subr_2("drem", wrap_drem);
#endif
	init_subr_1("roman", wrap_roman);
}

