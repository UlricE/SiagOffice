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

#ifdef HAVE_LIBGMP

/*
gmp.c

Each function accepts input as either LISP floats or LISP strings.
The latter is used to get arbitrary precision. Results are always
returned as LISP strings.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>

#include <gmp.h>

#include "../common/common.h"
#include <Mowitz/MwUtils.h>

#include "../siod/siod.h"

#include "calc.h"

static gmp_randstate_t randstate;

static long lmpz_geti(LISP a1)
{
	char *p;
	if (FLONUMP(a1)) {
		return get_c_long(a1);
	} else {
		return strtod(get_c_string(a1), &p);
	}
}

static void lmpz_get(mpz_t a, LISP a1)
{
	mpz_init(a);
	if (FLONUMP(a1)) {
		mpz_set_d(a, get_c_double(a1));
	} else {
		mpz_set_str(a, get_c_string(a1), 10);
	}
}

static void lmpf_get(mpf_t a, LISP a1)
{
	mpf_init(a);
	if (FLONUMP(a1)) {
		mpf_set_d(a, get_c_double(a1));
	} else {
		mpf_set_str(a, get_c_string(a1), 10);
	}
}

#define LMPZ_ZZZ(f) \
	static LISP l##f(LISP a1, LISP b1, LISP c1) { \
		mpz_t a, b, c, p; char *s; LISP r; \
		lmpz_get(a, a1); lmpz_get(b, b1); lmpz_get(c, c1); \
		mpz_init(p); \
		f(p, a, b, c); \
		mpz_clear(a); mpz_clear(b); mpz_clear(c); \
		s = mpz_get_str(NULL, 10, p); mpz_clear(p); \
		r = strcons(strlen(s), s); free(s); \
		return r; \
	}

#define LMPZ_ZZ(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpz_t a, b, p; char *s; LISP r; \
		lmpz_get(a, a1); lmpz_get(b, b1); \
		mpz_init(p); \
		f(p, a, b); \
		mpz_clear(a); mpz_clear(b); \
		s = mpz_get_str(NULL, 10, p); mpz_clear(p); \
		r = strcons(strlen(s), s); free(s); \
		return r; \
	}

#define LMPFI_FFI(f) \
	static LISP l##f(LISP a1, LISP b1, LISP c1) { \
		mpf_t a, b; double p; \
		lmpf_get(a, a1); lmpf_get(b, b1); \
		p = f(a, b, (unsigned long)lmpz_geti(c1)); \
		mpf_clear(a); mpf_clear(b); \
		return flocons(p); \
	}

#define LMPFI_FF(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpf_t a, b; double p; \
		lmpf_get(a, a1); lmpf_get(b, b1); \
		p = f(a, b); \
		mpf_clear(a); mpf_clear(b); \
		return flocons(p); \
	}

#define LMPFI_F(f) \
	static LISP l##f(LISP a1) { \
		mpf_t a; double p; \
		lmpf_get(a, a1); \
		p = f(a); \
		mpf_clear(a); \
		return flocons(p); \
	}

static LISP mpf_to_lisp_string(mpf_t p)
{
	LISP r;
	int i;
	long bexp;
	char *q, *s = mpf_get_str(NULL, &bexp, 10, 0, p);
	mpf_clear(p);
	q = MwMalloc(strlen(s)+3);
	if (s[0] == '-') bexp++;
	for (i = 0; i < bexp; i++) q[i] = s[i];
	q[i] = '.';
	for (; s[i]; i++) q[i+1] = s[i];
	q[i+1] = '\0';
	r = strcons(strlen(q), q);
	free(s);
	free(q);
	return r;
}

#define LMPF_FF(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpf_t a, b, p; \
		lmpf_get(a, a1); lmpf_get(b, b1); \
		mpf_init(p); \
		f(p, a, b); \
		mpf_clear(a); mpf_clear(b); \
		return mpf_to_lisp_string(p); \
	}

#define LMPF_FI(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpf_t a, p; \
		lmpf_get(a, a1); \
		mpf_init(p); \
		f(p, a, (unsigned long)lmpz_geti(b1)); \
		mpf_clear(a); \
		return mpf_to_lisp_string(p); \
	}

#define LMPF_F(f) \
	static LISP l##f(LISP a1) { \
		mpf_t a, p; \
		lmpf_get(a, a1); \
		mpf_init(p); \
		f(p, a); \
		mpf_clear(a); \
		return mpf_to_lisp_string(p); \
	}

#define LMPZ_ZI(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpz_t a, p; char *s; LISP r; \
		lmpz_get(a, a1); \
		mpz_init(p); \
		f(p, a, (unsigned long)lmpz_geti(b1)); \
		mpz_clear(a); \
		s = mpz_get_str(NULL, 10, p); mpz_clear(p); \
		r = strcons(strlen(s), s); free(s); \
		return r; \
	}

#define LMPZ_RI(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpz_t a; char *s; LISP r; \
		lmpz_get(a, a1); \
		f(a, (unsigned long)lmpz_geti(b1)); \
		s = mpz_get_str(NULL, 10, a); mpz_clear(a); \
		r = strcons(strlen(s), s); free(s); \
		return r; \
	}

#define LMPZ_Z(f) \
	static LISP l##f(LISP a1) { \
		mpz_t a, p; char *s; LISP r; \
		lmpz_get(a, a1); \
		mpz_init(p); \
		f(p, a); \
		mpz_clear(a); \
		s = mpz_get_str(NULL, 10, p); mpz_clear(p); \
		r = strcons(strlen(s), s); free(s); \
		return r; \
	}

#define LMPZ_I(f) \
	static LISP l##f(LISP a1) { \
		mpz_t p; char *s; LISP r; \
		mpz_init(p); \
		f(p, (unsigned long)lmpz_geti(a1)); \
		s = mpz_get_str(NULL, 10, p); mpz_clear(p); \
		r = strcons(strlen(s), s); free(s); \
		return r; \
	}

#define LMPZI_ZI(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpz_t a; \
		double p; \
		lmpz_get(a, a1); \
		p = f(a, (unsigned long)lmpz_geti(b1)); \
		mpz_clear(a); \
		return flocons(p); \
	}

#define LMPZI_ZZ(f) \
	static LISP l##f(LISP a1, LISP b1) { \
		mpz_t a, b; \
		double p; \
		lmpz_get(a, a1); lmpz_get(b, b1); \
		p = f(a, b); \
		mpz_clear(a); mpz_clear(b); \
		return flocons(p); \
	}

#define LMPZI_Z(f) \
	static LISP l##f(LISP a1) { \
		mpz_t a; \
		double p; \
		lmpz_get(a, a1); \
		p = f(a); \
		mpz_clear(a); \
		return flocons(p); \
	}


/* Arithmetic Functions */

/*
;@mpz_add(a, b)
;@Computes a+b for integers of arbitrary size.
;@
;@
*/
LMPZ_ZZ(mpz_add)

/*
;@mpz_sub(a, b)
;@Computes a-b for integers of arbitrary size.
;@
;@
*/
LMPZ_ZZ(mpz_sub)

/*
;@mpz_mul(a, b)
;@Computes a*b for integers of arbitrary size.
;@
;@
*/
LMPZ_ZZ(mpz_mul)

/*
;@mpz_mul_2exp(op1, op2)
;@Set ROP to OP1 times 2 raised to OP2.  This operation can also be
;     defined as a left shift, OP2 steps.
;@
;@
*/
LMPZ_ZI(mpz_mul_2exp)

/*
;@mpz_neg(op)
;@Set ROP to -OP.
;@
;@
*/
LMPZ_Z(mpz_neg)

/*
;@mpz_abs(op)
;@Set ROP to the absolute value of OP.
;@
;@
*/
LMPZ_Z(mpz_abs)


/* Division and mod functions */

/*
;@mpz_tdiv_q(n, d)
;@Set Q to [N/D], truncated towards 0.
;@
;@
*/
LMPZ_ZZ(mpz_tdiv_q)

/*
;@mpz_tdiv_r(n, d)
;@Set R to (N - [N/D] * D), where the quotient is truncated towards
;     0.  Unless R becomes zero, it will get the same sign as N.
;@
;@
*/
LMPZ_ZZ(mpz_tdiv_r)

/*
;@mpz_fdiv_q(n, d)
;@Set Q to N/D, rounded towards -infinity.
;@
;@
*/
LMPZ_ZZ(mpz_fdiv_q)

/*
;@mpz_fdiv_r(n, d)
;@Set R to (N - N/D * D), where the quotient is rounded towards
;     -infinity.  Unless R becomes zero, it will get the same sign as D.
;@
;@
*/
LMPZ_ZZ(mpz_fdiv_r)

/*
;@mpz_cdiv_q(n, d)
;@Set Q to N/D, rounded towards +infinity.
;@
;@
*/
LMPZ_ZZ(mpz_cdiv_q)

/*
;@mpz_cdiv_r(n, d)
;@Set R to (N - N/D * D), where the quotient is rounded towards
;     +infinity.  Unless R becomes zero, it will get the opposite sign
;     as D.
;@
;@
*/
LMPZ_ZZ(mpz_cdiv_r)

/*
;@mpz_mod(n, d)
;@Set R to N `mod' D.  The sign of the divisor is ignored; the
;     result is always non-negative.
;@
;@
*/
LMPZ_ZZ(mpz_mod)

/*
;@mpz_divexact(n, d)
;@Set Q to N/D.  This function produces correct results only when it
;     is known in advance that D divides N.
;
;     Since mpz_divexact is much faster than any of the other routines
;     that produce the quotient (*note References:: Jebelean), it is the
;     best choice for instances in which exact division is known to
;     occur, such as reducing a rational to lowest terms.
;@
;@
*/
LMPZ_ZZ(mpz_divexact)

/*
;@mpz_tdiv_q_2exp(n, d)
;@Set Q to N divided by 2 raised to D.  The quotient is truncated
;     towards 0.
;@
;@
*/
LMPZ_ZI(mpz_tdiv_q_2exp)

/*
;@mpz_tdiv_r_2exp(n, d)
;@Divide N by (2 raised to D) and put the remainder in R.  Unless it
;     is zero, R will have the same sign as N.
;@
;@
*/
LMPZ_ZI(mpz_tdiv_r_2exp)

/*
;@mpz_fdiv_q_2exp(n, d)
;@Set Q to N divided by 2 raised to D, rounded towards -infinity.
;@
;@
*/
LMPZ_ZI(mpz_fdiv_q_2exp)

/*
;@mpz_fdiv_r_2exp(n, d)
;@Divide N by (2 raised to D) and put the remainder in R.  The sign
;     of R will always be positive.
; 
;     This operation can also be defined as masking of the D least
;     significant bits.
;@
;@
*/
LMPZ_ZI(mpz_fdiv_r_2exp)


/* Exponentiation Functions */

/*
;@mpz_powm(base, exp, mod)
;@Set ROP to (BASE raised to EXP) `mod' MOD.  If EXP is negative,
;     the result is undefined.
;@
;@
*/
LMPZ_ZZZ(mpz_powm)

/*
;@mpz_pow_ui(base, exp)
;@Set ROP to BASE raised to EXP.  The case of 0^0 yields 1.
;@
;@
*/
LMPZ_ZI(mpz_pow_ui)


/* Root Functions */

/*
;@mpz_root(op, n)
;@Set ROP to the truncated integer part of the Nth root of OP.
;     Return non-zero if the computation was exact, i.e., if OP is ROP
;     to the Nth power.
;@
;@
*/
LMPZ_ZI(mpz_root)

/*
;@mpz_sqrt(op)
;@Set ROP to the truncated integer part of the square root of OP.
;@
;@
*/
LMPZ_Z(mpz_sqrt)

/*
;@mpz_perfect_power_p(op)
;@Return non-zero if OP is a perfect power, i.e., if there exist
;     integers A and B, with B > 1, such that OP equals a raised to b.
;     Return zero otherwise.
;@
;@
*/
LMPZI_Z(mpz_perfect_power_p)

/*
;@mpz_perfect_square_p(op)
;@Return non-zero if OP is a perfect square, i.e., if the square
;     root of OP is an integer.  Return zero otherwise.
;@
;@
*/
LMPZI_Z(mpz_perfect_square_p)


/* Number Theoretic Functions */

/*
;@mpz_probab_prime_p(n, reps)
;@If this function returns 0, N is definitely not prime.  If it
;     returns 1, then N is `probably' prime.  If it returns 2, then N is
;     surely prime.  Reasonable values of reps vary from 5 to 10; a
;     higher value lowers the probability for a non-prime to pass as a
;     `probable' prime.
; 
;     The function uses Miller-Rabin's probabilistic test.
;@
;@
*/
LMPZI_ZI(mpz_probab_prime_p)

/*
;@mpz_nextprime(op)
;@Set ROP to the next prime greater than OP.
; 
;     This function uses a probabilistic algorithm to identify primes,
;     but for for practical purposes it's adequate, since the chance of
;     a composite passing will be extremely small.
;@
;@
*/
LMPZ_Z(mpz_nextprime)

/*
;@mpz_gcd(op1, op2)
;@Set ROP to the greatest common divisor of OP1 and OP2.  The result
;     is always positive even if either of or both input operands are
;     negative.
;@
;@
*/
LMPZ_ZZ(mpz_gcd)

/*
;@mpz_lcm(op1, op2)
;@Set ROP to the least common multiple of OP1 and OP2.
;@
;@
*/
LMPZ_ZZ(mpz_lcm)

/*
;@mpz_invert(op1, op2)
;@Compute the inverse of OP1 modulo OP2 and put the result in ROP.
;     Return non-zero if an inverse exist, zero otherwise.  When the
;     function returns zero, ROP is undefined.
;@
;@
*/
LMPZ_ZZ(mpz_invert)

/*
;@mpz_jacobi(op1, op2)
;@Compute the Jacobi and Legendre symbols, respectively.
;@
;@mpz_legendre
*/
LMPZI_ZZ(mpz_jacobi)

/*
;@mpz_legendre(op1, op2)
;@Compute the Jacobi and Legendre symbols, respectively.
;@
;@mpz_jacobi
*/
LMPZI_ZZ(mpz_legendre)

/*
;@mpz_remove(op, f)
;@Remove all occurrences of the factor F from OP and store the
;     result in ROP.
;@
;@
*/
LMPZ_ZZ(mpz_remove)

/*
;@mpz_fac_ui(op)
;@Set ROP to OP!, the factorial of OP.
;@
;@
*/
LMPZ_I(mpz_fac_ui)

/*
;@mpz_bin_ui(n, k)
;@Compute the binomial coefficient N over K and store the result in
;     ROP.
;@
;@
*/
LMPZ_ZI(mpz_bin_ui)

/*
;@mpz_fib_ui(n)
;@Compute the Nth Fibonacci number and store the result in ROP.
;@
;@
*/
LMPZ_I(mpz_fib_ui)


/* Comparison Functions */

/*
;@mpz_cmp(op1, op2)
;@Compare OP1 and OP2.  Return a positive value if OP1 > OP2, zero
;     if OP1 = OP2, and a negative value if OP1 < OP2.
;@
;@
*/
LMPZI_ZZ(mpz_cmp)

/*
;@mpz_cmpabs(op1, op2)
;@Compare the absolute values of OP1 and OP2.  Return a positive
;     value if OP1 > OP2, zero if OP1 = OP2, and a negative value if OP1
;     < OP2.
;@
;@
*/
LMPZI_ZZ(mpz_cmpabs)

/*
;@mpz_sgn(op)
;@Return +1 if OP > 0, 0 if OP = 0, and -1 if OP < 0.
;@
;@
*/
LMPZI_Z(mpz_sgn)


/* Logical and Bit Manipulation Functions */

/*
;@mpz_and(op1, op2)
;@Set ROP to OP1 logical-and OP2.
;@
;@
*/
LMPZ_ZZ(mpz_and)

/*
;@mpz_ior(op1, op2)
;@Set ROP to OP1 inclusive-or OP2.
;@
;@
*/
LMPZ_ZZ(mpz_ior)

/*
;@mpz_xor(op1, op2)
;@Set ROP to OP1 exclusive-or OP2.
;@
;@
*/
LMPZ_ZZ(mpz_xor)

/*
;@mpz_com(op)
;@Set ROP to the one's complement of OP.
;@
;@
*/
LMPZ_Z(mpz_com)

/*
;@mpz_popcount(op)
;@For non-negative numbers, return the population count of OP.  For
;     negative numbers, return the largest possible value (MAX_ULONG).
;@
;@
*/
LMPZI_Z(mpz_popcount)

/*
;@mpz_hamdist(op1, op2)
;@If OP1 and OP2 are both non-negative, return the hamming distance
;     between the two operands.  Otherwise, return the largest possible
;     value (MAX_ULONG).
;@
;@
*/
LMPZI_ZZ(mpz_hamdist)

/*
;@mpz_scan0(op, starting_bit)
;@Scan OP, starting with bit STARTING_BIT, towards more significant
;     bits, until the first clear bit is found.  Return the index of the
;     found bit.
;@
;@
*/
LMPZI_ZI(mpz_scan0)

/*
;@mpz_scan1(op, starting_bit)
;@Scan OP, starting with bit STARTING_BIT, towards more significant
;     bits, until the first set bit is found.  Return the index of the
;     found bit.
;@
;@
*/
LMPZI_ZI(mpz_scan1)

/*
;@mpz_setbit(rop, bit_index)
;@Set bit BIT_INDEX in ROP.
;@
;@
*/
LMPZ_RI(mpz_setbit)

/*
;@mpz_clrbit(rop, bit_index)
;@Clear bit BIT_INDEX in ROP.
;@
;@
*/
LMPZ_RI(mpz_clrbit)

/*
;@mpz_tstbit(op, bit_index)
;@Check bit BIT_INDEX in OP and return 0 or 1 accordingly.
;@
;@
*/
LMPZI_ZI(mpz_tstbit)

#if 0
/*
;XXX@mpz_urandomb(n)
;XXX@Generate a uniform random integer in the range
;     0 to 2^N - 1, inclusive.
;XXX@
;XXX@
*/
static LISP lmpz_urandomb(LISP n)
{
	mpz_t p;
	LISP r;
	char *s;
	mpz_init(p);
	mpz_urandomb(p, randstate, lmpz_geti(n));
	s = mpz_get_str(NULL, 10, p);
	mpz_clear(p);
	r = strcons(strlen(s), s);
	free(s);
	return r;
}

/*
;XXX@mpz_urandomm(n)
;XXX@Generate a uniform random integer in the range 0 to N -
;     1, inclusive.
;XXX@
;XXX@
*/
static LISP lmpz_urandomm(LISP n)
{
	mpz_t a, p;
	LISP r;
	char *s;
	mpz_init(p);
	lmpz_get(a, n);
	mpz_urandomm(p, randstate, a);
	mpz_clear(a);
	s = mpz_get_str(NULL, 10, p);
	mpz_clear(p);
	r = strcons(strlen(s), s);
	free(s);
	return r;
}

/*
;XXX@mpz_rrandomb(n)
;XXX@Generate a random integer with long strings of zeros and ones in
;     the binary representation.  Useful for testing functions and
;     algorithms, since this kind of random numbers have proven to be
;     more likely to trigger corner-case bugs.  The random number will
;     be in the range 0 to 2^N - 1, inclusive.
;XXX@
;XXX@
*/
static LISP lmpz_rrandomb(LISP n)
{
	mpz_t p;
	LISP r;
	char *s;
	mpz_init(p);
	mpz_rrandomb(p, randstate, lmpz_geti(n));
	s = mpz_get_str(NULL, 10, p);
	mpz_clear(p);
	r = strcons(strlen(s), s);
	free(s);
	return r;
}
#endif

/*
;@mpz_sizeinbase(op, base)
;@Return the size of OP measured in number of digits in base BASE.
;     The base may vary from 2 to 36.  The returned value will be exact
;     or 1 too big.  If BASE is a power of 2, the returned value will
;     always be exact.
;@
;@
*/
LMPZI_ZI(mpz_sizeinbase)


/* Floating-point Functions */

/* Arithmetic Functions */

/*
;@mpf_add(op1, op2)
;@Set ROP to OP1 + OP2.
;@
;@
*/
LMPF_FF(mpf_add)

/*
;@mpf_sub(op1, op2)
;@Set ROP to OP1 - OP2.
;@
;@
*/
LMPF_FF(mpf_sub)

/*
;@mpf_mul(op1, op2)
;@Set ROP to OP1 times OP2.
;@
;@
*/
LMPF_FF(mpf_mul)

/*
;@mpf_div(op1, op2)
;@Set ROP to OP1/OP2.
;@
;@
*/
LMPF_FF(mpf_div)

/*
;@mpf_sqrt(op)
;@Set ROP to the square root of OP.
;@
;@
*/
LMPF_F(mpf_sqrt)

/*
;@mpf_pow_ui(op1, op2)
;@Set ROP to OP1 raised to the power OP2.
;@
;@
*/
LMPF_FI(mpf_pow_ui)

/*
;@mpf_neg(op)
;@Set ROP to -OP.
;@
;@
*/
LMPF_F(mpf_neg)

/*
;@mpf_abs(op)
;@Set ROP to the absolute value of OP.
;@
;@
*/
LMPF_F(mpf_abs)

/*
;@mpf_mul_2exp(op1, op2)
;@Set ROP to OP1 times 2 raised to OP2.
;@
;@
*/
LMPF_FI(mpf_mul_2exp)

/*
;@mpf_div_2exp(op1, op2)
;@Set ROP to OP1 divided by 2 raised to OP2.
;@
;@
*/
LMPF_FI(mpf_div_2exp)


/* Comparison Functions */

/*
;@mpf_cmp(op1, op2)
;@Compare OP1 and OP2.  Return a positive value if OP1 > OP2, zero
;     if OP1 = OP2, and a negative value if OP1 < OP2.
;@
;@
*/
LMPFI_FF(mpf_cmp)

/*
;@mpf_eq(op1, op2, op3)
;@Return non-zero if the first OP3 bits of OP1 and OP2 are equal,
;     zero otherwise.  I.e., test if OP1 and OP2 are approximately equal.
;@
;@
*/
LMPFI_FFI(mpf_eq)

/*
;@mpf_reldiff(op1, op2)
;@Compute the relative difference between OP1 and OP2 and store the
;     result in ROP.
;@
;@
*/
LMPF_FF(mpf_reldiff)

/*
;@mpf_sgn(op)
;@Return +1 if OP > 0, 0 if OP = 0, and -1 if OP < 0.
;@
;@
*/
LMPFI_F(mpf_sgn)


/* Miscellaneous Functions */
/*
;@mpf_ceil(op)
;@Set ROP to OP rounded to an integer.  `mpf_ceil' rounds to the
;     next higher integer, `mpf_floor' to the next lower, and
;     `mpf_trunc' to the integer towards zero.
;@
;@mpf_floor, mpf_trunc
*/
LMPF_F(mpf_ceil)

/*
;@mpf_floor(op)
;@Set ROP to OP rounded to an integer.  `mpf_ceil' rounds to the
;     next higher integer, `mpf_floor' to the next lower, and
;     `mpf_trunc' to the integer towards zero.
;@
;@mpf_ceil, mpf_trunc
*/
LMPF_F(mpf_floor)

/*
;@mpf_trunc(op)
;@Set ROP to OP rounded to an integer.  `mpf_ceil' rounds to the
;     next higher integer, `mpf_floor' to the next lower, and
;     `mpf_trunc' to the integer towards zero.
;@
;@mpf_floor, mpf_ceil
*/
LMPF_F(mpf_trunc)

#if 0
/*
;XXX;@mpf_urandomb()
;XXX;@Generate a universally distributed random float in the interval 0
;     <= X < 1.
;XXX;@
;XXX;@
*/
static LISP lmpf_urandomb(void)
{
	mpf_t p;
	mpf_init(p);
	mpf_urandomb(p, randstate);
	return mpf_to_lisp_string(p);
}
	

/*
;XXX;@mpf_random2(max_size, max_exp)
;XXX;@Generate a random float of at most MAX_SIZE limbs, with long
;     strings of zeros and ones in the binary representation.  The
;     exponent of the number is in the interval -EXP to EXP.  This
;     function is useful for testing functions and algorithms, since
;     this kind of random numbers have proven to be more likely to
;     trigger corner-case bugs.  Negative random numbers are generated
;     when MAX_SIZE is negative.
;XXX;@
;XXX;@
*/
#endif



/* ---
*/
void init_gmp(void)
{
	mpz_t a;
	unsigned long c = 7;
	unsigned long m2exp = 1234567;
	unsigned long seed = time(NULL);

	mpz_init(a);
	mpz_set_d(a, 13);
	gmp_randinit_lc_2exp(randstate, a, c, m2exp);
	gmp_randseed_ui(randstate, seed);

	init_subr_2("mpz_add", lmpz_add);
	init_subr_2("mpz_sub", lmpz_sub);
	init_subr_2("mpz_mul", lmpz_mul);
	init_subr_2("mpz_mul_2exp", lmpz_mul_2exp);
	init_subr_1("mpz_neg", lmpz_neg);
	init_subr_1("mpz_abs", lmpz_abs);
	init_subr_2("mpz_tdiv_q", lmpz_tdiv_q);
	init_subr_2("mpz_tdiv_r", lmpz_tdiv_r);
	init_subr_2("mpz_fdiv_q", lmpz_fdiv_q);
	init_subr_2("mpz_fdiv_r", lmpz_fdiv_r);
	init_subr_2("mpz_cdiv_q", lmpz_cdiv_q);
	init_subr_2("mpz_cdiv_r", lmpz_cdiv_r);
	init_subr_2("mpz_mod", lmpz_mod);
	init_subr_2("mpz_divexact", lmpz_divexact);
	init_subr_2("mpz_tdiv_q_2exp", lmpz_tdiv_q_2exp);
	init_subr_2("mpz_tdiv_r_2exp", lmpz_tdiv_r_2exp);
	init_subr_2("mpz_fdiv_q_2exp", lmpz_fdiv_q_2exp);
	init_subr_2("mpz_fdiv_r_2exp", lmpz_fdiv_r_2exp);
	init_subr_3("mpz_powm", lmpz_powm);
	init_subr_2("mpz_pow_ui", lmpz_pow_ui);
	init_subr_2("mpz_root", lmpz_root);
	init_subr_1("mpz_sqrt", lmpz_sqrt);
	init_subr_1("mpz_perfect_power_p", lmpz_perfect_power_p);
	init_subr_1("mpz_perfect_square_p", lmpz_perfect_square_p);
	init_subr_2("mpz_probab_prime_p", lmpz_probab_prime_p);
	init_subr_1("mpz_nextprime", lmpz_nextprime);
	init_subr_2("mpz_gcd", lmpz_gcd);
	init_subr_2("mpz_lcm", lmpz_lcm);
	init_subr_2("mpz_invert", lmpz_invert);
	init_subr_2("mpz_jacobi", lmpz_jacobi);
	init_subr_2("mpz_legendre", lmpz_legendre);
	init_subr_2("mpz_remove", lmpz_remove);
	init_subr_1("mpz_fac_ui", lmpz_fac_ui);
	init_subr_2("mpz_bin_ui", lmpz_bin_ui);
	init_subr_1("mpz_fib_ui", lmpz_fib_ui);
	init_subr_2("mpz_cmp", lmpz_cmp);
	init_subr_2("mpz_cmpabs", lmpz_cmpabs);
	init_subr_1("mpz_sgn", lmpz_sgn);
	init_subr_2("mpz_and", lmpz_and);
	init_subr_2("mpz_ior", lmpz_ior);
	init_subr_2("mpz_xor", lmpz_xor);
	init_subr_1("mpz_com", lmpz_com);
	init_subr_1("mpz_popcount", lmpz_popcount);
	init_subr_2("mpz_hamdist", lmpz_hamdist);
	init_subr_2("mpz_scan0", lmpz_scan0);
	init_subr_2("mpz_scan1", lmpz_scan1);
	init_subr_2("mpz_setbit", lmpz_setbit);
	init_subr_2("mpz_clrbit", lmpz_clrbit);
	init_subr_2("mpz_tstbit", lmpz_tstbit);
	init_subr_2("mpz_sizeinbase", lmpz_sizeinbase);
	init_subr_2("mpf_add", lmpf_add);
	init_subr_2("mpf_sub", lmpf_sub);
	init_subr_2("mpf_mul", lmpf_mul);
	init_subr_2("mpf_div", lmpf_div);
	init_subr_1("mpf_sqrt", lmpf_sqrt);
	init_subr_2("mpf_pow_ui", lmpf_pow_ui);
	init_subr_1("mpf_neg", lmpf_neg);
	init_subr_1("mpf_abs", lmpf_abs);
	init_subr_2("mpf_mul_2exp", lmpf_mul_2exp);
	init_subr_2("mpf_div_2exp", lmpf_div_2exp);
	init_subr_2("mpf_cmp", lmpf_cmp);
	init_subr_3("mpf_eq", lmpf_eq);
	init_subr_2("mpf_reldiff", lmpf_reldiff);
	init_subr_1("mpf_sgn", lmpf_sgn);
	init_subr_1("mpf_ceil", lmpf_ceil);
	init_subr_1("mpf_floor", lmpf_floor);
	init_subr_1("mpf_trunc", lmpf_trunc);
#if 0
	init_subr_1("mpz_rrandomb", lmpz_rrandomb);
	init_subr_1("mpz_urandomb", lmpz_urandomb);
	init_subr_1("mpz_urandomm", lmpz_urandomm);
#endif

	mpf_set_default_prec(1024);
}

#else
#include <stdio.h>

void init_gmp(void)
{
	;
}
#endif	/* HAVE_LIBGMP */

