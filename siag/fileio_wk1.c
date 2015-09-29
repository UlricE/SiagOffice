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

/* ---
   980727: WK1 format support by Jon K. Hellan" <Jon.K.Hellan@item.ntnu.no>
   981030: Dirty hack to make this work with the new format code.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../siod/siod.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>
#include "calc.h"

/* ---
Saving strategy:
   We always have the fallback option to save as a floating-point
   or string constant. But it is desirable to save formulas as such
   to not lose information. In particular, a wk1 file that can be
   loaded by SIAG should also be possible to save again.
   The formula string is parsed by SIOD into list form using the
   read-from-string function. The resulting list is then passed
   into a C function, which traverses it and generates RPN code
   to be stored in the worksheet.
--- */

static LISP pre2post(LISP l)
{
	LISP p;
	if (CONSP(l)) {
		/* process all the arguments first; must also count args */
		for (p = CDR(l); NNULLP(p); p = CDR(p)) {
			pre2post(CAR(p));
		}
		if (SYMBOLP(CAR(l))) {
			/* should save the matching operator code */
			printf("operator: %s\n", get_c_string(CAR(l)));
		} else {
			printf("error: missing operator\n");
		}
	} else if (FLONUMP(l)) {
		/* simple enough */
		printf("number: %g\n", get_c_double(l));
	} else if (SYMBOLP(l)) {
		/* this will be a range, a reference or an error */
		printf("symbol: %s\n", get_c_string(l));
	} else {
		printf("error: missing operand\n");
	}
	return NIL;
}

static int col_last_changed(buffer *b, int s, int row)
{
        int i;

        if (row > b->sht[s].alloc_lines) return 0;
        for (i = b->sht[s].alloc_cols[row]; i > 1; i--)
                if (ret_type(b, s, row, i) != EMPTY)
                        break;
        return i;
}

static void writeint(unsigned char *p, int n)
{
	p[0] = n % 256;
	p[1] = n / 256;
}

static void writefloat(unsigned char *p, double x)
{
        int s = 1, e = 0, i;
        unsigned int idx, mask;
        double m;

        for (i = 0; i < 8; i++)
                p[i] = 0;
        if (x == 0) {
                return;
        }
        if (x < 0) {
                s = -1;
                x = -x;
                p[7] = 0x80;
        }
        m = frexp(x, &e);
        m *= 2;
        e--;
        e += 1023;
        m--;
        e &= 2047;
        p[7] |= (e >> 4);
        p[6] = ((e & 0x0f) << 4);
        idx = 6; mask = 8;
        for (i = 0; i < 52; i++) {
                if (!mask) {
                        idx--;
                        mask = 0x80;
                }
                m *= 2;
                if (m >= 1) {
                        p[idx] |= mask;
                        m -= 1;
                } else {
                        ;
                }
                mask >>= 1;
        }
}

static int save_cell(FILE *fp, buffer *buf, int sh, int r, int c)
{
	unsigned char b[2000], s[2000], *p;
	double num;
	int len;

	switch (ret_type(buf, sh, r, c)) {
	case LABEL:
		writeint(b, 0x0F);		/* type LABEL */
		p = (unsigned char *)ret_text(buf, sh, r, c);
		len = 1+2+2+1+strlen((char *)p)+1;
		writeint(b+2, len);
		b[4] = 0xFF;			/* format */
		writeint(b+5, c-1);		/* column */
		writeint(b+7, r-1);		/* row */
		b[9] = '\'';
		strcpy((char *)b+10, (char *)p);
		fwrite(b, 1, len+4, fp);
		break;
	case STRING:	/* treat as label */
		writeint(b, 0x0F);		/* type LABEL */
		p = (unsigned char *)ret_pvalue(NULL, buf, sh, r, c, -1);
		len = 1+2+2+1+strlen((char *)p)+1;
		writeint(b+2, len);
		b[4] = 0xFF;			/* format */
		writeint(b+5, c-1);		/* column */
		writeint(b+7, r-1);		/* row */
		b[9] = '\'';
		strcpy((char *)b+10, (char *)p);
		fwrite(b, 1, len+4, fp);
		break;
	case EXPRESSION:
	case CONSTANT:
		writeint(b, 0x0E);		/* type NUMBER */
		len = 1+2+2+8;
		writeint(b+2, len);		/* length 1+2+2+8 */
		b[4] = 0xFF;			/* format */
		writeint(b+5, c-1);		/* column */
		writeint(b+7, r-1);		/* row */
		num = ret_val(buf, sh, r, c).number;
		writefloat(b+9, num);
		sprintf((char *)s, "(scm->list %d %d)", r, c);
		execute((char *)s);
		fwrite(b, 1, len+4, fp);	/* 4 is for type and length */
		break;
	default:	/* ignore for now */
		break;
	}
	return 0;
}

static int save_wk1(char *fn, buffer *buf)
{
	int r, c;
	FILE *fp;
	static unsigned char preblurb[] =
		{0x00, 0x00, 0x02, 0x00, 0x06, 0x04};
	static unsigned char postblurb[] =
		{0x01, 0x00, 0x00, 0x00};
	int s = 0;	/* only first sheet */

	if ((fp = fopen(fn, "wb")) == NULL)
		return 1;

	if (fwrite(preblurb, 1, sizeof preblurb, fp) != sizeof preblurb) {
		fclose(fp);
		return 1;
	}
	for (r = 1; r <= line_last_used(buf, s); r++) {
		for (c = 1; c <= col_last_changed(buf, s, r); c++) {
			if (save_cell(fp, buf, s, r, c)) {
				fclose(fp);
				return 1;
			}
		}
	}
	if (fwrite(postblurb, 1, sizeof postblurb, fp) != sizeof postblurb) {
		fclose(fp);
		return 1;
	}
	fclose(fp);
	return 0;
}

/* ---
Loading strategy:
   The Lotus format stores formulas in postfix notation. It is most
   straightforward to translate this to prefix, i.e. Scheme.
   Every token is stored on a stack, in string form. Example:
   1 2 + => "1", "1" "2", "(+ 1 2)"
   So an operator is done by creating a new string from several on
   the stack.
--- */

static char *stack[10];
static int sp;

static void push(char *p)
{
	if (sp >= 10) return;
	stack[sp++] = p;
}

static char *pop(void)
{
	if (sp <= 0) return NULL;
	--sp;
	return stack[sp];
}

/* ---
Dates in wk1 format are based on day 0 = Jan 1st 1900, However, 
   files produced by Excel are 1 day off, because Excel believes that 1900
   was a leap year. I don't know if Lotus has the same bug, and choose to
   treat Excel as the norm. 
   BTW, this code breaks in 2034, if we are still using 32 bit int then.
   Jon Kåre Hellan July 23rd 1998
*/

static double from_wk1date(const double wk1date, int justtime)
{
	double rdate = wk1date;
	if (!justtime) {
		rdate -= 25569;
	}
	return (double)(rdate * 86400);
}

/* ---
borrowed from teapot
*/

static double readfloat(const unsigned char *s)
{
  double x;
  int sg,e,i;

  x=0.0;
  for (i=1; i<256; i<<=1) x=x/2.0+!!(s[0]&i);
  for (i=1; i<256; i<<=1) x=x/2.0+!!(s[1]&i);
  for (i=1; i<256; i<<=1) x=x/2.0+!!(s[2]&i);
  for (i=1; i<256; i<<=1) x=x/2.0+!!(s[3]&i);
  for (i=1; i<256; i<<=1) x=x/2.0+!!(s[4]&i);
  for (i=1; i<256; i<<=1) x=x/2.0+!!(s[5]&i);
  x=x/2.0+!!(s[6]&0x01);
  x=x/2.0+!!(s[6]&0x02);
  x=x/2.0+!!(s[6]&0x04);
  x=x/2.0+!!(s[6]&0x08);
  x=x/2.0+1.0;
  if ((e=((s[6]>>4)+((s[7]&0x7f)<<4))-1023)==-1023)
  {
    x=0.0;
    e=0;
  }
  if (s[7]&0x80) sg=-1; else sg=1;
  return (sg*ldexp(x,e));
}

static unsigned int readint(unsigned char *p)
{
	return p[0]+256*p[1];
}

static int readsint(unsigned char *p)
{
	int sign, value;

	if (p[1] & 0x80) sign = -32768;
	else sign = 0;
	value = p[0]+256*(p[1]&0x7f);
	return sign+value;
}

static int readfmt(unsigned char *p)
{
#define WK1FMT_PROT_MASK 0x80
#define WK1FMT_TYPE_MASK 0x70
#define WK1FMT_DECIMALS_MASK  0x0F
#define WK1FMT_SPEC_MASK WK1FMT_DECIMALS_MASK
#define WK1FMT_SHIFT      4
#define WK1FMT_FIXED      (0<<WK1FMT_SHIFT)
#define WK1FMT_SCIENTIFIC (1<<WK1FMT_SHIFT)
#define WK1FMT_CURRENCY   (2<<WK1FMT_SHIFT)
#define WK1FMT_PERCENT    (3<<WK1FMT_SHIFT)
#define WK1FMT_COMMA      (4<<WK1FMT_SHIFT)
#define WK1FMT_SPECIAL    (7<<WK1FMT_SHIFT)
#define WK1FMT_DDMMYY     2
#define WK1FMT_DDMM       3
#define WK1FMT_MMYY       4
#define WK1FMT_TEXT       5
#define WK1FMT_HIDDEN     6
#define WK1FMT_DATEHHMMSS 7	
#define WK1FMT_DATEHHMM   8	
#define WK1FMT_DATEINTL1  9	
#define WK1FMT_DATEINTL2  10	
#define WK1FMT_TIMEINTL1  11	
#define WK1FMT_TIMEINTL2  12	

	int format = MW_FMT_DEFAULT;
	char type, stype, decimals;

	type = *p & WK1FMT_TYPE_MASK;
	switch (type) {
	case WK1FMT_FIXED:
		format = MW_FMT_FIXED;
		decimals = *p & WK1FMT_DECIMALS_MASK;
		break;
	case WK1FMT_SCIENTIFIC:
		format = MW_FMT_SCIENTIFIC;
		decimals = *p & WK1FMT_DECIMALS_MASK;
		break;
	case WK1FMT_CURRENCY:
		format = MW_FMT_CURRENCY;
		decimals = *p & WK1FMT_DECIMALS_MASK;
		break;
	case WK1FMT_PERCENT:
		format = MW_FMT_PERCENT;
		decimals = *p & WK1FMT_DECIMALS_MASK;
		break;
	case WK1FMT_COMMA:
#ifndef MW_FMT_COMMA
#define MW_FMT_COMMA MW_FMT_DEFAULT
#endif
		format = MW_FMT_COMMA;
		decimals = *p & WK1FMT_DECIMALS_MASK;
		break;
	case WK1FMT_SPECIAL:
		stype = *p & WK1FMT_SPEC_MASK;
		switch (stype) {
		case WK1FMT_DDMMYY:
		case WK1FMT_DDMM:
		case WK1FMT_MMYY:
		case WK1FMT_DATEHHMMSS:
		case WK1FMT_DATEHHMM:
		case WK1FMT_DATEINTL1:
		case WK1FMT_DATEINTL2:
			format = MW_FMT_DATE;
			break;
		case WK1FMT_TIMEINTL1:
		case WK1FMT_TIMEINTL2:
			format = MW_FMT_TIME;
			break;
		case WK1FMT_TEXT:
			break;	/* Text */
		case WK1FMT_HIDDEN:
			break;	/* Hidden */
		default: 
			break;
		}
		break;
	default:
		break;
	}
	if (*p & WK1FMT_PROT_MASK) {
		/* Protected - ignore */
	}
		
	return format;
}

static int makecoord(int orig, int coord)
{
        if (coord & 0x8000)
                /* relative; this requires a 2-complement int */
                coord += orig;
        return coord & 0x3FFF;
}

#define CASE0(n,s,m) \
	case (n): \
		sprintf(b, "(%s)", s); \
		push(MwStrdup(b)); \
		i += (m); \
		break;

#define CASE1(n,s,m) \
	case (n): \
		p1 = pop(); \
		sprintf(b, "(%s %s)", s, p1); MwFree(p1); \
		push(MwStrdup(b)); \
		i += (m); \
		break;

#define CASE2(n,s,m) \
	case (n): \
		p1 = pop(); q1 = pop(); \
		sprintf(b, "(%s %s %s)", s, q1, p1); MwFree(p1); MwFree(q1); \
		push(MwStrdup(b)); \
		i += (m); \
		break;

#define CASE3(n,s,m) \
	case (n): \
		p1 = pop(); q1 = pop(); r1 = pop(); \
		sprintf(b, "(%s %s %s %s)", s, r1, q1, p1); \
		MwFree(p1); MwFree(q1); MwFree(r1); \
		push(MwStrdup(b)); \
		i += (m); \
		break;

#define CASEn(n,s,m) \
        case (n): \
                sprintf(b, "(%s", s); \
                addargs(b, p[i+1]); \
                strcat(b, ")"); \
                push(MwStrdup(b)); \
                i += (m); \
                break;

static void addargs(char *b, int n)
{
        if (n) {
                char *p = pop();
                addargs(b, n-1);
                strcat(b, " ");
                strcat(b, p);
        }
}

static void formula(int len, unsigned char *p, int r, int c)
{
	int i = 0;
	char b[2000];
	char *p1, *q1, *r1;

	sp = 0;

	while (i < len) {
		switch (p[i]) {
		case 0x00:
			sprintf(b, "%f", readfloat(p+i+1));
			push(MwStrdup(b));
			i += 9;
			break;
		case 0x01:
			if (a1_refs == 0) {
				sprintf(b, "r%dc%d",
                        	        1+makecoord(r, readint(p+i+3)),
                        	        1+makecoord(c, readint(p+i+1)));
			} else {
				sprintf(b, "%s%d",
       	                         	a1coord(1+makecoord(c, readint(p+i+1))),
       	                         	1+makecoord(r, readint(p+i+3)));
			}
			push(MwStrdup(b));
			i += 5;
			break;
		case 0x02:
			if (a1_refs == 0) {
				sprintf(b, "r%dc%d..r%dc%d",
	                                1+makecoord(r, readint(p+i+3)),
	                                1+makecoord(c, readint(p+i+1)),
	                                1+makecoord(r, readint(p+i+7)),
	                                1+makecoord(c, readint(p+i+5)));
			} else {
				sprintf(b, "%s%d",
       	                         	a1coord(1+makecoord(c, readint(p+i+1))),
                                	1+makecoord(r, readint(p+i+3)));
				sprintf(b+strlen(b), "..%s%d",
                                	a1coord(1+makecoord(c, readint(p+i+5))),
                                	1+makecoord(r, readint(p+i+7)));
			}
			push(MwStrdup(b));
			i += 9;
			break;
		case 0x03:
			return;
		case 0x04:
			i += 1;
			break;
		case 0x05:
			sprintf(b, "%d", readsint(p+i+1));
			push(MwStrdup(b));
			i += 3;
			break;
		case 0x06:
			i++;
			p1 = b;
			*p1++ = '"';
			while (p[i]) {
				if (p[i] == '"' || p[i] == '\\') *p1++ = '\\';
				*p1++ = p[i++];
			}
			*p1++ = '"';
			*p1 = '\0';
			push(MwStrdup(b));
			i++;
			break;
		CASE1(0x08, "-", 1);
		CASE2(0x09, "+", 1);
		CASE2(0x0A, "-", 1);
		CASE2(0x0B, "*", 1);
		CASE2(0x0C, "/", 1);
		CASE2(0x0D, "pow", 1);
		CASE2(0x0E, "@EQ", 1);
		CASE2(0x0F, "@NE", 1);
		CASE2(0x10, "@LE", 1);
		CASE2(0x11, "@GE", 1);
		CASE2(0x12, "@LT", 1);
		CASE2(0x13, "@GT", 1);
		CASE2(0x14, "bit-and", 1);
		CASE2(0x15, "bit-or", 1);
		CASE1(0x16, "@NOT", 1);
		CASE1(0x17, "+", 1);
		CASE1(0x1F, "@NA", 1);
		CASE1(0x20, "@ERR", 1);
		CASE1(0x21, "abs", 1);
		CASE1(0x22, "floor", 1);
		CASE1(0x23, "sqrt", 1);
		CASE1(0x24, "@LOG10", 1);
		CASE1(0x25, "log", 1);
		CASE0(0x26, "@PI", 1);
		CASE1(0x27, "sin", 1);
		CASE1(0x28, "cos", 1);
		CASE1(0x29, "tan", 1);
		CASE1(0x2A, "@ATAN2", 1);
		CASE1(0x2B, "atan", 1);
		CASE1(0x2C, "asin", 1);
		CASE1(0x2D, "acos", 1);
		CASE1(0x2E, "exp", 1);
		CASE2(0x2F, "@MOD", 1);
		CASE0(0x30, "@CHOOSE", 1);
		CASE0(0x31, "@ISNA", 1);
		CASE0(0x32, "@ISERR", 1);
		CASE0(0x33, "@FALSE", 1);
		CASE0(0x34, "@TRUE", 1);
		CASE0(0x35, "@RAND", 1);
		CASE0(0x36, "@DATE", 1);
		CASE0(0x37, "@TODAY", 1);
		CASE0(0x38, "@PMT", 1);
		CASE0(0x39, "@PV", 1);
		CASE0(0x3A, "@FV", 1);
		CASE3(0x3B, "@IF", 1);
		CASE0(0x3C, "@DAY", 1);
		CASE0(0x3D, "@MONTH", 1);
		CASE0(0x3E, "@YEAR", 1);
		CASE2(0x3F, "@ROUND", 1);
		CASE0(0x40, "@TIME", 1);
		CASE0(0x41, "@HOUR", 1);
		CASE0(0x42, "@MINUTE", 1);
		CASE0(0x43, "@SECOND", 1);
		CASE1(0x44, "@ISNUMBER", 1);
		CASE1(0x45, "@ISSTRING", 1);
		CASE1(0x46, "@LENGTH", 1);
		CASE1(0x47, "@VALUE", 1);
		CASE1(0x48, "@FIXED", 1);
		CASE1(0x49, "@MID", 1);
		CASE1(0x4A, "@CHR", 1);
		CASE1(0x4B, "@ASCII", 1);
		CASE1(0x4C, "@FIND", 1);
		CASE0(0x4D, "@DATEVALUE", 1);
		CASE0(0x4E, "@TIMEVALUE", 1);
		CASE0(0x4F, "@CELLPOINTER", 1);
        /* 50-54 take an additional byte for the number of args */
                CASEn(0x50, "r_sum", 2);
                CASEn(0x51, "r_avg", 2);
                CASEn(0x52, "@COUNT", 2);
                CASEn(0x53, "r_min", 2);
                CASEn(0x54, "r_max", 2);
		CASE3(0x55, "@VLOOKUP", 1);
		CASE1(0x56, "@NPV", 1);
		CASE1(0x57, "@VAR", 1);
		CASE1(0x58, "@STD", 1);
		CASE1(0x59, "@IRR", 1);
		CASE3(0x5A, "@HLOOKUP", 1);
		CASE1(0x5B, "DSUM", 1);
		CASE1(0x5C, "AVG", 1);
		CASE1(0x5D, "DCNT", 1);
		CASE1(0x5E, "DMIN", 1);
		CASE1(0x5F, "DMAX", 1);
		CASE1(0x60, "DVAR", 1);
		CASE1(0x61, "DSTD", 1);
		CASE1(0x62, "@INDEX", 1);
		CASE1(0x63, "@COLS", 1);
		CASE1(0x64, "@ROWS", 1);
		CASE1(0x65, "@REPEAT", 1);
		CASE1(0x66, "@UPPER", 1);
		CASE1(0x67, "@LOWER", 1);
		CASE0(0x68, "@LEFT", 1);
		CASE0(0x69, "@RIGHT", 1);
		CASE1(0x6A, "@REPLACE", 1);
		CASE1(0x6B, "@PROPER", 1);
		CASE1(0x6C, "@CELL", 1);
		CASE1(0x6D, "@TRIM", 1);
		CASE1(0x6E, "@CLEAN", 1);
		CASE2(0x71, "@STREQ", 1);
		CASE1(0x72, "@CALL", 1);
		CASE1(0x74, "@RATE", 1);
		CASE1(0x75, "@TERM", 1);
		CASE1(0x76, "@CTERM", 1);
		CASE1(0x77, "@SLN", 1);
		CASE1(0x78, "@SOY", 1);
		CASE1(0x79, "@DDB", 1);
		CASE1(0x9C, "@AAFSTART", 1);
		CASE1(0xCE, "@AAFUNKNOWN", 1);
		CASE1(0xFF, "@AAFEND", 1);
		default:
			return;
		}
	}
}

static void wk1(FILE *fp, buffer *buf)
{
        int i, j, sf = MwFmtNewToOld(0), f, rf;
	char b[2000];
	char *p1;
	cval value;
	int s = 0;	/* first sheet */
	buf->a1_refs = a1_refs;	/* imported doc will use A1 references */

	for (;;) {
		unsigned char head[4];
		unsigned char *p;
		int type, len;
		if (fread(head, 1, 4, fp) != 4) {
			fprintf(stderr, "Unexpected end of file\n");
			return; /* error or end */
		}
		type = readint(head);
		len = readint(head+2);
		p = MwMalloc(len);
		if (!p) {
			fprintf(stderr, "Couldn't allocate %d bytes\n", len);
			return;
		}
		if (fread(p, 1, len, fp) != len) {
			fprintf(stderr, "Couldn't read %d bytes\n", len);
			MwFree(p);
			return;
		}
		switch (type) {
		case 0x00:
			break;
		case 0x01:
			MwFree(p);
			return;
		case 0x08:
			break;
		case 0x0D:
			value.number = readsint(p+5);
			sprintf(b, "%d", readsint(p+5));
		        i = 1+readint(p+3);
			j = 1+readint(p+1);
			rf = readfmt(p); 
			f = sf | rf;
			if (rf == MW_FMT_DATE) {
				value.number 
					= from_wk1date(value.number,FALSE);
				sprintf(b, "%d", (int)value.number);
			}
			ins_data(buf, siod_interpreter, b,
				value, EXPRESSION, s, i, j);
			ins_format(buf,	s, i, j, MwFmtOldToNew(f));
			break;
		case 0x0E:
			value.number = readfloat(p+5);
			sprintf(b, "%f", value.number);
		        i = 1+readint(p+3);
			j = 1+readint(p+1);
			rf = readfmt(p); 
			f = sf | rf;
			if (rf == MW_FMT_DATE || rf == MW_FMT_TIME) {
				value.number = from_wk1date(value.number, 
							    rf == MW_FMT_TIME);
				sprintf(b, "%d", (int) value.number);
			}
			ins_data(buf, siod_interpreter, b,
				value, EXPRESSION, s, i, j);
			ins_format(buf,	s, i, j, MwFmtOldToNew(f));
			break;
		case 0x0F:
			switch (p[5]) {
			case '\'':
				break;
			case '"':
				break;
			case '^':
				break;
			default:
				break;
			}
			value.number = 0;
		        i = 1+readint(p+3);
			j = 1+readint(p+1);
			f = sf | readfmt(p); 
			ins_data(buf, siod_interpreter, (char *)p+6,
				value, LABEL, s, i, j);
			ins_format(buf,	s, i, j, MwFmtOldToNew(f));
			break;
		case 0x10:
		/* 5-12 = value */
		/* 13-14 = formula length */
		        i = readint(p+3);
			j = readint(p+1);
			f = sf | readfmt(p); 
			formula(readsint(p+13), p+15, i, j);
			p1 = pop();
			value.number = readfloat(p+5);
			ins_data(buf, siod_interpreter, p1,
				value, EXPRESSION, s, i+1, j+1);
			MwFree(p1);
			ins_format(buf,	s, i+1, j+1, MwFmtOldToNew(f));
			break;
		default:
			break;
		}
		MwFree(p);
	}
}

static int load_wk1(char *fn, buffer *buf)
{
	FILE *fp;
	if ((fp = fopen(fn, "rb")) == NULL)
		return 1;

	wk1(fp, buf);
	fclose(fp);
	return 0;
}

/* ---
File format guessing:
   1. extension .wk1 or .wks
   2. starts with the sequence 0x00 0x00 0x02 0x00
*/

static int myformat(char *fn)
{
        char *ext;
        FILE *fp;
        char b[10];

        ext = strrchr(fn, '.');
        if (!ext) return 0;     /* no extension */
        if (MwStrcasecmp(ext, ".wk1") && MwStrcasecmp(ext, ".wks"))
                return 0;       /* wrong extension */
        if ((fp = fopen(fn, "rb")) == NULL) return 0;    /* can't open */
        if (fread(b, 1, 4, fp) != 4) {		/* can't read */
		fclose(fp);
		return 0;
	}
	if (b[0] == 0 && b[1] == 0 && b[2] == 2 && b[3] == 0) {	/* gotcha */
		fclose(fp);
		return 1;
	}
        fclose(fp);
        return 0;
}

/* ---
*/
void fileio_wk1_init(void)
{
	init_subr_1("pre->post", pre2post);
        register_format(load_wk1, save_wk1, myformat, "Lotus 1-2-3 (*.wk1)");
}

