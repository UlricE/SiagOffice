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
 * ci.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <locale.h>

#include <sys/types.h>
#include <Mowitz/MwUtils.h>
#include "../common/common.h"

#include "calc.h"

/* C expression evaluator */
#define READCHAR (c=*calcexpr++)
#define READ_RETURN(t) return(READCHAR,(t))

#define EMPTY_STACK (opind == 0)
#define PUSH(e) (opstack[++opind] = (e))
#define POP (opstack[opind--])
#define TOP (opstack[opind])
#define POPRESSTACK (resstack[resind--])
#define PUSHRESSTACK(x) (resstack[++resind] = MwStrdup(x))

#define ISOPERATOR prio

typedef enum { ADD = 1, PLUS, ADD_ASS, INC,
        SUB, MINUS, SUB_ASS, DEC,
        MULT, MULT_ASS, POWER, DIV, DIV_ASS, IDIV,
        MOD, MOD_ASS, RSHIFT, RSHIFT_ASS,
        LSHIFT, LSHIFT_ASS, BAND, BAND_ASS,
        LAND, BOR, BOR_ASS, LOR,
        XOR, XOR_ASS, LNOT, BNOT,
        ASSIGN, PTRTOMB, MEMBER, LT,
        LE, GT, GE, EQUAL,
        NE, LPAR, RPAR, LBRACK,
        RBRACK, LBRACE, RBRACE, NUMBER,
        ROW, COLUMN, QMARK, COLON,
        COMMA, SEMICOLON, AT_SIGN, IDENT,
        SIZEOF, FOR, WHILE, IF,
        ELSE, DO, SWITCH, RETURN,
        BREAK, CONTINUE, GOTO, NOSYMBOL,
	FUN, RANGE, PERCENT, XREF,
        END
} opcode;

static char *calcexpr;			/* the expression being calculated */
static int c;			/* the last character read */

static int prio[END + 1];
static int isinfix[END + 1];

static int opind, resind;
static int opstack[40];
static char *resstack[40];
static char symbol_text[1024];

static int inarglist;		/* evaluate function arguments */

#define BREAKCHARS " +-*/%=(),;:!&|^<>?\t\r\n"

/* ---
move block between (r1,c1) and (r2,c2), direction (rd,cd)
*/

static char *update_C_references(buffer *buf, char *expr, int r1, int c1,
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

/* ---
the new scanner
*/

static int next_symbol(buffer *b)
{
	char *p;
	int lineno;
	char *endptr;
	size_t n;

Restart:	/* restart efter comment */
	while (isspace(c)) {
		if (c == '\n')
			lineno++;
		READCHAR;
	}
	switch (c) {
	case ',':
		READ_RETURN(COMMA);
	case '=':
		if (READCHAR)
			READ_RETURN(EQUAL);
		return ASSIGN;
	case '+':
		switch (READCHAR) {
		case '=':
			READ_RETURN(ADD_ASS);
		case '+':
			READ_RETURN(INC);
		default:
			return ADD;
		}
	case '-':
		switch (READCHAR) {
		case '=':
			READ_RETURN(SUB_ASS);
		case '-':
			READ_RETURN(DEC);
		case '>':
			READ_RETURN(PTRTOMB);
		default:
			return SUB;
		}
	case '*':
		switch (READCHAR) {
		case '=':
			READ_RETURN(MULT_ASS);
		case '*':
			READ_RETURN(POWER);
		default:
			return MULT;
		}
	case '/':
		switch (READCHAR) {
		case '=':
			READ_RETURN(DIV_ASS);
		case '*':
			READCHAR;
			while (c != '*' || READCHAR != '/') {
				if (c == '\0') {
					errorflag = TRUE;
					return NOSYMBOL;
				}
				if (c != '*')
					READCHAR;
			}
			READCHAR;
			goto Restart;
		default:
			return DIV;
		}
	case '\\':
		READ_RETURN(IDIV);
	case '%':
		if (READCHAR == '=')
			READ_RETURN(MOD_ASS);
		return MOD;
	case '>':
		switch (READCHAR) {
		case '>':
			if (READCHAR == '=')
				READ_RETURN(RSHIFT_ASS);
			return RSHIFT;
		case '=':
			READ_RETURN(GE);
		default:
			return GT;
		}
	case '<':
		switch (READCHAR) {
		case '<':
			if (READCHAR == '=')
				READ_RETURN(LSHIFT_ASS);
			return LSHIFT;
		case '=':
			READ_RETURN(LE);
		default:
			return LT;
		}
	case '&':
		switch (READCHAR) {
		case '=':
			READ_RETURN(BAND_ASS);
		case '&':
			READ_RETURN(LAND);
		default:
			return BAND;
		}
	case '|':
		switch (READCHAR) {
		case '=':
			READ_RETURN(BOR_ASS);
		case '|':
			READ_RETURN(LOR);
		default:
			return BOR;
		}
	case '^':
		if (READCHAR == '=')
			READ_RETURN(XOR_ASS);
		return XOR;
	case '?':
		READ_RETURN(QMARK);
	case ':':
		READ_RETURN(COLON);
	case '!':
		if (READCHAR == '=')
			READ_RETURN(NE);
		return LNOT;
	case '~':
		READ_RETURN(BNOT);
	case '(':
		READ_RETURN(LPAR);
	case ')':
		READ_RETURN(RPAR);
	case '[':
		READ_RETURN(LBRACK);
	case ']':
		READ_RETURN(RBRACK);
	case '{':
		READ_RETURN(LBRACE);
	case '}':
		READ_RETURN(RBRACE);
	case ';':
		READ_RETURN(SEMICOLON);
	case '\0':
		return END;
	case '0':
		/* hack to not treat 0.1 as octal 0 followed by error */
		if (*calcexpr == '.') {
			(void)strtod(calcexpr, &endptr);
			n = endptr-calcexpr;
			strncpy(symbol_text, calcexpr, n);
			symbol_text[n] = '\0';
		} else {
			/* this can be in octal or hexadecimal notation,
			   therefore we reencode in decimal */
			long m;
			calcexpr--;
			m = strtol(calcexpr, &endptr, 0);
			sprintf(symbol_text, "%ld", m);
		}
		if (calcexpr == endptr)
			return NOSYMBOL;
		calcexpr = endptr;
		READCHAR;
		return NUMBER;
	case '.':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		/* read decimal number */
		calcexpr--;
		(void)strtod(calcexpr, &endptr);
		if (calcexpr == endptr)	/* no conversion performed */
			return NOSYMBOL;
		n = endptr-calcexpr;
		strncpy(symbol_text, calcexpr, n);
		symbol_text[n] = '\0';
		calcexpr = endptr;
		READCHAR;
		return NUMBER;
	case '"':	/* string constant */
		p = symbol_text;
		*p++ = c;
		READCHAR;
		while (c != '"') {
			if (c == '\\') {
				*p++ = c;
				READCHAR;
			}
			if (!c) return NOSYMBOL;
			*p++ = c;
			READCHAR;
		}
		*p++ = '"';
		READCHAR;
		*p = '\0';
		return IDENT;	/* anything will do, actually */
	default:		/* read identifier or function */
		p = symbol_text;
		while (isalnum(c) ||
		       c == '_' || c == '.' || c == '@' || c == '$') {
			*p++ = c;
			READCHAR;
		}
		*p = '\0';
		if (!MwStrcasecmp(symbol_text, "R")) {
			strcpy(symbol_text, "(row)");
			return ROW;
		}
		if (!MwStrcasecmp(symbol_text, "C")) {
			strcpy(symbol_text, "(col)");
			return COLUMN;
		}
		return IDENT;	/* also ranges */
	}
	return NOSYMBOL;
}

static void oper1arg(char *p)
{
	char *tmp1 = POPRESSTACK;
	char a[2048];
	MwSnprintf(a, sizeof a, "(%s %s)", p, tmp1);
	MwFree(tmp1);
	PUSHRESSTACK(a);
}

static void oper2arg(char *p)
{
	char *tmp1 = POPRESSTACK;
	char *tmp2 = POPRESSTACK;
	char a[2048];
	MwSnprintf(a, sizeof a, "(%s %s %s)", p, tmp2, tmp1);
	MwFree(tmp1);
	MwFree(tmp2);
	PUSHRESSTACK(a);
}

static void calculate(buffer *b, int op)
{
	register char *tmp1, *tmp2;
	char a[2048];

	/* the first two are legacy ugliness which are implemented in Scheme */
	switch (op) {
	case SEMICOLON:
		oper2arg("siag_rowsum");
		break;
	case COLON:
		oper2arg("siag_colsum");
		break;
	/* comma can only appear in argument lists, *except* in expressions
	   which do not contain any function call. In that case, backwards
	   compatibility mandates that it is interpreted as get_number */
	case COMMA:
		if (!inarglist) {
			oper2arg("get_number");
		} else { /* just cons'em up */
			tmp1 = POPRESSTACK;
			tmp2 = POPRESSTACK;
			MwSnprintf(a, sizeof a, "%s %s", tmp2, tmp1);
			MwFree(tmp1);
			MwFree(tmp2);
			PUSHRESSTACK(a);
		}
		break;
	case FUN:
		tmp1 = POPRESSTACK;
		tmp2 = POPRESSTACK;
		MwSnprintf(a, sizeof a, "(%s %s)", tmp2, tmp1);
		MwFree(tmp1);
		MwFree(tmp2);
		PUSHRESSTACK(a);
		break;
	case LOR:
		oper2arg("@OR");
		break;
	case LAND:
		oper2arg("@AND");
		break;
	case BOR:
		oper2arg("bit-or");
		break;
	case XOR:
		oper2arg("bit-xor");
		break;
	case BAND:
		oper2arg("bit-and");
		break;
	case EQUAL:
		oper2arg("@EQ");	/* lives in 123.scm */
		break;
	case NE:
		oper2arg("@NE");
		break;
	case LT:
		oper2arg("@LT");
		break;
	case LE:
		oper2arg("@LE");
		break;
	case GT:
		oper2arg("@GT");
		break;
	case GE:
		oper2arg("@GE");
		break;
	case RSHIFT:
		oper1arg("-");
		oper2arg("ash");
		break;
	case LSHIFT:
		oper2arg("ash");
		break;
	case ADD:
		oper2arg("+");
		break;
	case SUB:
		oper2arg("-");
		break;
	case MULT:
		oper2arg("*");
		break;
	case POWER:
		oper2arg("pow");
		break;
	case DIV:
		oper2arg("/");
		break;
	case IDIV:
		oper2arg("quotient");
		break;
	case MOD:
		oper2arg("fmod");
		break;
	case LNOT:
		oper1arg("@NOT");
		break;
	case BNOT:
		oper1arg("bit-not");
		break;
	case PLUS:
		oper1arg("+");
		break;
	case MINUS:
		oper1arg("-");
		break;
	case PERCENT:
		tmp1 = POPRESSTACK;
		MwSnprintf(a, sizeof a, "(/ %s 100)", tmp1);
		MwFree(tmp1);
		PUSHRESSTACK(a);
		break;
	case XREF:
		oper2arg("get_xref");
		break;
	default:
		errorflag = TRUE;
	}
	if (resind < 1)
		errorflag = TRUE;
}				/* calculate */

/* ---
Non-zero return value means something went wrong
*/

static int operand(buffer *b)
{
	register int op;

      Operand:
	if (errorflag)
		return TRUE;
	op = next_symbol(b);
      Operand2:
	switch (op) {
	case RANGE:
	case IDENT:
	case NUMBER:
	case ROW:
	case COLUMN:
		PUSHRESSTACK(symbol_text);
		break;
	case RPAR:
		PUSHRESSTACK("");	/* empty arg list */
		inarglist--;
		goto Restart;	/* which will make a function call */
		break;
	case LPAR:
	case LNOT:
	case BNOT:
		PUSH(op);
		goto Operand;
	case SUB:
		PUSH(MINUS);
		goto Operand;
	case ADD:
		PUSH(PLUS);
		goto Operand;
	default:
		return errorflag = TRUE;
	}

      Operator:
	if (errorflag)
		return TRUE;
	op = next_symbol(b);
      Restart:
	if (errorflag)
		return TRUE;
	if (op == LPAR) {
		inarglist++;
		if (TOP == PERCENT) {
			TOP = MOD;
			PUSH(LPAR);
		} else {
			PUSH(FUN);
		}
		goto Operand;
	} else if (op == MOD) {
		PUSH(PERCENT);
		goto Operator;
	} else if (op == LNOT) {
		op = XREF;
		goto Restart;
	} else if (isinfix[op]) {
		if ( (EMPTY_STACK || prio[op] > prio[TOP]) ) {
			PUSH(op);
			goto Operand;
		} else {
			calculate(b, POP);
			goto Restart;
		}
	} else if (op == END) {
		while (!EMPTY_STACK) {
			if (ISOPERATOR[TOP]) {
				calculate(b, POP);
			} else {
				return errorflag = TRUE;
			}
		}
		return FALSE;
	} else if (op == RPAR) {
		if (EMPTY_STACK) {
			return errorflag = TRUE;
		}
		while (TOP != LPAR && TOP != FUN) {
			calculate(b, POP);
			if (EMPTY_STACK) {
				return errorflag = TRUE;
			}
		}
		if (POP == FUN) {
			calculate(b, FUN);
		}
		goto Operator;
	} else if (POP == PERCENT) {
		PUSH(MOD);
		goto Operand2;
	}
	return errorflag = TRUE;
}

/* End of C expression evaluator */

static cval parse_C_expr(buffer *b, char *expr, int s, int row, int col)
{
	cval value;
	calcexpr = expr;
	READCHAR;

	setlocale(LC_NUMERIC, "C");
	/* for default COMMA meaning in polish and spanish language  (Albert) */

	inarglist = 0;
	siag_type = EXPRESSION;
	opind = resind = 0;
	/* index 0 will never contain anything, it's used as a safety margin */
	operand(b);
	if (errorflag) {
		value.number = 0;
		siag_type = ERROR;
		return value;
	}
	value = parse_expr(b, siod_interpreter, resstack[1], s, row, col);
	MwFree(resstack[1]);
	return value;
}

static void exec_C_expr(char *s)
{
	if (ok2print) hide_cur(w_list);
	parse_C_expr(buffer_of_window(w_list), s, w_list->sht,
			get_point(w_list).row, get_point(w_list).col);
	if (ok2print) show_cur(w_list);
}

/* ---
*/
int init_C_parser(void)
{
	int p = 0;

	prio[LPAR] = p++;
	prio[SEMICOLON] = prio[COLON] = prio[COMMA] = p++;
	prio[LOR] = p++;
	prio[LAND] = p++;
	prio[BOR] = p++;
	prio[XOR] = p++;
	prio[BAND] = p++;
	prio[EQUAL] = prio[NE] = p++;
	prio[LT] = prio[LE] = prio[GT] = prio[GE] = p++;
	prio[RSHIFT] = prio[LSHIFT] = p++;
	prio[ADD] = prio[SUB] = p++;
	prio[MULT] = prio[DIV] = prio[MOD] = p++;
	prio[POWER] = prio[IDIV] = p++;
	prio[LNOT] = prio[BNOT] = prio[PLUS] = prio[MINUS] = p++;
	prio[PERCENT] = p++;
	prio[XREF] = p++;

	isinfix[LPAR] 	= isinfix[SEMICOLON] 	= isinfix[COLON]	= 
	isinfix[COMMA] 	= isinfix[LOR] 		= isinfix[LAND] 	= 
	isinfix[BOR] 	= isinfix[XOR] 		= isinfix[BAND] 	= 
	isinfix[EQUAL] 	= isinfix[NE] 		= isinfix[LT] 		= 
	isinfix[LE] 	= isinfix[GT] 		= isinfix[GE] 		= 
	isinfix[RSHIFT]	= isinfix[LSHIFT] 	= isinfix[ADD] 		= 
	isinfix[SUB] 	= isinfix[MULT] 	= isinfix[DIV] 		= 
	isinfix[MOD] 	= isinfix[XREF] 	/*= isinfix[BNOT]*/ 	= 
	isinfix[POWER]	= isinfix[IDIV]		=
	isinfix[PLUS] 	= isinfix[MINUS] 	= TRUE;

	return register_interpreter("C", parse_C_expr, exec_C_expr,
				update_C_references);
}

