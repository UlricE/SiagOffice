/*
   Pathetic Writer
   Copyright (C) 1997, 1998  Ulric Eriksson <ulric@siag.nu>

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
#include <stdarg.h>
#include <ctype.h>

#include "../siod/siod.h"

#include "../common/common.h"
#include <Mowitz/MwUtils.h>

#include "pw.h"

int errorflag;


#define MAX_INTERPRETERS 10

int interpreter_count;

static struct {
	char *name;
	double (*evalfn)(void);
	void (*execfn)(char *);
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
		double (*evalfn)(void), void (*execfn)(char *))
{
	if (interpreter_count >= MAX_INTERPRETERS) return -1;
	interpreters[interpreter_count].evalfn = evalfn;
	interpreters[interpreter_count].execfn = execfn;
	interpreters[interpreter_count].name = MwStrdup(name);
	intnames[interpreter_count] = interpreters[interpreter_count].name;
	intnames[interpreter_count+1] = NULL;
	return interpreter_count++;
}

/* ---
*/
void exec_expr(int interpreter, char *expr)
{
	void (*execfn)(char *);
	if (interpreter < 0 || interpreter >= interpreter_count) return;
	execfn = interpreters[interpreter].execfn;
	if (execfn == NULL) {
		return;
	}
	execfn(expr);
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
*/
void init_interpreters(void)
{
	interpreter_count = 0;
}

