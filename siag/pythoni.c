/*
   Siag, Scheme In A Grid
   Copyright (C) 1999-2002  Ulric Eriksson <ulric@siag.nu>

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
#ifdef HAVE_LIBPYTHON

/*
 * pythoni.c
 */

/*#define DEBUG
*/

#include "Python.h"

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

static int siag_row, siag_col;
static cval siag_result;
static buffer *siag_buffer;
static int siag_sht;

static int python_interpreter;

static PyObject *mainmod, *globals;

/* ---
This same function also does strings
*/

static cval parse_python_expr(buffer *b, char *expr, int s, int row, int col)
{
	cval nothing;

	PyObject *result;
	nothing.number = 0;

	siag_row = row;
	siag_col = col;
	siag_buffer = b;
	siag_sht = s;

MW_TRACE((stderr, "Python expression: '%s'\n", expr));

	PyErr_Clear();
	result = PyRun_String(expr, Py_eval_input, globals, globals);

	if (result == NULL) {
		siag_type = ERROR;
		siag_result.number = 0;
MW_TRACE((stderr, "Error\n"));
	} else if (PyFloat_Check(result)) {
		siag_type = EXPRESSION;
		siag_result.number = PyFloat_AsDouble(result);
MW_TRACE((stderr, "Float = %f\n", siag_result.number));
	} else if (PyLong_Check(result)) {
		siag_type = EXPRESSION;
		siag_result.number = PyLong_AsLong(result);
MW_TRACE((stderr, "Long = %f\n", siag_result.number));
	} else if (PyNumber_Check(result)) {
		siag_type = EXPRESSION;
		siag_result.number = PyInt_AsLong(result);
MW_TRACE((stderr, "Integer = %f\n", siag_result.number));
	} else if (PyString_Check(result)) {
		siag_type = STRING;
		siag_result.text = PyString_AsString(result);
MW_TRACE((stderr, "String = '%s'\n", siag_result.text));
	} else {
		siag_type = ERROR;
		siag_result.number = 0;
MW_TRACE((stderr, "Bogus value\n"));
	}

	errorflag = 0;
	return siag_result;
}

static void exec_python(char *expr)
{
	PyObject *o;

	if (ok2print)
		hide_cur(w_list);
	siag_row = get_point(w_list).row;
	siag_col = get_point(w_list).col;
	siag_buffer = buffer_of_window(w_list);
	siag_sht = w_list->sht;
	PyErr_Clear();
	o = PyRun_String(expr, Py_single_input, globals, globals);
	if (!o) llpr("Error in Python command");
	if (ok2print)
		show_cur(w_list);
}

static PyObject *siag_foo(PyObject *self, PyObject *args)
{
	return Py_BuildValue("i", 42);
}

static PyMethodDef siag_methods[] = {
	{"siag_foo", siag_foo, 1},
	{NULL, NULL, 0}
};

/* ---
*/
int init_python_parser(void)
{
	Py_Initialize();
	PyImport_AddModule("siag");
	Py_InitModule("siag", siag_methods);
	mainmod = PyImport_AddModule("__main__");
	globals = PyModule_GetDict(mainmod);
	Py_INCREF(globals);
	return python_interpreter = register_interpreter("Python",
			parse_python_expr, exec_python, NULL);
}
#else
int init_python_parser(void)
{
	return -1;
}
#endif	/* PYTHON */

