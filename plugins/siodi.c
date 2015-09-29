/*
   Dummy Plugin
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
 * siodi.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "../siod/siod.h"

extern void init_ss(void);
extern void init_ndbm(void);
extern void init_regex(void);
extern void init_tar(void);

/* from slib.c */
extern char *repl_c_string_arg;
extern long repl_c_string_flag;
extern LISP repl_c_string_read(void);

static void siag_puts(char *p)
{
}

static void siag_print(LISP p)
{
}

/* ---
*/
void execute(char *s)
{
	/* mostly stolen from repl_c_string in slib.c */
	struct repl_hooks h;
	long retval;
	int want_sigint = 0;
	static int want_init = 1;

	h.repl_puts = siag_puts;
	h.repl_read = repl_c_string_read;
	h.repl_eval = NULL;
	h.repl_print = siag_print;
	repl_c_string_arg = s;	/* let's hope repl_driver */
	repl_c_string_flag = 0;	/* is non-destructive */
	retval = repl_driver(want_sigint, want_init, &h);
	want_init = 0;		/* only once... */
}

/* ---
*/
int init_parser(int argc, char **argv)
{
	print_welcome();
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

	return 0;
}

