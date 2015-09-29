/*
   Pathetic Writer
   Copyright (C) 1997-2003  Ulric Eriksson <ulric@siag.nu>

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
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <locale.h>
#include <sys/stat.h>

#include "../config.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

#include "pw.h"

#ifdef HAVE_LIBGUILE
#include <guile/gh.h>
#endif

#ifdef HAVE_LIBTCL
#include <tcl.h>
extern int init_tcl_parser(Tcl_Interp *);
#endif

#include "../siod/siod.h"

extern int init_parser(int, char **);	/* siodi.c */
extern int init_guile_parser(void);
extern int init_ruby_parser(void);
extern int init_python_parser(void);
extern void init_interpreters(void);

static char *pwrc;

int ok2print = 0, plugin = 0;

int siod_interpreter;

extern void mainloop(void);

#ifdef HAVE_LIBTCL
int Tcl_AppInit(Tcl_Interp *interp)
{
	init_tcl_parser(interp);
	mainloop();
	return TCL_OK;
}
#endif

/* ---
*/
void realmain(int argc, char **argv)
{
	char b[256];
	struct stat statbuf;

	buffer *buf = NULL;
	char path[1024];
	int i;

	setlocale(LC_NUMERIC, "C");
	common_init("Pathetic Writer %s. No Warranty");

	sprintf(b, "%s/%ld", siag_basedir, (long)getpid());
	mkdir(b, 0700);
	sprintf(b, "%s/pw.scm", siag_basedir);
	pwrc = MwStrdup(b);

	init_interpreters();
	siod_interpreter = init_parser(argc, argv);
	init_python_parser();
	init_guile_parser();
	init_ruby_parser();
	waitforchild(0);
	MwMallocInit(NULL, 0);

	init_position();
	init_cmds();

	buf = new_buffer("noname.pw", "noname.pw");

	sprintf(path, "%s/pw/pw.scm", datadir);
	if (stat(path, &statbuf)) {
		fprintf(stderr, "Can't find the runtime library (pw.scm).\n");
		fprintf(stderr, "Expected it in %s\n", path);
		fprintf(stderr, "SIAGHOME (if set) is '%s'\n", datadir);
		fprintf(stderr, "Please read installation instructions.\n");
		exit(EXIT_FAILURE);
	}

	setvar(cintern("libdir"), strcons(-1, libdir), NIL);
	setvar(cintern("datadir"), strcons(-1, datadir), NIL);
	setvar(cintern("docdir"), strcons(-1, docdir), NIL);

	/* load runtime library */
	sprintf(b, "(load \"%s/pw/pw.scm\")", datadir);
	execute(b);

	init_windows(buf, &argc, argv);
	setlocale(LC_NUMERIC, "C");	/* again, because X hosed it */

	/* load user customizations, if any */
	if (!stat(pwrc, &statbuf)) {
		sprintf(b, "(load \"%s\")", pwrc);
		execute(b);
	}

	execute("(init-windows)");
	execute("(create-menus)");

	fileio_init();
	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') {
			strcpy(path, argv[i]);
			buf = new_buffer(buffer_name(argv[i]), path);
			loadmatrix(path, buf, guess_file_format(path));
			w_list->buf = buf;
		}
	}
	pr_scr_flag = TRUE;

	/* this works, for reasons beyond my comprehension */
	execute("(print-version)");
	execute("(print-version)");
	activate_window(w_list);
#ifdef HAVE_LIBTCL
	Tcl_Main(argc, argv, Tcl_AppInit);
#else
	mainloop();
#endif
}

/* ---
*/
int main(int argc, char **argv)
{
#ifdef HAVE_LIBGUILE
	gh_enter(argc, argv, realmain);
#else
	realmain(argc, argv);
#endif
	return 0;
}

