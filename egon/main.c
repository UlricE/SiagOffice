/*
   Egon Animator
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
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <locale.h>
#include <sys/stat.h>


#include "../config.h"
#include "egon.h"
#include "user_interface.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>


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

static char *egonrc;

int ok2print = 0;

int siod_interpreter;

/* ---
The following display of uncleanliness is caused by the fact that Tcl
and Guile both want to own the main loop. This kind of workaround should
not be necessary for any libraries, IMHO.
--- */

#ifdef HAVE_LIBTCL
/* ---
This function is passed to Tcl_Main and called from there.
*/

int Tcl_AppInit(Tcl_Interp *interp)
{
	init_tcl_parser(interp);
	mainloop();
	return TCL_OK;
}
#endif

/* ---
This function is passed to gh_enter and called from there.
*/

void realmain(int argc, char **argv)
{
	char b[256];
	struct stat statbuf;
	/* vars from oldmain */
	buffer *buf = NULL;
	char path[1024];
	int i;

	setlocale(LC_NUMERIC, "C");
	common_init("Egon Animator %s. No Warranty");

	sprintf(b, "%s/%ld", siag_basedir, (long)getpid());
	mkdir(b, 0700);
	sprintf(b, "%s/egon.scm", siag_basedir);
	egonrc = MwStrdup(b);

	init_interpreters();
	siod_interpreter = init_parser(argc, argv);
	init_python_parser();
	init_guile_parser();
	init_ruby_parser();
	waitforchild(0);

	init_position();
	init_cmds();

	buf = new_buffer("noname.egon", "noname.egon");

	sprintf(path, "%s/egon/egon.scm", datadir);
	if (stat(path, &statbuf)) {
		fprintf(stderr, "Can't find the runtime library (egon.scm).\n");
		fprintf(stderr, "Expected it in %s\n", path);
		fprintf(stderr, "SIAGHOME (if set) is '%s'\n", datadir);
		fprintf(stderr, "Please read installation instructions.\n");
		exit(EXIT_FAILURE);
	}

	setvar(cintern("libdir"), strcons(-1, libdir), NIL);
	setvar(cintern("datadir"), strcons(-1, datadir), NIL);
	setvar(cintern("docdir"), strcons(-1, docdir), NIL);

	/* load runtime library */
	sprintf(b, "(load \"%s/egon/egon.scm\")", datadir);
	execute(b);

	init_windows(buf, &argc, argv);
	setlocale(LC_NUMERIC, "C");	/* possibly hosed by X */

	/* load user customizations, if any */
	if (!stat(egonrc, &statbuf)) {
		sprintf(b, "(load \"%s\")", egonrc);
		execute(b);
	}

	execute("(init-windows)");
	execute("(create-menus)");

	fileio_init();
	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') {
			strcpy(path, argv[i]);
			free_buffer(w_list->buf);
			buf = new_buffer(buffer_name(argv[i]), path);
			loadmatrix(path, buf, guess_file_format(path));
			buf->change = FALSE;
			w_list->buf = buf;
		}
	}
	pr_scr_flag = TRUE;

	execute("(popup-editor)");
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
The third "main" function is this one, used by C.
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

