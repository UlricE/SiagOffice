/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2003  Ulric Eriksson <ulric@siag.nu>

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

#include <Mowitz/MwUtils.h>
#include "../common/common.h"
#include <Mowitz/MwFormat.h>

#include "../config.h"
#include "../siag/calc.h"

extern void init_db(void);
extern void mainloop(void);

#ifdef HAVE_LIBGUILE
#include <guile/gh.h>
#endif

#ifdef HAVE_LIBTCL
#include <tcl.h>
#endif

#include "../siod/siod.h"

extern void init_position(void);        /* position.c */
extern int init_C_parser(void);         /* ci.c */
extern int init_parser(int, char **);   /* siodi.c */
extern int init_guile_parser(void);     /* guilei.c */
extern int init_ruby_parser(void);	/* ruby.c */
extern void init_interpreters(void);    /* railway.c */
extern void init_stocks(void);		/* stocks.c */
extern void init_gmp(void);		/* gmp.c */
extern void init_ccmath(void);		/* ccmath.c */
extern void init_mathfunc(void);	/* mathfunc.c */

static char *siagrc;

int ok2print = 0, grid_only = 0, plugin = 0;

int siod_interpreter, C_interpreter;

#ifdef HAVE_LIBTCL
extern int init_tcl_parser(Tcl_Interp *);       /* tcli.c */

/* ---
*/
int Tcl_AppInit(Tcl_Interp *interp)
{
        init_tcl_parser(interp);
        mainloop();
        return TCL_OK;  /* it won't, though */
}
#endif

extern int init_python_parser(void);

static void malloc_fail_handler(void)
{
	fprintf(stderr, "Memory allocation error.\n");

	fprintf(stderr, "(should save all changed buffers before aborting)\n");
        abort();
}


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
	P_MIN = make_position(1, 1);

	common_init("Siag (Scheme In A Grid) %s. No Warranty.");

	sprintf(b, "%s/%ld", siag_basedir, (long)getpid());
	mkdir(b, 0700);
        sprintf(b, "%s/siag.scm", siag_basedir);
        siagrc = MwStrdup(b);

        siod_interpreter = init_parser(argc, argv);
        C_interpreter = init_C_parser();
	init_python_parser();
        init_guile_parser();
	init_ruby_parser();
        init_mathwrap();        /* standard FP library functions */

        init_position();
        init_cmds();

	init_stocks();
	init_gmp();
	init_ccmath();
	init_db();
	init_mathfunc();

        buf = new_buffer("noname.siag", "noname.siag");

        /* find runtime library */
	sprintf(path, "%s/siag/siag.scm", datadir);
        if (stat(path, &statbuf)) {
		fprintf(stderr, "Can't find the runtime library (siag.scm).\n");
		fprintf(stderr, "Expected it in %s\n", path);
		fprintf(stderr, "SIAGHOME (if set) is '%s'\n", datadir);
		fprintf(stderr, "Please read installation instructions.\n");
                exit(EXIT_FAILURE);
        }

        /* must set SIAGHOME like this if SLIBU is undefined */
	setvar(cintern("libdir"), strcons(-1, libdir), NIL);
	setvar(cintern("datadir"), strcons(-1, datadir), NIL);
	setvar(cintern("docdir"), strcons(-1, docdir), NIL);

        /* now load runtime library */
        sprintf(b, "(load \"%s/siag/siag.scm\")", datadir);
        execute(b);

	/* Set some default values */
	input_warp_pointer = 1;
	kbd_macro.size = 0;
	kbd_macro.maxsize = 0;
	grid_lines = 1;

	/* now doing this after siag.scm has been loaded */
        init_windows(buf, &argc, argv);
	setlocale(LC_NUMERIC, "C");	/* chances are X hosed it */

	/* Set ok2print only after init_windows is called */
        ok2print = 1;

        /* load user customizations, if any */
        if (!stat(siagrc, &statbuf)) {
                sprintf(b, "(load \"%s\")", siagrc);
                execute(b);
        }

	/* now create menus (this should be called from the UI code) */
	execute("(create-menus)");
	/* do window initialization: keybindings, tooltips... */
	execute("(init-windows)");

        fileio_init();
        for (i = 1; i < argc; i++) {
                if (argv[i][0] != '-') {

                        strncpy(path, argv[i], 1020);
                        buf = new_buffer(buffer_name(argv[i]), path);
                        /* make it less clueless */
                        loadmatrix(path, buf, guess_file_format(path));

                        calc_matrix(buf);
                        buf->change = FALSE;
                        w_list->buf = buf;
                }
        }
        pr_scr_flag = TRUE;

        /* this works, for reasons beyond my comprehension */
        execute("(print-version)");
        execute("(print-version)");

        activate_window(w_list);

        waitforchild(0);

        /* we now have "valuable" data, so need for a better handler */
        MwMallocInit(malloc_fail_handler, 1);


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

