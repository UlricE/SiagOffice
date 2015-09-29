/*
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

#ifndef COMMON_H
#define COMMON_H

#include <sys/types.h>
#include <unistd.h>	/* for pid_t */

extern char *libdir;
extern char *datadir;
extern char *docdir;
extern char *siag_basedir;
extern char *siag_tmpdir;
extern char *version;
extern int from_cchar(char *);
extern void to_cchar(char *, int);
extern void common_init(char *);
extern int itsayes(char *);
extern void deletia_add(long, char *);
extern void deletia_mark(long);
extern void deletia_reap(void);
extern void waitforchild(int);
extern int tryuntar(char *, char *);
extern int only_space(char *);
extern char *plugin_basedir(char *, char *);

enum status_states {ABORT = 0, DONE, WAITING, INIT, GOLEFT, GORIGHT, GOUP, GODOWN };

#ifndef ABS
#define ABS(a) ((a)>0?(a):-(a))
#endif

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

#define sign(x) ((x)<0?-1:1)
#define todigit(c) (isdigit(c)?(c)-'0':0)

#define CONTROL_MASK 4
#define ALT_MASK 8

#define M_BIT 0x80
#define ALT(c) ((c)|M_BIT)
#define C_BIT 31
#ifndef CTRL
#define CTRL(c) ((c)&C_BIT)
#endif

#define ESC 27
#define DEL 0x7f

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define PLUGIN_LINK 1
#define PLUGIN_COPY 2

/* global variables */
extern int pr_scr_flag;		/* if the display needs updating */
extern int input_warp_pointer;		/* move cursor to input field */

int pfb2ps(FILE *, FILE *);

#endif /* COMMON_H */
