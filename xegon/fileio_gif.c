/*
   Egon Animator
   Copyright (C) 1997-2002  Ulric Eriksson <ulric@siag.nu>

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
 fileio_gif.c

 This format requires the netpbm package, particularly the "xpmtoppm"
 and "ppmtogif" programs.
 It also requires the "gifmerge" program, not part of netpbm.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/xpm.h>

#include "../egon/egon.h"
#include "xegon.h"
#include "../common/common.h"
#include "../xcommon/embed.h"
#include <Mowitz/Mowitz.h>

static int save(char *fn, buffer *buf)
{
	unsigned long now = 0;
	Widget shell, w;
	Pixmap pm;
	char tmpxpm[256];	/* for xpm file */
	char tmpgif[256];	/* for gif file */
	char *tmpgifs = 0, *p;	/* list of all gifs */
	char cmd[256];		/* for xpmtoppm | ppmtogif */
	XpmAttributes xa;
	char *docdir;

	xa.closeness = 40000;
	xa.exactColors = FALSE;
	xa.valuemask = XpmCloseness | XpmExactColors;

	shell = XtVaCreatePopupShell("shell",
		transientShellWidgetClass, topLevel, NULL);

	docdir = MwStrdup(buf->path);

	p = strrchr(docdir, '/');
	if (p) {
		*p = '\0';
	}

	w = XtVaCreateManagedWidget("stage",
		mwAnimatorWidgetClass, shell,
		XtNheight, buf->height,
		XtNwidth, buf->width,
		XtNanimatorCast, buf->sht[0].cast,
		XtNanimatorNow, now,
		XtNanimatorBgPixmap, buf->sht[0].bg,
		XtNgradient, buf->sht[0].bgrad,
		NULL);

	XtPopup(shell, XtGrabNone);

	/* start by saving one gif for time now=0 */
	for (now = 0; now <= buf->sht[0].duration; now += buf->sht[0].delta) {
		pm = MwAnimatorPixmap(w, now);
		sprintf(tmpxpm, "%s/egon-%ld.xpm", siag_tmpdir, now);
		sprintf(tmpgif, "%s/egon-%ld.gif", siag_tmpdir, now);
		if (tmpgifs == NULL) {
			tmpgifs = (char *)MwMalloc(strlen(tmpgif)+1);
			strcpy(tmpgifs, tmpgif);
		} else {
			tmpgifs = (char *)MwRealloc(tmpgifs,
						strlen(tmpgifs)+strlen(tmpgif)+2);
			strcat(tmpgifs, " ");
			strcat(tmpgifs, tmpgif);
		}
		XpmWriteFileFromPixmap(XtDisplay(w),
			tmpxpm, pm, None, &xa);
		XFreePixmap(XtDisplay(w), pm);
		sprintf(cmd, "xpmtoppm %s | ppmtogif > %s", tmpxpm, tmpgif);
		system(cmd);
		remove(tmpxpm);
	}

	p = (char *)MwMalloc(strlen(tmpgifs)+100);
	sprintf(p, "gifmerge -%d -l0 %s > %s",
		buf->sht[0].delta/10, tmpgifs, fn);
	system(p);
	sprintf(p, "rm %s", tmpgifs);
	system(p);
	XtPopdown(shell);
	XtDestroyWidget(shell);
	MwFree(tmpgifs);
	MwFree(p);
	MwFree(docdir);

	return 0;
}

#define GIF_MAGIC "GIF8"

/* ---
file format guessing:
   1. extension .gif
   2. Starts with "GIF8"
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".gif") &&
		(fp = fopen(fn, "r")) &&
		fread(b, 1, 4, fp) &&
		!memcmp(b, GIF_MAGIC, strlen(GIF_MAGIC)));
	if (fp) fclose(fp);
	return result;
}

/* ---
*/
void fileio_gif_init(void)
{
	register_format(NULL, save, myformat, "Animated GIF (*.gif)");
}

