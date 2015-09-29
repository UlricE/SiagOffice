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

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <limits.h>
#include <sys/stat.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Toggle.h>
#include <X11/xpm.h>
#include <Mowitz/Mowitz.h>
#include "../common/common.h"
#include "xcommon.h"

static pid_t splashpid;

void start_splash(void)
{
	char *p = getenv("SIAG_SPLASH");

	if (p) {
		splashpid = fork();

		if (splashpid == 0) {
			char cmd[1024];
			char image[1024];
			sprintf(cmd, "%s/plugins/image", libdir);
			sprintf(image, "%s/common/bitmaps/SO3.xpm", datadir);
			execlp(cmd, "image", image, (char *)NULL);
			_exit(0);
		}
	}
}

void stop_splash(void)
{
	if (splashpid > 0) kill(splashpid, SIGINT);
}

Pixmap load_pixmap(Display *dpy, Pixel color, char *pm)
{
	return MwLoadPixmap(dpy, color, pm);
}

void theme_init(Display *display)
{
        char b[1024], *tr;
	char *p, *q;
        XrmDatabase themedb, olddb;
        XrmValue vr;
 
        sprintf(b, "%s/theme", siag_basedir);
        themedb = XrmGetFileDatabase(b);
	if (!themedb) {	/* use kde2 as default */
		sprintf(b, "%s/common/themes/theme.kde2", datadir);
		themedb = XrmGetFileDatabase(b);
	}
        if (themedb) {
                olddb = XrmGetDatabase(display);
                XrmMergeDatabases(themedb, &olddb);
                XrmSetDatabase(display, olddb);

                if (XrmGetResource(olddb, "PIXPATH", XtNstring, &tr, &vr)) {
			strcpy(b, "PIXPATH=");
			p = b+strlen(b);
			q = vr.addr;
			while (*q) {
				if (q[0] == '.' && q[1] == '.' && q[2] == '.') {
					strcpy(p, datadir);
					p += strlen(p);
					q += 2;
				} else {
					*p++ = *q;
				}
				q++;
			}
			*p = '\0';
                        putenv(MwStrdup(b));
                }
                if (XrmGetResource(olddb, "XAWM_THEME", XtNstring, &tr, &vr)) {
                        sprintf(b, "XAWM_THEME=%s", vr.addr);
                        putenv(MwStrdup(b));
                }
        }
}

static void copy_file(char *from, char *to)
{
	FILE *fp1, *fp2;
	int c;

	remove(to);
	fp1 = fopen(from, "r");
	fp2 = fopen(to, "w");
	if (!fp1 || !fp2) return;
	while ((c = fgetc(fp1)) != EOF) fputc(c, fp2);
	fclose(fp1);
	fclose(fp2);
}

int select_theme(Widget pw)
{
	char path[1024], name[1024], fn[1024];
	char fmt[80];
	char extra[1024];
	int n;

	sprintf(extra, "Home=%s", getenv("HOME"));
	sprintf(path, "%s/common/themes", datadir);
	name[0] = '\0';
	n = MwFileselInput(pw, path, name, NULL, fmt, extra, 0);
	if (!n) return 0;
	sprintf(fn, "%s/%s", path, name);
	sprintf(extra, "%s/.siag/theme", getenv("HOME"));
	copy_file(fn, extra);
	return 1;
}

