/*

   Copyright (c) 1995  Randolf Werner

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the X Consortium shall
   not be used in advertising or otherwise to promote the sale, use or
   other dealings in this Software without prior written authorization
   from the X Consortium.

 */

/*****************************************************************************/
/* Routinen zum erstellen und abarbeiten einer Fileselectbox unter X-Windows */
/*                                                                           */
/* Aufruf: 1.)  init_file_select(w);   nur einmal! aufrufen                  */
/*         2.)  file_select(patt,titel);                                     */
/*                                                                           */
/* Parameter: 1.) Widget w; Widget an das die Fileselectbox im Widgetbaum    */
/*                          angehangen wird                                  */
/*            2.) char *patt; Pattern, das die waehlbaren Dateien erfuellen  */
/*                            muessen. "" fuer letztes Pattern bzw. "*" beim */
/*                            ersten Aufruf.                                 */
/*                char *titel; Titelzeile, die oben in der Box angezeigt wird */
/* Rueckgabe: 1.) Popup fuer Fileselectbox                                   */
/*            2.) char *filename; Ausgewaehlter Dateiname als String; bei    */
/*                                einem Fehler wird NULL zureuckgeliefert;   */
/*                                es erfolgt keine Ueberpruefung, ob Datei   */
/*                                fuer den Benutzer zu oeffnen ist.          */
/*                                                                           */
/* September 1989 EWH Koblenz Karl-Heinz Staudt                              */
/*****************************************************************************/
#define MAXPATH LAENGE+LAENGE2	/* Laenge mit Pfad */
#define LAENGE 21		/* Laenge Dateiname */
#define WEITE 150		/* Breite des Directoryfensters */
#define LAENGE2 51		/* Laenge Pfadname */
#define HOEHE 300		/* Hoehe des Directoryfensters */
#define ABSTAND 8		/* Abstand zwischen den einzelnen Widgets */
#define DEFAULT_X 300		/* Vorgabe fuer X Koordinate */
#define DEFAULT_Y 300		/* Vorgabe fuer Y Koordinate */
#define MULTI_CLICK_TIME 500L
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#ifndef SVR4
#include <strings.h>
#else
#include <string.h>
#define rindex(s,c)     (strrchr(s,c))
#define index(s,c)      (strchr(s,c))
#endif
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <pwd.h>
#include <X11/IntrinsicP.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/DialogP.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Text.h>
#include <X11/StringDefs.h>

#include <Mowitz/Mowitz.h>


/*****************************************************************************/
/* Funktion liefert das aktuelle Verzeichnis                                 */
/*****************************************************************************/
void getdirectory(buf, size)
char *buf;
int size;
{
	getcwd(buf, size);
}


static Widget topLevel;

Widget init_file_select(Widget w)
{
	topLevel = w;
	return None;
}

static int select_file(char *path, char *name, char *patterns[], char *fmt)
{
	char extra[1024];
	sprintf(extra, "Home=%s",
		getenv("HOME"));
	return MwFileselInput(topLevel, path, name, patterns, fmt, extra, 0);
}

char *file_select(char *pattern, char *title)
{
	static char path[1024], name[1024];
	char fn[1024], fmt[80];
	static int need_init = 1;
	int n;

	if (need_init) {
		getcwd(path, 1024);
		need_init = 0;
		name[0] = '\0';
	}
	n = select_file(path, name, NULL, fmt);

	if (n) {
		sprintf(fn, "%s/%s", path, name);
		return MwStrdup(fn);
	}
	return NULL;
}

/*******************************************************************
 * Center a popup in the middle of another widget if possible,     *
 * otherwise make shure that the popup is visible on the screen    *
 *******************************************************************/
void centerpopup(centerin, tocenter)
Widget centerin, tocenter;
{
	Display *CurDpy;
	Position xin, yin, x, y;
	Dimension win, hin, wto, hto;
	Arg args[4];

	CurDpy = XtDisplay(centerin);
	XtRealizeWidget(tocenter);
	XtSetArg(args[0], XtNwidth, &wto);
	XtSetArg(args[1], XtNheight, &hto);
	XtGetValues(tocenter, args, 2);

	XtSetArg(args[0], XtNwidth, &win);
	XtSetArg(args[1], XtNheight, &hin);
	XtGetValues(centerin, args, 2);

	XtTranslateCoords(centerin, 0, 0, &xin, &yin);

	x = xin + win / 2 - wto / 2;
	y = yin + hin / 2 - hto / 2;
	if (x < 0)
		x = 0;
	if (y < 0)
		y = 0;
	if ((x + wto) > DisplayWidth(CurDpy, DefaultScreen(CurDpy)))
		x = DisplayWidth(CurDpy, DefaultScreen(CurDpy)) - wto;
	if ((y + hto) > DisplayHeight(CurDpy, DefaultScreen(CurDpy)))
		y = DisplayHeight(CurDpy, DefaultScreen(CurDpy)) - hto;

	XtSetArg(args[0], XtNx, x);
	XtSetArg(args[1], XtNy, y);
	XtSetValues(tocenter, args, 2);
	XWarpPointer(CurDpy, None, DefaultRootWindow(CurDpy), 0, 0, 0, 0, x + wto / 2, y + hto / 2);
}
