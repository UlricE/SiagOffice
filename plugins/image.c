/*
   image.c
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
#include <time.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/xpm.h>
#include "../common/common.h"
#include "../common/bitmaps/image.xpm"
#include <Mowitz/Mowitz.h>

typedef struct {
	Boolean plugin;
	Boolean fit;
} AppData;

static AppData app_data;

#define XtNplugin "plugin"
#define XtCPlugin "Plugin"
#define XtNfit "fit"
#define XtCFit "Fit"

static XtResource resources[] = {
	{
		XtNplugin,
		XtCPlugin,
		XtRBoolean,
		sizeof(Boolean),
		XtOffsetOf(AppData, plugin),
		XtRImmediate,
		(XtPointer)False,
	},
	{
		XtNfit,
		XtCFit,
		XtRBoolean,
		sizeof(Boolean),
		XtOffsetOf(AppData, fit),
		XtRImmediate,
		(XtPointer)False
	}
};

static XrmOptionDescRec options[] = {
	{"-plugin", "*plugin", XrmoptionNoArg, "True"},
	{"-fit", "*fit", XrmoptionNoArg, "True"}
};

static Atom wm_delete_window;
static Pixmap pixmap, mask;
static XpmAttributes xa;

static Widget topLevel, image;

/* Actions */
static void quit_action(Widget w, XEvent *e, String *p, Cardinal *n)
{
	if (!app_data.plugin) exit(0);
}

static XtActionsRec actions[] = {
	{"quit", quit_action}
};

/* Plugin stuff */
static void win(char *p)
{
	printf("250 %lx\n", (unsigned long) XtWindow(topLevel));
}

static void quit(char *p)
{
	printf("221 Over and out\n");
	exit(0);
}

/* ---
First line:
<width> <height> <ncolors> <cpp> [optional junk]

Allocate enough room to house <ncolors> colors. Each color is
a string (the <chars> below) and three shorts for RGB.

Then <ncolors> lines of this format:
<chars> {<key> <color>}+

<chars> is <cpp> characters. Then white space. Then we read tokens
two at a time until we find the key 'c'. Then <color> is looked up
using XParseColor and the resulting RGB value (*yes*! we found one!)
is stored in the color array, along with <chars>.

Then <width> lines of data. Read <cpp> characters at a time and
look up the result from the color table. Print the damn thing.
On with the next.

Then there may be extension data, which we will ignore.
--- */

typedef struct {
	char *chars;
	XColor xcolor;
} colors;

static void prnt(char *p)
{
	unsigned int width, height, depth = 8;
	unsigned int ncolors, cpp;
	int x, y, col;
	char *title = "Plugged in image";
	time_t t;
	char **data, *key, *color;
	XpmAttributes xa;
	int i, n;
	colors *cm;
	Dimension w1, h1;
	double scalew, scaleh;

	xa.valuemask = 0;

	/* figure out a thing or two */
	n = XpmCreateDataFromPixmap(XtDisplay(topLevel), &data,
		pixmap, None, &xa);
	if (n != XpmSuccess) {
		printf("503 XpmCreateDataFromPixmap returns %d\n", n);
		return;
	}
	sscanf(data[0], "%d %d %d %d", &width, &height, &ncolors, &cpp);
	cm = (colors *)MwMalloc(ncolors*sizeof(colors));

	for (i = 0; i < ncolors; i++) {
		cm[i].chars = (char *)MwMalloc(cpp);
		memcpy(cm[i].chars, data[i+1], cpp);
		key = strtok(data[i+1]+cpp, " \t");	/* skip past chars */
		color = strtok(NULL, " \t");
		while (key && color && strcmp(key, "c")) {
			key = strtok(NULL, " \t");
			color = strtok(NULL, " \t");
		};
		if (!color) {
			printf("504 No such color\n");
			return;
		}
		XParseColor(XtDisplay(topLevel),
			XDefaultColormapOfScreen(XtScreen(topLevel)),
			color, &cm[i].xcolor);
	}

	printf("200 Postscript coming right up\n");

	XtVaGetValues(image, XtNwidth, &w1, XtNheight, &h1, (char *)0);
	scalew = w1;
	scaleh = h1;
	scalew /= width;
	scaleh /= height;

	printf(" %.3f %.3f scale\n", scalew, scaleh);

	/* print postscript preblurb */
	printf(" %%!PS-Adobe-2.0 EPSF-2.0\n");
	printf(" %%%%Creator: Image plugin for Siag Office\n");
	printf(" %%%%Title: %s\n", title);
	printf(" %%%%Pages: 1\n");
	printf(" %%%%BoundingBox: %d %d %d %d\n", 0, 0, width, height);
	t = time(NULL);
	printf(" %%%%CreationDate: %s", ctime(&t));
	printf(" %%%%EndComments\n");
	printf(" %%%%EndProlog\n");
	printf(" %%%%Page: 1 1\n");
	printf(" gsave\n");
	printf(" /inch {72 mul} def\n");
	printf(" %d %d scale\n", width, height);
	printf(" /line %d string def\n", 3*width);
	printf(" %d %d %d\n", width, height, depth);
	printf(" [ %d %d %d %d %d %d ]\n", width, 0, 0, -height, 0, height);
	printf(" {currentfile line readhexstring pop}\n");
	printf(" false 3 colorimage\n");

	/* print all the pixels */
	col = 0;
	printf(" ");
	for (y = 0; y < height; y++) {
		char *line = data[y+ncolors+1];
		for (x = 0; x < width; x++) {
			char *pix = line+cpp*x;
			for (i = 0; i < ncolors; i++)
				if (!memcmp(cm[i].chars, pix, cpp)) break;
			if (i == ncolors) i = 0;
			printf("%02hx%02hx%02hx",
				(cm[i].xcolor.red / 256) & 255,
				(cm[i].xcolor.green / 256) & 255,
				(cm[i].xcolor.blue / 256) & 255);
			col += 6;
			if (col >= 72) {
				printf("\n ");
				col = 0;
			}
		}
	}
	if (col) printf("\n");

	/* print postscript postblurb */
	printf(" %%\n");
	printf(" grestore\n");
	printf("END\n");

	for (i = 0; i < ncolors; i++) MwFree(cm[i].chars);
	MwFree(cm);
	MwFree(data);
}

static struct {
	char *verb;
	void (*cb) (char *);
} plugin_cmds[] = {

/*        {"SAVE", save},
   {"LOAD", load_},
   {"EXEC", exec_},
   {"HELP", help},
   {"NOOP", noop},
 */
	{"WIN", win},
	{"QUIT", quit},
        {"PRNT", prnt},
	{ NULL, NULL }
};

static void read_plugin_cmd(XtPointer client_data, int *fid, XtInputId * id)
{
	char b[1024], *p;
	int i, n;

	if ((n = read(*fid, b, 1020)) == -1)
		return;

	b[n] = '\0';
	if ((p = strchr(b, '\n')) == NULL) {
		printf("501 Incomplete command\n");
		fflush(stdout);
		return;
	}
	*p = '\0';
	for (i = 0; plugin_cmds[i].verb; i++) {
		if (!strncmp(b, plugin_cmds[i].verb,
			     strlen(plugin_cmds[i].verb)))
			break;
	}
	if (plugin_cmds[i].verb)
		(*plugin_cmds[i].cb) (b+strlen(plugin_cmds[i].verb)+1);
	else
		printf("500 What are you talking about\n");
	fflush(stdout);
}

/* ---
*/
void mainloop(void)
{
	if (app_data.plugin) {
		XtAppAddInput(XtWidgetToApplicationContext(topLevel),
			fileno(stdin), (XtPointer) XtInputReadMask,
			read_plugin_cmd, NULL);
		printf("220 Image plugin\n");
		fflush(stdout);
	}

	XtAppMainLoop(XtWidgetToApplicationContext(topLevel));
}


/* ---
*/
int main(int argc, char **argv)
{
	Window root;
	int x, y, width, height, rw, rh, border, depth;
	Dimension ow, oh;
	XtAppContext ac;
	char b[1024];
	char tmpfile[1024];
	int n;
	char *fn = NULL;

	common_init(NULL);
	sprintf(tmpfile, "%s/siagimage.xpm", siag_tmpdir);

	topLevel = XtVaOpenApplication(&ac, "Image",
		options, XtNumber(options),
		&argc, argv,
		NULL,	/* missing ad */
		mwApplicationShellWidgetClass,
		(char *)0);

	if (!topLevel) {
		exit(1);
	}

	XtGetApplicationResources(topLevel, &app_data, resources,
			XtNumber(resources), NULL, 0);

	XtAppAddActions(ac, actions, XtNumber(actions));

	if (argc < 2) {
		printf("501 Bogus command line\n");
		exit(0);
	}
	fn = argv[argc-1];

	xa.closeness = 40000;
	xa.exactColors = FALSE;
	xa.valuemask = XpmCloseness | XpmExactColors;

	sprintf(b, "%s/common/any2xpm \"%s\" > \"%s\"",
		datadir, fn, tmpfile);
	system(b);

	n = XpmReadFileToPixmap(XtDisplay(topLevel),
		DefaultRootWindow(XtDisplay(topLevel)), tmpfile,
		&pixmap, &mask, &xa);
	if (n != XpmSuccess) {
		printf("502 Can't load %s: %d\n", fn, n);
		/*exit(0);*/
	}

	remove(tmpfile);

	XGetGeometry(XtDisplay(topLevel), pixmap, &root,
		&x, &y, &width, &height, &border, &depth);

	XGetGeometry(XtDisplay(topLevel), root, &root,
		&x, &y, &rw, &rh, &border, &depth);

	XGetGeometry(XtDisplay(topLevel), pixmap, &root,
		&x, &y, &width, &height, &border, &depth);

	/* scale to fit */
	if (app_data.fit && (width > rw || height > rh)) {
		double dw = (double)width/(double)rw;
		double dh = (double)(height+30)/(double)rh;
		double d = (dw > dh)?dw:dh;
		width /= d;
		height /= d;
	}

	XtVaGetValues(topLevel,
		XtNwidth, &ow,
		XtNheight, &oh,
		(char *)0);

	if (ow == 0 || oh == 0) {
		ow = width;
		oh = height;
	}

	XtVaSetValues(topLevel,
		XtNwidth, ow,
		XtNheight, oh,
		XtNborderWidth, 0,
		(char *) 0);
	image = XtVaCreateManagedWidget("image",
		mwImageWidgetClass, topLevel,
		XtNbitmap, pixmap,
		(char *)0);

	XtRealizeWidget(topLevel);
	MwSetIcon(topLevel, image_xpm);
	wm_delete_window = XInternAtom(XtDisplay(topLevel),
				"WM_DELETE_WINDOW", False);
	XtOverrideTranslations(topLevel,
		XtParseTranslationTable(
			"<Message>WM_PROTOCOLS: quit()"));
	XSetWMProtocols(XtDisplay(topLevel), XtWindow(topLevel),
		&wm_delete_window, 1);

	XtOverrideTranslations(image,
		XtParseTranslationTable(
			"<Key>q: quit()"));

	XClearWindow(XtDisplay(image), XtWindow(image));

	mainloop();
	return 0;
}

