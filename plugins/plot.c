/*
   plot.c
   Copyright (C) 1999-2003  Ulric Eriksson <ulric@siag.nu>

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
The Gnuplot PBMplus driver looks promising. Gnuplot generates a PBM format
image, which can be displayed by an Image widget. The following command
makes the plot look pretty good:

set term pbm small color
set size 0.5,0.5
set output "foo.ppm"

Colour output; 320x240 pixels; small fonts.

I will probably want to add PPM support to the plugin in order to not have
to use pnmtoxpm to load the plot. That would make PBMplus mandatory; a
good thing in itself but an obstacle for people who want to start using
Siag.

Using the PBM driver for Gnuplot has the additional advantage that X11
support doesn't have to be compiled into Gnuplot.

--------

The plugin is based on the Image widget.

I had originally thought that it would be a good idea to keep Gnuplot
running and control it using a pipe, but it seems like a waste of
memory and effort. It is slightly slower but less brittle to use
a system call to run Gnuplot in batch mode when the plot needs to be
updated. One cool thing about using the Image widget is that it handles
resizing and scaling automatically. Otherwise it would have been
necessary to run Gnuplot for every update. Now it will be sufficient
to set a timeout for every resize and thus call Gnuplot only after
the resize has finished. This event handler is added to the Image:

if (timeout already set) remove timeout;
set timeout in 2 seconds;

After two seconds of inactivity, Gnuplot will be rerun and the plot
regenerated.

Here is a sample plot.cmd file:

# This file is used by Siag to control Gnuplot
set terminal pbm small color
set size 0.5,0.5
set output "plot.ppm"
set data style lines
set xtics ("" 0, "Jan" 1, "Feb" 2)
plot "1", "2", "3"

And here is a sample data file:

1200
1300
4100
11

That's all Gnuplot needs, but we want to keep more integration with the
main application. For example:

 - Where does the data come from? Knowing the range means we can get
   the data again.

 - Do we want automatic live update, and how often?

 - Is the data oriented horizontally or vertically?

One immensely elegant way to do this is to not have Siag create any files
at all except the file that controls the plugin. We can then do without
the tar system. Here is an example file:

[Control]
range=3 4 5 6
style=lines
titles=0
xtics=1
horizontal=1
update=0

[Data]
3 4 "Jan
3 5 "Feb
3 6 "Mar
4 4 1.1
4 5 1.2
4 6 1.5
5 4 2.0
5 5 1.6
5 6 1.3

The file starts with the control section, which contains pairs of the
form foo = bar. The rest of the file consists of data. Each data line
has row and column coordinates followed by data. If the data starts
with the character '"', the data is a label. Empty cells are simply
omitted.

The plugin reads this file and emits command and data files for
Gnuplot. Gnuplot runs and generates an image file which is loaded.
When the plugin exits, the command, data and image files are deleted.
This way there are no longer any temporary files left behind.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/xpm.h>
#include "../common/common.h"
#include <Mowitz/Mowitz.h>

static Pixmap pixmap, mask;
static XpmAttributes xa;

static Widget topLevel, image;

static int r0, c0, r1, c1, r2, c2, size, titles, xtics, horiz, update;
static char style[100];

enum { CELL_EMPTY = 0, CELL_STRING, CELL_NUMBER };

typedef struct {
	int type;	/* 0 for empty, 1 for string, 2 for number */
	union {
		char *p;
		double d;
	} data;
} cell;
static int size = 0;
static cell *data = NULL;

static int coords2index(int r, int c)
{
	int width = c2-c1+1;
	return (r-r1)*width+(c-c1);
}

static void ins_data(int row, int col, cell c)
{
	int n = coords2index(row, col);

	if (n >= size) return;

	data[n] = c;
}

static cell get_data(int row, int col)
{
	int n = coords2index(row, col);
	cell none;

	none.type = CELL_EMPTY;
	none.data.d = 0;

	if (n >= size) return none;

	return data[n];
}

static int write_cmd_file(char *dir)
{
	char fn[1024];
	FILE *fp;
	int r, c;
	cell ce;

	sprintf(fn, "%s/gnuplot.cmd", dir);
	fp = fopen(fn, "w");
	if (fp == NULL) return 1;

	fprintf(fp, "# This file is used by Siag to control Gnuplot\n");
	fprintf(fp, "set terminal pbm small color\n");
	fprintf(fp, "set size 0.5,0.5\n");
	sprintf(fn, "%s/gnuplot.ppm", dir);
	fprintf(fp, "set output \"%s\"\n", fn);
	fprintf(fp, "set data style %s\n", style);

	/* figure out top left corner of data */
	if (xtics) r0 = r1+1;
	else r0 = r1;
	if (titles) c0 = c1+1;
	else c0 = c1;

	/* print names of X-axis tick marks */
	if (xtics) {
		fprintf(fp, "set xtics (");
		for (c = c0; c <= c2; c++) {
			ce = get_data(r1, c);
			if (ce.type == CELL_STRING) {
				fprintf(fp, "\"%s\" ", ce.data.p);
			} else {
				fprintf(fp, "\"\" ");
			}
			fprintf(fp, "%d", c-c0);
			if (c < c2) fprintf(fp, ", ");
		}
		fprintf(fp, ")\n");
	}

	/* list the data files */
	/* not doing titles yet */
	fprintf(fp, "plot ");
	for (r = r0; r <= r2; r++) {
		FILE *fp2;
		sprintf(fn, "%s/%d", dir, r);
		fp2 = fopen(fn, "w");
		if (fp2 == NULL) {
			fclose(fp);
			return 1;
		}
		fprintf(fp, "\"%s\"", fn);
		if (r < r2) fprintf(fp, ", ");
		for (c = c0; c <= c2; c++) {
			ce = get_data(r, c);
			if (ce.type == CELL_NUMBER)
				fprintf(fp2, "%g\n", ce.data.d);
			else fprintf(fp2, "\n");
		}
		fclose(fp2);
	}
	fprintf(fp, "\n");
	fclose(fp);
	sprintf(fn, "gnuplot %s/gnuplot.cmd", dir);
	system(fn);
	return 0;
}

/* Plugin stuff */
static void cmd_win(char *p)
{
	printf("250 %lx\n", (unsigned long) XtWindow(topLevel));
}

static void cmd_quit(char *p)
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

static void cmd_prnt(char *p)
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

	/* print postscript preblurb */
	printf(" %%!PS-Adobe-2.0 EPSF-2.0\n");
	printf(" %%%%Creator: Plotting plugin for Siag Office\n");
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
	printf(" %%\n \n");
	printf(" grestore\n");
	printf(" %%%%Trailer\n");
	printf("END\n");

	for (i = 0; i < ncolors; i++) MwFree(cm[i].chars);
	MwFree(cm);
	MwFree(data);
}

/* ---
Handle exec commands.
*/

static void do_exec(char *p)
{
	char *s = strchr(p, '=');

	if (s == NULL) return;
	*s++ = '\0';
	if (!MwStrcasecmp(p, "range")) {
		int i;
		cell none;

		none.type = 0;
		none.data.d = 0;

		sscanf(s, "%d %d %d %d",
			&r1, &c1, &r2, &c2);
		size = (r2-r1+1)*(c2-c1+1);
		data = MwRealloc(data, size*sizeof(cell));
		for (i = 0; i < size; i++) data[i] = none;
	} else if (!MwStrcasecmp(p, "style")) {
		strcpy(style, s);
	} else if (!MwStrcasecmp(p, "titles")) {
		sscanf(s, "%d", &titles);
	} else if (!MwStrcasecmp(p, "xtics")) {
		sscanf(s, "%d", &xtics);
	} else if (!MwStrcasecmp(p, "horizontal")) {
		sscanf(s, "%d", &horiz);
	} else if (!MwStrcasecmp(p, "update")) {
		sscanf(s, "%d", &update);
	}
}

static void cmd_exec(char *p)
{
	do_exec(p);
	printf("200 OK\n");
}

/* ---
Load the named file. If it can't be loaded, initialize with empty data.
*/

static void load_plot(char *p)
{
	char b[1024];
	int mode = 0;	/* 0 for control, 1 for data */
	FILE *fp;

	fp = fopen(p, "r");
	if (fp == NULL) return;
	while (fgets(b, sizeof b, fp)) {
		MwChomp(b);	/* lose trailing newline */
		if (!MwStrcasecmp(b, "[control]")) {
			mode = 0;
		} else if (!MwStrcasecmp(b, "[data]")) {
			mode = 1;
		} else if (mode == 0) {
			do_exec(b);
		} else {
			int r, c;
			char d[1024];
			cell ce;
			if (sscanf(b, "%d %d %[^\n]", &r, &c, d) == 3) {
				if (d[0] == '"') {
					ce.type = CELL_STRING;
					ce.data.p = MwStrdup(d+1);
				} else {
					ce.type = CELL_NUMBER;
					ce.data.d = strtod(d, NULL);
				}
				ins_data(r, c, ce);
			}
		}
	}
}

static void cmd_load(char *p)
{
	load_plot(p);
	printf("250 OK\n");
}

static void save_plot(char *p)
{
	int row, col;
	FILE *fp = fopen(p, "w");
	if (fp == NULL) return;
	fprintf(fp, "[Control]\n");
	fprintf(fp, "range=%d %d %d %d\n", r1, c1, r2, c2);
	fprintf(fp, "style=%s\n", style);
	fprintf(fp, "titles=%d\n", titles);
	fprintf(fp, "xtics=%d\n", xtics);
	fprintf(fp, "horizontal=%d\n", horiz);
	fprintf(fp, "update=%d\n", update);
	fprintf(fp, "\n[Data]\n");
	for (row = r1; row <= r2; row++) {
		for (col = c1; col <= c2; col++) {
			cell ce = get_data(row, col);
			if (ce.type == CELL_STRING)
				fprintf(fp, "%d %d \"%s\n",
					row, col, ce.data.p);
			else if (ce.type == CELL_NUMBER)
				fprintf(fp, "%d %d %f\n",
					row, col, ce.data.d);
		}
	}
	fclose(fp);
}

static void cmd_save(char *p)
{
	save_plot(p);
	printf("250 OK\n");
}

static struct {
	char *verb;
	void (*cb) (char *);
} plugin_cmds[] = {
        {"SAVE", cmd_save},
	{"LOAD", cmd_load},
	{"EXEC", cmd_exec},
	{"WIN", cmd_win},
	{"QUIT", cmd_quit},
        {"PRNT", cmd_prnt},
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
	XtAppAddInput(XtWidgetToApplicationContext(topLevel),
		fileno(stdin), (XtPointer) XtInputReadMask,
		read_plugin_cmd, NULL);
	printf("220 Image plugin\n");
	fflush(stdout);

	XtAppMainLoop(XtWidgetToApplicationContext(topLevel));
}


/* ---
*/
int main(int argc, char **argv)
{
	Window root;
	int x, y, width, height, border, depth;
	Dimension ow, oh;
	XtAppContext ac;
	char b[1024];
	char tmpfile[1024];
	int n;
	char *fn = NULL;

	common_init(NULL);
	sprintf(tmpfile, "%s/siagimage.xpm", siag_tmpdir);

	topLevel = XtOpenApplication(&ac, "Image",
		NULL, 0, &argc, argv,
		NULL, mwApplicationShellWidgetClass, NULL, 0);

	if (!topLevel) {
		exit(1);
	}


	if (argc < 2) {
		printf("501 Bogus command line\n");
		exit(0);
	}
	fn = argv[argc-1];

	load_plot(fn);
	write_cmd_file(siag_tmpdir);

	xa.closeness = 40000;
	xa.exactColors = FALSE;
	xa.valuemask = XpmCloseness | XpmExactColors;

	sprintf(b, "%s/common/any2xpm %s/%s > %s",
		datadir, siag_tmpdir, "gnuplot.ppm", tmpfile);
	system(b);

	n = XpmReadFileToPixmap(XtDisplay(topLevel),
		DefaultRootWindow(XtDisplay(topLevel)), tmpfile,
		&pixmap, &mask, &xa);
	if (n != XpmSuccess) {
		printf("502 Can't load %s: %d\n", fn, n);
	}

	remove(tmpfile);

	XGetGeometry(XtDisplay(topLevel), pixmap, &root,
		&x, &y, &width, &height, &border, &depth);

	XGetGeometry(XtDisplay(topLevel), pixmap, &root,
		&x, &y, &width, &height, &border, &depth);

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
	XClearWindow(XtDisplay(image), XtWindow(image));

	mainloop();
	return 0;
}

