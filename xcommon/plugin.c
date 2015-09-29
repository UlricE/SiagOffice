/*
    Copyright (C) 1996-2001  Ulric Eriksson <ulric@siag.nu>

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
plugin.c

The protocol runs over two pipes, connected to the plugin's
stdin and stdout:

Upon startup, the plugin prints an identification message of
the following form:

200 text

This line, like every line, is terminated by a single newline
character, *not* a CRLF sequence.

A result code starting with the digit '2' indicates successful
startup. Any other result code is taken as a failure; in particular,
a non-digit suggests that this is not a plugin at all!

Communication takes place in a command-reply fashion. The command
consists of a single line of no more that 1024 characters, starting
with a command verb and optionally followed by parameters. Commands
are case sensitive. These commands are implemented here:

SAVE filename	Save data in file
LOAD filename	Load data from file
EXEC string	Execute the string, which should be interpretable
		by the plugin
HELP		List available commands
NOOP		Do nothing
WIN		Print the window id of window to be captured
EWIN		Tell the plugin where to send events
PH		Tell the plugin its handle
QUIT		Quit
PRNT		Produce Postscript representation of the plugin

The reply consists of a numeric result code, the character
' ' followed by text:

200 text	Command completed successfully
400 text	Command failed
500 text	Command unknown

The plugin *must reply* even if the command cannot be executed.
Otherwise there will be an annoying timeout when the main
program waits for the reply.

This implementation does *not* handle multi-line replies and will
likely choke on them. I'm just testing.


The typical use of the plugin library will be like this:

1. plugin_start: The plugin is loaded into the buffer but not displayed.
   The application runs and displays its toplevel window outside Siag.
   The toplevel window is found.
   The geometry is saved.
   The window is unmapped.
   The window ID is saved.
2. In activate_window, every plugin in the buffer is checked to see
   if it needs to be displayed.
3. plugin_show: A new widget is created in the grid, using the
   geometry information retrieved before.
   The plugin toplevel is reparented into the widget and then mapped.
4. plugin_hide: The toplevel window is unmapped and the widget
   destroyed.
5. plugin_stop: plugin is hidden, QUIT is sent and pipes are closed.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <X11/xpm.h>
#include "../common/common.h"
#include <Mowitz/Mowitz.h>
#include "plugin.h"

#define REPARENT_LOOPS 50
#define PLUGIN_MAX 30

/*#define DEBUG
*/

char **plugin_patterns;

static struct {
	int pfd[2], qfd[2];
	int taken;
	int visible;
	pid_t pid;
	Window victim;
	Position x, y;
	Dimension width, height, border, depth;
	Widget parent, core;	/* keeper of new parent window */
} plugin[PLUGIN_MAX];

static int nplugin = 0;

static struct {
	char *desc, *ext, *cmd;
} *handler;

static int nhandler = 0;

static Widget topLevel = None;
static Cursor sane_cursor, lr_cursor;

static Atom plugin_protocol;
static void (*plugin_execute)(char *) = NULL;

/* ---
write a line to the plugin
*/

int plugin_write(int ph, char *b)
{
	int retval = write(plugin[ph].pfd[1], b, strlen(b));

	return retval;
}

/* ---
read a line from the plugin, return number of chars read
*/

int plugin_read(int ph, char *b)
{
	fd_set rfds;
	struct timeval tv;
	int retval, n;

	FD_ZERO(&rfds);
	FD_SET(plugin[ph].qfd[0], &rfds);
	tv.tv_sec = 30;
	tv.tv_usec = 0;
	retval = select(plugin[ph].qfd[0]+1, &rfds, NULL, NULL, &tv);
	if (!retval) return 0;
#if 1
	for (n = 0; n < 1024; n++) {
		if (!read(plugin[ph].qfd[0], b+n, 1)) break;
		if (b[n] == '\n') {
			n++;
			break;
		}
	}
#else
	n = read(plugin[ph].qfd[0], b, 1024);
#endif
	b[n] = '\0';

	return n;
}

static char **parse_cmd(char *cmd, char *fn, char *name)
{
	int i = 0;
	char **argv = (char **)MwMalloc(10*sizeof(char *));
	char *cmd_copy = MwStrdup(cmd);
	char *p;

	for (p = strtok(cmd_copy, " \n"); p && i < 9; p = strtok(NULL, " \n")) {
		if (p[0] == '%') {
			switch (p[1]) {
			case 's':
				argv[i++] = MwStrdup(fn);
				break;
			case 'W':
				argv[i++] = MwStrdup(name);
				break;
			case '%':
				argv[i++] = p+1;
				break;
			default:
				;
			}
		} else {
			argv[i++] = p;
		}
	}
	argv[i] = NULL;
	return argv;
}

static int plugin_handler_find(char *ext)
{
	int i;

	for (i = 0; i < nhandler; i++)
		if (!strcmp(ext, handler[i].ext)) return i;

	return -1;
}

/* ---
Run a plugin without displaying it.

fn: File name of file to be loaded. The extension is used to
    choose from available plugins. All the plumbing for
    communicating with the plugin is set up.

Returns a plugin handle for success of -1 for failure.
*/

int plugin_start(char *fn)
{
	int x, y;
	unsigned int width, height, border, depth;
	Window victim, root;
	int ph, i;
	char *cmd;
	char *ext = strrchr(fn, '.');

	if (!ext) ext = fn;
	else ext++;
	i = plugin_handler_find(ext);

	if (i == -1) return -1;	/* no handler found */

	for (ph = 0; ph < nplugin; ph++)
		if (!plugin[ph].taken) break;
	if (ph == nplugin) nplugin++;
	if (nplugin >= PLUGIN_MAX) return -1;	/* too many */
	cmd = handler[i].cmd;
	plugin[ph].taken = 1;
	if (pipe(plugin[ph].pfd) == -1) return -1;
	if (pipe(plugin[ph].qfd) == -1) return -1;
	plugin[ph].pid = fork();
	if (plugin[ph].pid == -1) {		/* error */
		nplugin--;
		return -1;
	} else if (plugin[ph].pid == 0) {	/* child */
		char **argv;
		close(plugin[ph].pfd[1]);
		dup2(plugin[ph].pfd[0], 0);
		close(plugin[ph].pfd[0]);
		close(plugin[ph].qfd[0]);
		dup2(plugin[ph].qfd[1], 1);
		close(plugin[ph].qfd[1]);
		/* run the plugin */
		argv = parse_cmd(cmd, fn, "SiAg_PlUgIn");
		execvp(argv[0], argv);
		exit(EXIT_FAILURE);
	} else {				/* parent */
		char r[1024];
		int n;
		close(plugin[ph].pfd[0]);
		close(plugin[ph].qfd[1]);
		n = 1;
		while (n) {
			n = sleep(n);
		}
		if (plugin_read(ph, r) < 6 || r[0] != '2') {
			return -1;
		}
		sprintf(r, "EWIN %lx\n", (unsigned long)XtWindow(topLevel));
		plugin_write(ph, r);
		if (plugin_read(ph, r) < 2) {
			/* it doesn't matter if the plugin returns error */
			return -1;
		}
		sprintf(r, "PH %d\n", ph);
		plugin_write(ph, r);
		if (plugin_read(ph, r) < 2) {
			return -1;
		}
		plugin_write(ph, "WIN\n");
		if (plugin_read(ph, r) < 6 || r[0] != '2') {
			return -1;
		}
		victim = strtol(r+4, NULL, 16);
		if (victim == None) {
			plugin[ph].taken = 0;
			return -1;
		}

	        XGetGeometry(XtDisplay(topLevel), victim, &root,
                        &x, &y, &width, &height, &border, &depth);

		plugin[ph].width = width+2*border;
		plugin[ph].height = height+2*border;
		plugin[ph].border = border;
		plugin[ph].depth = depth;
		plugin[ph].victim = victim;
		plugin[ph].visible = 1;
		plugin[ph].parent = None;
		plugin[ph].core = None;
		plugin[ph].x = 0;	/* unused */
		plugin[ph].y = 0;
		plugin_hide(ph);
	}
	return ph;
}

/* ---
Resize a plugin.

ph: Plugin handle
wi: New width
he: New height

Returns 0 for success or non-0 for failure.
*/

int plugin_size_set(int ph, int width, int height)
{
	if (width < 10 || width > 1000 ||
		height < 10 || height > 1000) return -1;
	plugin[ph].width = width;
	plugin[ph].height = height;
	if (plugin[ph].visible && plugin[ph].core != None) {
		XtResizeWidget(plugin[ph].core, width+6, height+6, 1);
		XResizeWindow(XtDisplay(plugin[ph].core),
			plugin[ph].victim, width, height);
	}
	return 0;
}


/* ---
Kill a plugin. Close the pipes, close the window, kill the process.

ph: Plugin handle.

Returns 0 for success or non-0 for failure.
*/

int plugin_stop(int ph)
{
	char b[1024];

	plugin_hide(ph);
	plugin_write(ph, "QUIT\n");
	plugin_read(ph, b);
	close(plugin[ph].pfd[1]);
	close(plugin[ph].qfd[0]);
	kill(plugin[ph].pid, SIGINT);
	plugin[ph].taken = 0;
	return 0;
}

/* ---
Tell the plugin to load a file.

ph: Plugin handle
fn: File name

Returns 0 for success or non-0 for failure.
*/

int plugin_load(int ph, char *fn)
{
	char cmd[1024];

	sprintf(cmd, "LOAD %s\n", fn);
	plugin_write(ph, cmd);
	plugin_read(ph, cmd);	/* ignore the response for now */
	return 0;
}

/* ---
Tell the plugin to save to a file.

ph: plugin handle
fn: File name

Returns 0 for success or non-0 for failure.
*/

int plugin_save(int ph, char *fn)
{
	char cmd[1024];

	sprintf(cmd, "SAVE %s\n", fn);
	plugin_write(ph, cmd);
	plugin_read(ph, cmd);	/* ignore the response for now */
	return 0;
}

/* ---
Ask the plugin what commands it understands.

ph: plugin handle

Returns a one-line string with the available commands.
Caller must free.
*/

char *plugin_help(int ph)
{
	char cmd[1024];

	plugin_write(ph, "HELP\n");
	cmd[0] = '\0';
	plugin_read(ph, cmd);
	return MwStrdup(cmd);
}

/* ---
Find the handle of a plugin running under a widget.
*/

int plugin_find_by_widget(Widget w)
{
	int ph;

	for (ph = 0; ph < nplugin; ph++)
		if (plugin[ph].core == w) return ph;

	return -1;
}

/* ---
Find the handle of a plugin running in a window.
*/

int plugin_find_by_window(Window w)
{
	int ph;

	for (ph = 0; ph < nplugin; ph++)
		if (plugin[ph].victim == w) return ph;

	return -1;
}

/* a callback to tell the app about the change */
static void (*handle_plugin_exit)(int) = NULL;

static void handle_destroy(Widget w, XtPointer p, XEvent *event, Boolean *b)
{
	if (event->type == DestroyNotify) {
		XDestroyWindowEvent dwevent = event->xdestroywindow;
		int ph = plugin_find_by_window(dwevent.window);
		if (ph >= 0) {
			plugin[ph].taken = 0;
			if (plugin[ph].core != None)
				XtDestroyWidget(plugin[ph].core);
			if (handle_plugin_exit)
				(*handle_plugin_exit)(ph);
		}
	}
}

/* ---
Display the plugin, i.e. map the window and reparent if necessary.

win: window pointer
ph: plugin handle

Return 0 for success or non-0 for failure.
*/

int plugin_show(int ph, Widget grid)
{
        Display *display = XtDisplay(grid);
        Window w = plugin[ph].victim;
        Window to;
	int i;
	Cardinal n;
	Window root, parent, *child;

	plugin[ph].parent = grid;
	if (plugin[ph].visible) plugin_hide(ph);
	plugin[ph].visible = 1;
	/* we don't need to tell Table where to put the widget,
	   because it will be picked up from plugin_coordinates */
	plugin[ph].core = XtVaCreateManagedWidget("plugin-parent",
		compositeWidgetClass, grid,
		XtNwidth, plugin[ph].width+6,
		XtNheight, plugin[ph].height+6,
		XtNborderWidth, 1,
		(char *)0);
        to = XtWindow(plugin[ph].core);
        XSync(display, False);
        for (i = 0; i < REPARENT_LOOPS; i++) {
                XReparentWindow(display, w, to, 0, 0);
                XQueryTree(display, w, &root, &parent, &child, &n);
                XSync(display, False);
        }
	XMoveWindow(display, w, 2, 2);
        XMapWindow(display, w);
        XSync(display, False);
	XtAddEventHandler(plugin[ph].core,
		SubstructureNotifyMask, False,
		handle_destroy, (XtPointer)XtWindow(plugin[ph].core));
/*	XSelectInput(display, w,
		SubstructureNotifyMask |
		FocusChangeMask |
		LeaveWindowMask |
		EnterWindowMask |
		KeyPressMask |
		KeyReleaseMask |
		PropertyChangeMask |
		ColormapChangeMask);
*/
	XDefineCursor(display, w, sane_cursor);

	return plugin_size_set(ph, plugin[ph].width, plugin[ph].height);
}

/* ---
Hide the plugin, i.e. unmap the window.

ph: Plugin handle

Returns 0 for success or non-0 for failure.
*/

int plugin_hide(int ph)
{
	int i;
	XWMHints *hints;
	Display *display = XtDisplay(topLevel);
	Screen *screen = XtScreen(topLevel);
	Window w = plugin[ph].victim;

	XSync(display, False);
	XWithdrawWindow(display, w, XScreenNumberOfScreen(screen));
        XSync(display, False);
        hints = XGetWMHints(display, w);
        hints->flags |= WindowGroupHint;
        hints->window_group = RootWindowOfScreen(screen);
        XSetWMHints(display, w, hints);
#if 1	/* testing */
	for (i = 0; i < REPARENT_LOOPS; i++) {
		Window root, parent, *child;
		Cardinal n;
		XQueryTree(display, w, &root, &parent, &child, &n);
		XReparentWindow(display, w, root, 0, 0);
		XSync(display, False);
	}
#endif
	if (plugin[ph].core != None) {
		XtDestroyWidget(plugin[ph].core);
		plugin[ph].core = None;
	}
	return 0;
}


typedef struct {
        char *chars;
        XColor xcolor;
} colors;

/* ---
Clever little hack to print anything, including plugins that don't know
how to print themselves. Works the following way:
1. Create override widget of the right size.
2. Reparent plugin to override widget.
3. Send expose event to plugin.
4. Copy override widget to pixmap.
5. Reparent plugin back to main application.
6. Destroy override widget.
7. Print the widget as in image plugin.
*/

static void prnt(Pixmap pixmap, FILE *fp)
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
                key = strtok(data[i+1]+cpp, " \t");     /* skip past chars */
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

        /* print postscript preblurb */
        fprintf(fp, "%%!PS-Adobe-2.0 EPSF-2.0\n");
        fprintf(fp, "%%%%Creator: Image plugin for Siag Office\n");
        fprintf(fp, "%%%%Title: %s\n", title);
        fprintf(fp, "%%%%Pages: 1\n");
        fprintf(fp, "%%%%BoundingBox: %d %d %d %d\n", 0, 0, width, height);
        t = time(NULL);
        fprintf(fp, "%%%%CreationDate: %s", ctime(&t));
        fprintf(fp, "%%%%EndComments\n");
        fprintf(fp, "%%%%EndProlog\n");
        fprintf(fp, "%%%%Page: 1 1\n\n\n");
        fprintf(fp, "gsave\n\n");
        fprintf(fp, "/inch {72 mul} def\n");
        fprintf(fp, "%d %d scale\n", width, height);
        fprintf(fp, "/line %d string def\n", 3*width);
        fprintf(fp, "%d %d %d\n", width, height, depth);
        fprintf(fp, "[ %d %d %d %d %d %d ]\n", width, 0, 0, -height, 0, height);
        fprintf(fp, "{currentfile line readhexstring pop}\n");
        fprintf(fp, "false 3 colorimage\n");

        /* print all the pixels */
        col = 0;
        for (y = 0; y < height; y++) {
                char *line = data[y+ncolors+1];
                for (x = 0; x < width; x++) {
                        char *pix = line+cpp*x;
                        for (i = 0; i < ncolors; i++)
                                if (!memcmp(cm[i].chars, pix, cpp)) break;
                        if (i == ncolors) i = 0;
                        fprintf(fp, "%02hx%02hx%02hx",
                                (cm[i].xcolor.red / 256) & 255,
                                (cm[i].xcolor.green / 256) & 255,
                                (cm[i].xcolor.blue / 256) & 255);
                        col += 6;
                        if (col >= 72) {
                                fprintf(fp, "\n");
                                col = 0;
                        }
                }
        }
        if (col) fprintf(fp, "\n");

        /* print postscript postblurb */
        fprintf(fp, "%%\n\n");
        fprintf(fp, "grestore\n");

        for (i = 0; i < ncolors; i++) MwFree(cm[i].chars);
        MwFree(cm);
        MwFree(data);
}

/* ---
Print even though the plugin doesn't know how.
*/

static int print_anyway(int ph, FILE *fp)
{
	Widget ow;
	unsigned int width = plugin[ph].width;
	unsigned int height = plugin[ph].height;
	int i;

	ow = XtVaCreatePopupShell("plugin_print",
		overrideShellWidgetClass, topLevel,
		XtNx, 0, XtNy, 0,
		XtNwidth, width, XtNheight, height,
		(char *)0);
	XtPopup(ow, XtGrabNone);
	XSync(XtDisplay(ow), False);
	for (i = 0; i < 1 /*REPARENT_LOOPS*/; i++) {
		XReparentWindow(XtDisplay(topLevel), plugin[ph].victim,
			XtWindow(ow), 0, 0);
		XSync(XtDisplay(topLevel), False);
		XClearWindow(XtDisplay(topLevel), plugin[ph].victim);
		XSync(XtDisplay(topLevel), False);
	}

	/* print */
	prnt(plugin[ph].victim /*pixmap*/, fp);

	/* finished printing */

	for (i = 0; i < 1 /*REPARENT_LOOPS*/; i++) {
		XReparentWindow(XtDisplay(topLevel), plugin[ph].victim,
			XtWindow(plugin[ph].core), 2, 2);
		XSync(XtDisplay(topLevel), False);
	}
	XtDestroyWidget(ow);
	return 0;
}

#define TRAILER "%%Trailer"

/* ---
Tell the plugin to print its window into the file.

ph: Plugin handle

Returns 0 for success or non-0 for failure, such as an i/o error.
*/

/*
This code just doesn't work. There are too many opportunities for
the plugin to mess up the protocol state by sending data without a
trailer or by sending extra junk at the end or by sending bogus data.
I need to make this much more bulletproof. The first step is to better
show the difference between print data and plugin messages.
Precede each line with a single space and strip it off here.
Finish with a line consisting of the single word END.
*/

int plugin_print(int ph, FILE *fp, int x, int y, int trust_plugin)
{
	char b[1024];
	int w, h;
	int result = 1;

	fprintf(fp, "save\n");
	fprintf(fp, "%d %d translate\n", x, y);
	/* figure out how big the clipping rectangle should be */
	w = plugin[ph].width;
	h = plugin[ph].height;
	fprintf(fp, "newpath %d %d moveto %d %d lineto"
		" %d %d lineto %d %d lineto closepath clip\n",
		0, 0, 0, h, w, h, w, 0);
	fprintf(fp, "/showpage {} def\n");	/* disarm this */

	if (trust_plugin) {
		plugin_write(ph, "PRNT\n");
		if (!plugin_read(ph, b)) {
			fprintf(stderr, "Nothing from plugin\n");
			result = 1;
		} else if (strlen(b) < 5 || b[0] != '2') {
			result = print_anyway(ph, fp);
		} else {
			while (plugin_read(ph, b)) {
				if (b[0] != ' ') break;	/* found END */
				if (strcmp(b, " %%Trailer\n")
				    && strcmp(b, " %%EOF\n"))
					fputs(b+1, fp);
			}
			result = (b[0] == ' ');	/* didn't find END */
		}
	} else {
		result = print_anyway(ph, fp);
	}
	fprintf(fp, "restore\n");
	return result;
}

/* ---
Register a plugin handler.

desc: a plaintext description of the plugin
ext: magic extension to be handled by this plugin
cmd: command to run the plugin

Returns 0 for success, non-0 for failure.
*/

int plugin_register(char *desc, char *ext, char *cmd)
{
	char b[1024];
	int n = plugin_handler_find(ext);

	if (n == -1) {	/* allocate room for new plugin */
		n = nhandler++;

		handler = MwRealloc(handler, nhandler*sizeof *handler);
		plugin_patterns = MwRealloc(plugin_patterns,
			(nhandler+1)*sizeof *plugin_patterns);
		plugin_patterns[nhandler] = NULL;
	} else {	/* reuse old position */
		MwFree(handler[n].desc);
		MwFree(handler[n].ext);
		MwFree(handler[n].cmd);
		MwFree(plugin_patterns[n]);
	}

	handler[n].desc = MwStrdup(desc);
	handler[n].ext = MwStrdup(ext);
	handler[n].cmd = MwStrdup(cmd);
	sprintf(b, "%s (*.%s)", desc, ext);
	plugin_patterns[n] = MwStrdup(b);
	return 0;
}

/* ---
There are eight cases:
 - upper left corner
 - upper border
 - upper right corner
 - left border
 - right border
 - lower left corner
 - lower border
 - lower right corner

Corners resize, borders move. We don't implement moving yet, only
resizing. Also, we only allow resizing using the lower right corner.
*/

static void PluginResizeAction(Widget w,
		XEvent *event, String *params, Cardinal *n)
{
	XtAppContext app_context = XtWidgetToApplicationContext(w);
	int owner_events = True;
	unsigned int event_mask = ButtonReleaseMask |
				PointerMotionMask;
	int pointer_mode = GrabModeAsync;
	int keyboard_mode = GrabModeAsync;
	Window confine_to = XtWindow(XtParent(w));
	static Cursor cursor = None;
	Time time = CurrentTime;
	int waiting = True;
	Dimension wi, he, wi2, he2;
	int ph = plugin_find_by_widget(w);

	if (ph == -1) return;

	wi = event->xbutton.x;
	he = event->xbutton.y;
	XtVaGetValues(w,
		XtNwidth, &wi2,
		XtNheight, &he2,
		(char *)0);
	if (wi2-wi > 10 || he2-he > 10) return;

	/* snipped from colnum_grab */
	if (cursor == None)
		cursor = XCreateFontCursor(XtDisplay(w), XC_bottom_right_corner);
	XtGrabPointer(w, owner_events, event_mask,
		pointer_mode, keyboard_mode, confine_to, cursor, time);
	while (waiting) {
		XEvent event_return;
		XtAppNextEvent(app_context, &event_return);
		if (event_return.type == ButtonRelease) {
			waiting = False;
		} else if (event_return.type == MotionNotify) {
			wi = event_return.xmotion.x;
			he = event_return.xmotion.y;
			plugin_size_set(ph, wi, he);
		} else {
			XtDispatchEvent(&event_return);
		}
	}
	XtUngrabPointer(w, CurrentTime);
	pr_scr_flag = TRUE;
}

static void PluginCursorAction(Widget w,
	XEvent *event, String *params, Cardinal *n)
{
	int x, y;
	Dimension wi, he;

	x = event->xmotion.x;
	y = event->xmotion.y;
	XtVaGetValues(w,
		XtNwidth, &wi,
		XtNheight, &he,
		(char *)0);
	if (wi-x < 10 && he-y < 10) {
		XDefineCursor(XtDisplay(w), XtWindow(w), lr_cursor);
	} else {
		XDefineCursor(XtDisplay(w), XtWindow(w), None);
	}
}

static XtActionsRec actions[] =
{
	{"plugin-resize", PluginResizeAction},
	{"plugin-cursor", PluginCursorAction}
};


/* Handle callbacks from plugins

   The protocol today is:

   1. The application registers a window where events can be sent,
      and a function that can execute commands in the form of strings.
   2. An event handler is added to the window which handles ClientMessage
      events.
   3. Each started plugin is passed two pieces of information: the
      window above using the EWIN command, and its plugin handle using
      the PH command.
   4. When a plugin wants its parent's attention, it stuffs the following
      information into a ClientMessage event:
       - the protocol version, currently always 0
       - the plugin handle
       - a ticket: a number used to identify the message.
   5. The event is sent to the window registered above.
   6. The event handler receives the event, does a little sanity checking
      and extracts the information.
   7. The main application sends the command "WHAT ticket" to the plugin,
      where ticket is the number from step 4.
   8. The plugin responds with a string.
   9. The string is passed to the executor function registered in step 1.

   Now that the communication is established, it is up to the application
   and plugin to agree on how to continue. For Siag, the commands from the
   plugin will be handled by the execute function, so the command will
   simply be a Scheme function call. The function can then do what it
   is told and the exchange be finished. It may also be that the
   function comes back to the plugin and asks for more information,
   or it may feed data to the plugin. Examples:

    - A database plugin will take a SQL query (using the EXEC command).
      Since the query may take some time to finish, the main application
      goes about its business. When the query comes back, the plugin calls
      out to the main application. It uses a command (collect-sql),
      which goes back to the plugin and collects the data.
      There is no collect-sql function and no database plugin yet,
      I'm just making this up.
    - A plotting plugin uses a timer to periodically call back for
      updated data. That way real-time updates can be done without
      any special support in the main application.
      There is no plotting plugin yet either, although SciPlot looks
      promising.
    - A plugin can use the main application's user interface to display
      error messages.

    The plugin protocol only depends on Unix pipes for the communication.
    This extension depends on X for its implementation. For user interfaces
    based on X, such as Xt or Gtk, this is fine. For a plugin that has
    a text-based interface, or no user interface at all (what would a
    database plugin need a user interface for), there must be another
    mechanism.
*/

static int plugin_message(XEvent *event)
{
	return event->type == ClientMessage
		&& event->xclient.message_type == plugin_protocol
		&& event->xclient.format == 32;
}

static void PluginDispatchEvent(Widget w, XtPointer pt, XEvent *e, Boolean *b)
{
	int n, ph, ticket;
	char cmd[1024];
	if (!plugin_message(e)) {
		return;
	}
	if (e->xclient.data.l[0] != 0) {
		fprintf(stderr, "Unknown protocol version\n");
		return;
	}
	ph = e->xclient.data.l[1];
	ticket = e->xclient.data.l[2];
	sprintf(cmd, "WHAT %d\n", ticket);
	n = plugin_write(ph, cmd);
	if (plugin_read(ph, cmd) >= 2)
		plugin_execute(cmd);
}
/* end of plugin callback code */


/* ---
Initialise the plugin code.
*/

void plugin_init(Widget w, void (*handle_exit)(int), void (*exec_cmd)(char *))
{
	Display *dpy;
	XtAppContext app_context = XtWidgetToApplicationContext(w);
	topLevel = w;
	handle_plugin_exit = handle_exit;
	sane_cursor = XCreateFontCursor(XtDisplay(w), XC_top_left_arrow);
	lr_cursor = XCreateFontCursor(XtDisplay(w), XC_bottom_right_corner);
	XtAppAddActions(app_context, actions, XtNumber(actions));

	/* experimental code to get attention from plugins */
	dpy = XtDisplay(w);
	plugin_protocol = XInternAtom(dpy, "_PLUGIN_PROTOCOL", False);
	plugin_execute = exec_cmd;
	XtAddEventHandler(w, NoEventMask, True,
			PluginDispatchEvent, NULL);
}

/* ---
Returns the size of a plugin
*/

int plugin_size_get(int ph, int *width, int *height)
{
	if (ph < 0 || ph >= nplugin) return -1;
	*width = plugin[ph].width;
	*height = plugin[ph].height;
	return 0;
}
