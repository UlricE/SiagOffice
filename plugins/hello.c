/*
   hello.c
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
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <Mowitz/Mowitz.h>

static Widget topLevel;
static Atom plugin_protocol;
static Window ewin = None;
static int ph = 0;

/* Plugin stuff */
static void ewin_cmd(char *p)
{
	unsigned long w;
	if (sscanf(p, "%lx", &w) != 1) {
		printf("500 Where\n");
		return;
	}
   	ewin = w;
	printf("250 OK\n");
}

static void ph_cmd(char *p)
{
	if (sscanf(p, "%d", &ph) != 1) {
		printf("500 What\n");
		return;
	}
	printf("250 OK\n");
}

static void what_cmd(char *p)
{
	printf("(llpr \"Howdy!\")\n");
}

static void win_cmd(char *p)
{
	printf("250 %lx\n", (unsigned long) XtWindow(topLevel));
}

static void quit_cmd(char *p)
{
	printf("221 Over and out\n");
	exit(0);
}

static struct {
	char *verb;
	void (*cb) (char *);
} plugin_cmds[] = {
	{"EWIN", ewin_cmd},
	{"PH", ph_cmd},
	{"WHAT", what_cmd},
	{"WIN", win_cmd},
	{"QUIT", quit_cmd},
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
	if (plugin_cmds[i].verb) {
		(*plugin_cmds[i].cb) (b+strlen(plugin_cmds[i].verb)+1);
	} else {
		printf("500 What are you talking about\n");
	}
	fflush(stdout);
}

/* ---
*/
void mainloop(void)
{
	XtAppAddInput(XtWidgetToApplicationContext(topLevel),
		fileno(stdin), (XtPointer) XtInputReadMask,
		read_plugin_cmd, NULL);
	printf("220 Hello plugin\n");
	fflush(stdout);

	XtAppMainLoop(XtWidgetToApplicationContext(topLevel));
}

static void plugin_callback(Widget w, XtPointer cl, XtPointer ca)
{
	XEvent event;
	Window target = ewin;
	Display *dpy = XtDisplay(topLevel);
	if (target == None) {
		fprintf(stderr, "No event window set\n");
		return;
	}
	event.xclient.type = ClientMessage;
	event.xclient.display = dpy;
	event.xclient.message_type = plugin_protocol;
	event.xclient.format = 32;
	event.xclient.window = target;
	event.xclient.data.l[0] = 0;	/* protocol version */
	event.xclient.data.l[1] = ph;	/* plugin handle */
	event.xclient.data.l[2] = 0;	/* ticket */

	XSendEvent(dpy, target, True, NoEventMask, &event);
}

/* ---
*/
int main(int argc, char **argv)
{
	XtAppContext ac;
	Display *dpy;
	Widget w;

	topLevel = XtOpenApplication(&ac, "Hello",
		NULL, 0, &argc, argv, NULL,
		mwApplicationShellWidgetClass, NULL, 0);
	w = XtVaCreateManagedWidget("hello",
		commandWidgetClass, topLevel,
		XtNlabel, "Hello, World", (char *)0);
	XtAddCallback(w, XtNcallback, plugin_callback, NULL);
	dpy = XtDisplay(topLevel);
	plugin_protocol = XInternAtom(dpy, "_PLUGIN_PROTOCOL", False);

	XtRealizeWidget(topLevel);

	mainloop();
	return 0;
}
