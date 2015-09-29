/*
   clipart.c

   Copyright (C) 1999-2001  Ulric Eriksson <ulric@siag.nu>

   Based on Tim Theisen's Ghostview.

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
#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#include <sys/stat.h>
#include <signal.h>
#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

#include <math.h>

#include <errno.h>
/* BSD 4.3 errno.h does not declare errno */
extern int errno;
#ifdef VMS
#include <perror.h>
#else
#endif
#ifndef BUFSIZ
#define BUFSIZ 1024
#endif

#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xatom.h>
#include <X11/Xaw/Cardinals.h>

#include "../common/ps.h"
#include "../common/common.h"
#include "../xcommon/xcommon.h"
#include <Mowitz/Mowitz.h>

#include "../xcommon/Ghostview.h"

static int find_page(String);

#define APPNAME "Clipart"

String fallback_resources[] = {
"Clipart.geometry:		300x300",
"Clipart*topbox.xLayout:	100%",
"Clipart*topbox.yLayout:	100%",
"Clipart*input:			True",
"Clipart*allowShellResize:		True",
"Clipart*Ghostview.background:	white",
"Clipart.translations:		#replace \\n\
    <MapNotify>:		GhostviewCheckFile() \\n\
    <Message>WM_PROTOCOLS:	GhostviewDeleteWindow()",
"Clipart*TopLevelShell.translations:	#replace \\n\
    <Message>WM_PROTOCOLS:	GhostviewDismiss()",
"Clipart*TransientShell.translations:	#replace \\n\
    <Message>WM_PROTOCOLS:	GhostviewDismiss()",
"Clipart*Ghostview.translations:	#replace \\n\
    <Message>:	message() \\n\
    <EnterWindow>:	notify(0) \\n\
    <MotionNotify>:	notify(0) \\n\
    <Btn1Down>:	notify(180 180 200 200) \\n\
    <Btn2Down>:	notify(120 120 300 300) \\n\
    <Btn3Down>:	notify(90 90 400 400)",
"Clipart*topbox.translations:	#replace \\n\
    <Key>Q:	GhostviewQuit() \\n"
    "<Key>period:	GhostviewShow() \\n\
    Ctrl<Key>L:	GhostviewShow()",
    NULL
};

#ifndef max
#define max(a, b)	((a) > (b) ? (a) : (b))
#endif

/* Application resources */
typedef struct AppResources {
    Boolean show_title;		/* whether to show title */
    Boolean show_date;		/* whether to show date */
    Boolean show_locator;	/* whether to show locator */
    Boolean auto_center;	/* whether to automatically center the page */
    int wm_horiz_margin;	/* Space taken by window manager */
    int wm_vert_margin;		/* Space taken by window manager */
    int minimum_magstep;	/* smallest magstep allowed */
    int maximum_magstep;	/* largest magstep allowed */
    int magstep;		/* default magstep */
    XtPageOrientation orientation;	/* default orientation */
    String page;		/* first page to show */
    String pagemedia;		/* default page media */
    Boolean force_orientation;	/* use default to override document comments */
    Boolean force_pagemedia;	/* use default to override document comments */
    Boolean swap_landscape;	/* Landscape comment maps to Seascape */
    String print_command;	/* command used to print doc, usually "lpr" */
    String printer_variable;	/* env varaible to use, usually "PRINTER" */
    String default_printer;	/* printer to use if no PRINTER is not set*/
    String print_prompt;	/* string to prompt user for printer name */
    String print_fail;		/* string to inform user that print failed */
    String open_prompt;		/* string to prompt for file name to open */
    String open_fail;		/* string to inform user that open failed */
    String save_prompt;		/* string to prompt for file name to save */
    String save_fail;		/* string to inform user that save failed */
    /* Work arounds for others' bugs */
    Boolean openwindows;	/* whether to work around openwindow bug */
    Boolean ncdwm;		/* whether to work around ncdwm bug */
    Boolean plugin;		/* whether to act as plugin */
} AppResources;

static float	default_xdpi;
static float	default_ydpi;

static int	num_ghosts;
static FILE	*psfile;
static String	filename;
static String	oldfilename;
static int	current_page;
static int	default_pagemedia;
static int	current_pagemedia;
static Boolean	force_document_media;
static int	document_media;
static int	current_llx;
static int	current_lly;
static int	current_urx;
static int	current_ury;
static int	base_papersize;
static int	toc_length;
static int	toc_entry_length;
static time_t	mtime;
static struct document *doc;
static struct document *olddoc;
static Atom	wm_delete_window;
static int	catch_Xerror(Display *, XErrorEvent *);
static XErrorHandler	old_Xerror;
static Boolean	dying;
static XErrorEvent	bomb;

enum {OPEN, PRINT_WHOLE, PRINT_MARKED, SAVE};

static XtAppContext app_con;
static AppResources app_res;

/* Widgets */
static Widget toplevel;
static Widget topbox;
static Widget page;

static void add_actions(XtAppContext);

/* Callbacks */
static void quit_ghostview(Widget, XtPointer, XtPointer);
static void this_page(Widget, XtPointer, XtPointer);
static void message(Widget, XtPointer, XtPointer);
static void destroy_ghost(Widget, XtPointer, XtPointer);

/* Misc */
static void show_page(int);
static Boolean setup_ghostview(void);
static Boolean set_new_pagemedia(int);
static void new_file(int);

/* Start application folding up by Destroying the top level widget. */
/* The application exits when the last interpreter is killed during */
/* a destroy callback from ghostview widgets. */
static void quit_ghostview(Widget w, XtPointer client_data, XtPointer call_data)
{
    XtDestroyWidget(toplevel);
}

/* Get the selection, if no selection, get the insertion point. */
/* Show this page.  */
static void this_page(Widget w, XtPointer client_data, XtPointer call_data)
{
	GhostviewDisableInterpreter(page);
	show_page(0);
}

/* Process messages from ghostscript */
/* Refresh occurs when window was resized unexpectedly */
static void message(Widget w, XtPointer client_data, XtPointer call_data)
{
    int i;
    char *error = "Unknown error";

    if (!strcmp((char *) call_data, "Failed")) {
	if ((Widget)client_data == page) {
	    error = "Error: PostScript interpreter failed in main window.\n\n";
	}
    } else if (!strcmp((char *) call_data, "BadAlloc")) {
	if ((Widget)client_data == page) {
	    error = 
	       "Warning: Could not allocate backing pixmap in main window.\n\n";
	}
    } else if (!strcmp((char *) call_data, "Refresh")) {
	    GhostviewSendPS(w, psfile, doc->beginprolog,
			    doc->lenprolog, False);
	    GhostviewSendPS(w, psfile, doc->beginsetup,
			    doc->lensetup, False);
	    if (doc->pageorder == DESCEND)
		i = (doc->numpages - 1) - current_page;
	    else
		i = current_page;
	    GhostviewSendPS(w, psfile, doc->pages[i].begin,
			    doc->pages[i].len, False);
    }
}

/* destroy callback for Ghostview widgets. */
/* The disable interpreter call ensures that ghostscript is killed. */
/* One the count goes to 0, we are sure that all forked processes have */
/* been killed and that we can safely exit. */
static void destroy_ghost(Widget w, XtPointer client_data, XtPointer call_data)
{
    GhostviewDisableInterpreter((Widget) client_data);
    num_ghosts--;
    if (num_ghosts) return;
    if (dying) old_Xerror(XtDisplay(w), &bomb);
    XtDestroyApplicationContext(app_con);
    exit(0);
}

/* from actions.c */

/* Call the quit callback to stop ghostview */
static void gv_quit(Widget w, XEvent *event,
		String *params, Cardinal *num_params)
{
	if (!app_res.plugin) quit_ghostview(w, NULL, NULL);
}

/* Call the this_page callback */
static void gv_show(Widget w, XEvent *event,
		String *params, Cardinal *num_params)
{
    this_page(w, NULL, NULL);
}

/* Implement WM_DELETE_WINDOW protocol */
static void gv_delete_window(Widget w, XEvent *event,
		String *params, Cardinal *num_params)
{
    if (event->type == ClientMessage &&
	event->xclient.data.l[0] != wm_delete_window) return;
    XtDestroyWidget(w);
}

/* Check to see if file was updated */
static void gv_check_file(Widget w, XEvent *event, String *params,
		Cardinal *num_params)
{
    struct stat sbuf;

    if (psfile) {
	if (!stat(filename, &sbuf) && mtime != sbuf.st_mtime) {
	    show_page(current_page);
	}
    }
}

static XtActionsRec actions[] = {
    {"GhostviewQuit",		gv_quit},
    {"GhostviewShow",		gv_show},
    {"GhostviewDeleteWindow",	gv_delete_window},
    {"GhostviewCheckFile",	gv_check_file},
};

static void add_actions(XtAppContext ac)
{
   	XtAppAddActions(ac, actions, XtNumber(actions));
}

static XtResource resources[] = {
    {"showTitle", "Labels", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, show_title), XtRImmediate, (XtPointer)True},
    {"showDate", "Labels", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, show_date), XtRImmediate, (XtPointer)True},
    {"showLocator", "Labels", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, show_locator), XtRImmediate, (XtPointer)True},
    {"autoCenter", "AutoCenter", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, auto_center), XtRImmediate, (XtPointer)True},
    {"horizontalMargin", "Margin", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, wm_horiz_margin), XtRImmediate, (XtPointer)20},
    {"verticalMargin", "Margin", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, wm_vert_margin), XtRImmediate, (XtPointer)44},
    {"minimumMagstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, minimum_magstep), XtRImmediate, (XtPointer)-5},
    {"maximumMagstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, maximum_magstep), XtRImmediate, (XtPointer)5},
    {"magstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, magstep), XtRImmediate, (XtPointer)0},
    {"orientation", "Orientation", XtRPageOrientation,
     sizeof(XtPageOrientation), XtOffsetOf(AppResources, orientation),
     XtRImmediate, (XtPointer) XtPageOrientationPortrait},
    {"page", "Page", XtRString, sizeof(String),
     XtOffsetOf(AppResources, page), XtRImmediate, NULL},
    {"pageMedia", "PageMedia", XtRString, sizeof(String),
     XtOffsetOf(AppResources, pagemedia), XtRImmediate, "Letter"},
    {"forceOrientation", "Force", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, force_orientation), XtRImmediate,
     (XtPointer)False},
    {"forcePageMedia", "Force", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, force_pagemedia), XtRImmediate, (XtPointer)False},
    {"swapLandscape", "SwapLandscape", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, swap_landscape), XtRImmediate, (XtPointer)False},
#ifndef VMS
#if defined(SVR4) || defined(SYSV) || defined(USG)
    {"printCommand", "PrintCommand", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_command), XtRImmediate, "lp"},
    {"printerVariable", "PrinterVariable", XtRString, sizeof(String),
     XtOffsetOf(AppResources, printer_variable), XtRImmediate, "LPDEST"},
#else
    {"printCommand", "PrintCommand", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_command), XtRImmediate, "lpr"},
    {"printerVariable", "PrinterVariable", XtRString, sizeof(String),
     XtOffsetOf(AppResources, printer_variable), XtRImmediate, "PRINTER"},
#endif
    {"printPrompt", "PrintPrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_prompt), XtRImmediate, "Printer Name:"},
#else /* VMS */
    {"printCommand", "PrintCommand", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_command), XtRImmediate, "Print_dcl/Delete"},
    {"printerVariable", "PrinterVariable", XtRString, sizeof(String),
     XtOffsetOf(AppResources, printer_variable), XtRImmediate, "GV_PRINT_QUAL"},
    {"printPrompt", "PrintPrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_prompt), XtRImmediate, "Print Qualifiers:"},
#endif /* VMS */
    {"defaultPrinter", "DefaultPrinter", XtRString, sizeof(String),
     XtOffsetOf(AppResources, default_printer), XtRImmediate, NULL},
    {"printFail", "printFail", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_fail), XtRImmediate,
     "\"%s\" command failed."},
    {"openPrompt", "OpenPrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, open_prompt), XtRImmediate, "Open File:"},
    {"openFail", "OpenFail", XtRString, sizeof(String),
     XtOffsetOf(AppResources, open_fail), XtRImmediate, "Cannot open file: "},
    {"savePrompt", "SavePrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, save_prompt), XtRImmediate, "Save File:"},
    {"saveFail", "SaveFail", XtRString, sizeof(String),
     XtOffsetOf(AppResources, save_fail), XtRImmediate, "Cannot save file: "},
    {"openWindows", "OpenWindows", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, openwindows), XtRImmediate, (XtPointer)False},
    {"ncdwm", "Ncdwm", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, ncdwm), XtRImmediate, (XtPointer)False},
    {"plugin", "Plugin", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, plugin), XtRImmediate, (XtPointer)False},
};

static XrmOptionDescRec options[] = {
    {"-monochrome", "*Ghostview.palette", XrmoptionNoArg, "Monochrome"},
    {"-grayscale", "*Ghostview.palette", XrmoptionNoArg, "Grayscale"},
    {"-color", "*Ghostview.palette", XrmoptionNoArg, "Color"},
    {"-page", ".page", XrmoptionSepArg, NULL},
    {"-title", ".showTitle", XrmoptionNoArg, "True"},
    {"-notitle", ".showTitle", XrmoptionNoArg, "False"},
    {"-date", ".showDate", XrmoptionNoArg, "True"},
    {"-nodate", ".showDate", XrmoptionNoArg, "False"},
    {"-locator", ".showLocator", XrmoptionNoArg, "True"},
    {"-nolocator", ".showLocator", XrmoptionNoArg, "False"},
    {"-center", ".autoCenter", XrmoptionNoArg, "True"},
    {"-nocenter", ".autoCenter", XrmoptionNoArg, "False"},
    {"-labels", ".Labels", XrmoptionNoArg, "True"},
    {"-nolabels", ".Labels", XrmoptionNoArg, "False"},
    {"-quiet", "*Ghostview.quiet", XrmoptionNoArg, "True"},
    {"-noquiet", "*Ghostview.quiet", XrmoptionNoArg, "False"},
    {"-safer", "*Ghostview.safer", XrmoptionNoArg, "True"},
    {"-nosafer", "*Ghostview.safer", XrmoptionNoArg, "False"},
    {"-arguments", "*Ghostview.arguments", XrmoptionSepArg, NULL},
    {"-xdpi", "*Ghostview.xdpi", XrmoptionSepArg, NULL},
    {"-ydpi", "*Ghostview.ydpi", XrmoptionSepArg, NULL},
    {"-dpi", "*Ghostview.Resolution", XrmoptionSepArg, NULL},
    {"-resolution", "*Ghostview.Resolution", XrmoptionSepArg, NULL},
    {"-magstep", ".magstep", XrmoptionSepArg, NULL},
    {"-portrait", ".orientation", XrmoptionNoArg, "Portrait"},
    {"-landscape", ".orientation", XrmoptionNoArg, "Landscape"},
    {"-upsidedown", ".orientation", XrmoptionNoArg, "Upside-down"},
    {"-seascape", ".orientation", XrmoptionNoArg, "Seascape"},
    {"-forceorientation", ".forceOrientation", XrmoptionNoArg, "True"},
    {"-letter", ".pageMedia", XrmoptionNoArg, "Letter"},
    {"-tabloid", ".pageMedia", XrmoptionNoArg, "Tabloid"},
    {"-ledger", ".pageMedia", XrmoptionNoArg, "Ledger"},
    {"-legal", ".pageMedia", XrmoptionNoArg, "Legal"},
    {"-statement", ".pageMedia", XrmoptionNoArg, "Statement"},
    {"-executive", ".pageMedia", XrmoptionNoArg, "Executive"},
    {"-a3", ".pageMedia", XrmoptionNoArg, "A3"},
    {"-a4", ".pageMedia", XrmoptionNoArg, "A4"},
    {"-a5", ".pageMedia", XrmoptionNoArg, "A5"},
    {"-b4", ".pageMedia", XrmoptionNoArg, "B4"},
    {"-b5", ".pageMedia", XrmoptionNoArg, "B5"},
    {"-folio", ".pageMedia", XrmoptionNoArg, "Folio"},
    {"-quarto", ".pageMedia", XrmoptionNoArg, "Quarto"},
    {"-10x14", ".pageMedia", XrmoptionNoArg, "10x14"},
    {"-forcemedia", ".forcePageMedia", XrmoptionNoArg, "True"},
    {"-force", ".Force", XrmoptionNoArg, "True"},
    {"-swap", ".swapLandscape", XrmoptionNoArg, "True"},
    {"-noswap", ".swapLandscape", XrmoptionNoArg, "False"},
    {"-openwindows", ".openWindows", XrmoptionNoArg, "True"},
    {"-noopenwindows", ".openWindows", XrmoptionNoArg, "False"},
    {"-ncdwm", ".ncdwm", XrmoptionNoArg, "True"},
    {"-noncdwm", ".ncdwm", XrmoptionNoArg, "False"},
    {"-plugin", "*plugin", XrmoptionNoArg, "True"},
};

/* Plugin stuff */
static void win(char *p)
{
	printf("250 %lx\n", (unsigned long) XtWindow(toplevel));
}

static void quit(char *p)
{
	printf("221 Over and out\n");
	quit_ghostview(toplevel, NULL, NULL);
}

/* ---
Printing is pretty straightforward since we already have the postscript
code. All we have to do is get the scaling right.
*/

static void prnt(char *p)
{
	FILE *fp;
	char b[1024];
	Dimension width, height;
	double scalew, scaleh;

	printf("200 Postscript coming up\n");

	XtVaGetValues(page, XtNwidth, &width, XtNheight, &height, (char *)0);
	scalew = width;
	scaleh = height;
	scalew /= (current_urx-current_llx);
	scaleh /= (current_ury-current_lly);

	printf(" %.3f %.3f scale\n", scalew, scaleh);
	printf(" -%d -%d translate\n", current_llx, current_lly);
	fp = fopen(filename, "r");
	while (fgets(b, sizeof b, fp)) printf(" %s", b);
	fclose(fp);
	printf("END\n");
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

int main(int argc, char **argv)
{
    struct stat sbuf;
    Display *dpy;
    Screen *scr;
    Arg args[20];
    Cardinal num_args;
    int i;
    String s1;

    common_init("Clipart %s. No Warranty.");

    XtToolkitInitialize();
    XtSetTypeConverter(XtRString, XtRPageOrientation,
		       XmuCvtStringToPageOrientation, NULL, 0,
		       XtCacheAll, NULL);
    app_con = XtCreateApplicationContext();
    add_actions(app_con);
    XtAppSetFallbackResources(app_con, fallback_resources);
    dpy = XtOpenDisplay(app_con, NULL, NULL, "Clipart",
			options, XtNumber(options), &argc, argv);
    if (dpy == NULL) {
	fprintf(stderr, "%s: cannot open DISPLAY.\n", argv[0]);
	exit(EXIT_FAILURE);
    }
    if (argc > 2) exit(EXIT_FAILURE);
    if (argc == 2) {
	char *p, b[2000];
	filename = XtNewString(argv[1]);
	p = strrchr(filename, '.');
	if (p && !MwStrcasecmp(p, ".tex")) {
		sprintf(b,
			"(cp %s %s/clipart.tex &&"
			"cd %s &&"
			"latex clipart.tex &&"
			"dvips -E clipart.dvi -o clipart.ps) > /dev/null 2>&1",
			filename, siag_tmpdir, siag_tmpdir);
		system(b);
		sprintf(b, "%s/clipart.ps", siag_tmpdir);
		filename = XtNewString(b);
	}
	else if (p && !MwStrcasecmp(p, ".dvi")) {
		sprintf(b,
			"(cp %s %s/clipart.dvi &&"
			"dvips -E clipart.dvi -o clipart.ps) > /dev/null 2>&1",
			filename, siag_tmpdir);
		system(b);
		sprintf(b, "%s/clipart.ps", siag_tmpdir);
		filename = XtNewString(b);
	}
	if (strcmp(filename, "-")) {
#ifdef VMS
	    if ((psfile = fopen(filename, "r", "mbc=100")) == NULL) {
#else
	    if ((psfile = fopen(filename, "r")) == NULL) {
#endif
		printf("501 Cannot open\n");
		fprintf(stderr, "Cannot open ");
		perror(argv[1]);
		exit(EXIT_FAILURE);
	    }
	    stat(filename, &sbuf);
	    mtime = sbuf.st_mtime;
	}
    }

    old_Xerror = XSetErrorHandler(catch_Xerror);

    scr = DefaultScreenOfDisplay(dpy);
    wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);

    toplevel = XtAppCreateShell(NULL, "Clipart", applicationShellWidgetClass,
				dpy, NULL, ZERO);

    XtGetApplicationResources(toplevel, (XtPointer) &app_res,
			      resources, XtNumber(resources), NULL, ZERO);
    if ((s1 = getenv(app_res.printer_variable))) app_res.default_printer = s1;

    /* Instantiate Widgets */

	topbox = XtVaCreateManagedWidget("topbox",
		mwRudegridWidgetClass, toplevel,
		(char *)0);

	page = XtVaCreateManagedWidget("page",
		ghostviewWidgetClass, topbox,
		(char *)0);
    num_ghosts++;
    XtAddCallback(page, XtNdestroyCallback, destroy_ghost, (XtPointer)page);
    XtAddCallback(page, XtNmessageCallback, message, (XtPointer)page);
							num_args = 0;
    XtSetArg(args[num_args], XtNxdpi, &default_xdpi);	num_args++;
    XtSetArg(args[num_args], XtNydpi, &default_ydpi);	num_args++;
    XtGetValues(page, args, num_args);

    /* This sets most of the window sizes.  This keeps X server traffic
     * down during realization.
     */
    GhostviewDisableInterpreter(page);
    setup_ghostview();
    i = find_page(app_res.page);

    /* Coerce magstep to fall in range */
    if (app_res.magstep < app_res.minimum_magstep)
	app_res.magstep = app_res.minimum_magstep;
    if (app_res.magstep > app_res.maximum_magstep)
	app_res.magstep = app_res.maximum_magstep;
    set_new_pagemedia(i);

    XtSetMappedWhenManaged(toplevel, False);
    XtRealizeWidget(toplevel);

    XSetWMProtocols(dpy, XtWindow(toplevel), &wm_delete_window, 1);

    /* This sets the sizes on widget that were created during the realize. */
    XtMapWidget(toplevel);

    show_page(i);

	if (app_res.plugin) {
		XtAppAddInput(XtWidgetToApplicationContext(toplevel),
			fileno(stdin), (XtPointer) XtInputReadMask,
			read_plugin_cmd, NULL);
		printf("220 Image plugin\n");
		fflush(stdout);
	}

    XtAppMainLoop(app_con);

    /* should never get here */
    return 1;
}

static void reset_size_hints(void)
{
    Arg args[4];
    if (app_res.ncdwm) return;
    XtSetArg(args[0], XtNmaxWidth, XtUnspecifiedShellInt);
    XtSetArg(args[1], XtNmaxHeight, XtUnspecifiedShellInt);
    XtSetArg(args[2], XtNminWidth, XtUnspecifiedShellInt);
    XtSetArg(args[3], XtNminHeight, XtUnspecifiedShellInt);
    XtSetValues(toplevel, args, FOUR);
}

#define BOUNDINGBOX "%%BoundingBox:"

static void read_page_size(int *x1, int *y1, int *x2, int *y2)
{
	char b[100];
	FILE *fp = fopen(filename, "r");
	*x1 = *y1 = *x2 = *y2 = 0;
	if (fp == NULL) {
		fprintf(stderr, "Can't open %s\n", filename);
		return;
	}
	while (fgets(b, sizeof b, fp)) {
		if (!MwStrncasecmp(b, BOUNDINGBOX, sizeof BOUNDINGBOX - 1)) {
			sscanf(b+sizeof BOUNDINGBOX, "%d %d %d %d",
				x1, y1, x2, y2);
			break;
		}
	}
	fclose(fp);
	if (x1 >= x2) x2 = x1+100;
	if (y1 >= y2) y2 = y1+100;
}

/* Start rendering a new page */
static void show_page(int number)
{
    struct stat sbuf;
    int i;

    if (!filename) return;

    /* If the file has changed, rescan it so that offsets into the file
     * are still correct.  If the file is rescanned, we must setup ghostview
     * again.  Also, force a new copy of ghostscript to start. */
    if (psfile) {
	if (!stat(filename, &sbuf) && mtime != sbuf.st_mtime) {
	    fclose(psfile);
	    psfile = fopen(filename, "r");
	    mtime = sbuf.st_mtime;
	    if (oldfilename) XtFree(oldfilename);
	    oldfilename = XtNewString(filename);
	    new_file(number);
	}
    }

    /* Coerce page number to fall in range */
	number = 0;

    if (/*set_new_orientation(number) ||*/ set_new_pagemedia(number))
		;
    if (1) {
	current_page = number;
	if (GhostviewIsInterpreterReady(page)) {
	    GhostviewNextPage(page);
	} else {
if (doc == NULL) {
fprintf(stderr, "doc is NULL\n");
return;
}
	    GhostviewEnableInterpreter(page);
	    GhostviewSendPS(page, psfile, doc->beginprolog,
			    doc->lenprolog, False);
	    GhostviewSendPS(page, psfile, doc->beginsetup,
			    doc->lensetup, False);
	}
	if (doc->pageorder == DESCEND)
	    i = (doc->numpages - 1) - current_page;
	else
	    i = current_page;
if (doc && (doc->pages == NULL)) {
read_page_size(&doc->boundingbox[LLX],
		&doc->boundingbox[LLY],
		&doc->boundingbox[URX],
		&doc->boundingbox[URY]);
reset_size_hints();
current_llx = doc->boundingbox[LLX];
current_lly = doc->boundingbox[LLY];
current_urx = doc->boundingbox[URX];
current_ury = doc->boundingbox[URY];
if (current_llx < current_urx && current_lly < current_ury) {
XtVaSetValues(page,
	XtNllx, current_llx,
	XtNlly, current_lly,
	XtNurx, current_urx,
	XtNury, current_ury,
	(char *)0);
XtVaSetValues(toplevel,
	XtNwidth, current_urx-current_llx,
	XtNheight, current_ury-current_lly,
	(char *)0);
}
set_new_pagemedia(0);
return;
}

	GhostviewSendPS(page, psfile, doc->pages[i].begin,
			doc->pages[i].len, False);
    } else {
	if (!GhostviewIsInterpreterRunning(page))
	    GhostviewEnableInterpreter(page);
	else if (GhostviewIsInterpreterReady(page))
	    GhostviewNextPage(page);
	else
	    XBell(XtDisplay(page), 0);
    }
}

/* setup ghostview.  This includes:
 *  scanning the PostScript file,
 *  setting the title and date labels,
 *  building the pagemedia menu,
 *  building the toc (table of contents)
 *  sensitizing the appropriate menu buttons,
 *  popping down and erasing the infotext popup.
 */

static Boolean useful_page_labels;
Boolean setup_ghostview(void)
{
    Arg args[20];
    Cardinal num_args;
    int oldtoc_entry_length = 0;

    /* Reset to a known state. */
    psfree(olddoc);
    olddoc = doc;
    doc = NULL;
    current_page = -1;

    /* Scan document and start setting things up */
    if (psfile) doc = psscan(psfile);

    /* Reset ghostscript and output messages popup */
    if (!doc || !olddoc ||
	strcmp(oldfilename, filename) ||
	olddoc->beginprolog != doc->beginprolog ||
	olddoc->endprolog != doc->endprolog ||
	olddoc->beginsetup != doc->beginsetup ||
	olddoc->endsetup != doc->endsetup) {

	GhostviewDisableInterpreter(page);
    }

    /* Build table of contents */
    if (doc && ((!doc->epsf && doc->numpages > 0) ||
		 (doc->epsf && doc->numpages > 1))) {
	int maxlen = 0;
	int i;
	useful_page_labels = False;

	if (doc->numpages == 1) useful_page_labels = True;
	for (i = 1; i < doc->numpages; i++)
	    if ((useful_page_labels = (useful_page_labels ||
		    strcmp(doc->pages[i-1].label, doc->pages[i].label)))) break;
	if (useful_page_labels) {
	    for (i = 0; i < doc->numpages; i++) 
		maxlen = max(maxlen, strlen(doc->pages[i].label));
	} else {
	    double x;
	    x = doc->numpages;
	    maxlen = log10(x) + 1;
	}
							      	num_args = 0;
	XtSetArg(args[num_args], XtNfilename, NULL);      	num_args++;
	XtSetValues(page, args, num_args);
    } else {
	toc_length = 0;
	toc_entry_length = 3;
							      	num_args = 0;
	XtSetArg(args[num_args], XtNfilename, filename);      	num_args++;
	XtSetValues(page, args, num_args);
    }
								num_args = 0;
    XtSetArg(args[num_args], XtNlength, toc_length);		num_args++;
    return oldtoc_entry_length != toc_entry_length;
}

int
find_page(label)
    String label;
{
    int i, j;

    if (label == NULL || doc == NULL) return 0;

    if (useful_page_labels) {
	for (i = 0; i < doc->numpages; i++) {
	    if (doc->pageorder == DESCEND) {
		j = (doc->numpages - 1) - i;
	    } else {
		j = i;
	    }
	    if (!strcmp(label, doc->pages[j].label)) return i;
	}
	return 0;
    } else {
	return atoi(label) - 1;
    }
}

/* try_try_again sets the geometry of the form when the form failed
 * to do it earlier.  It uses activity check with exponential backoff
 * to make sure that the dust has settled before trying again.
 */
static unsigned int delay = 125;	/* Start with 1/8 second delay */

static void try_try_again(XtPointer client_data, XtIntervalId *timer)
{
    XSync(XtDisplay(toplevel), False);	/* Push everything out */
    if (XtAppPending(app_con)) {
	XtAppAddTimeOut(app_con, delay, try_try_again, NULL);
	/* fprintf(stderr, "Delaying(%d)...\n",delay); */
	delay *= 2;
    } else {
	/* fprintf(stderr, "Trying again...\n"); */
	;
    }
}

/* Attempt to open file, return error message string on failure */
String open_file(String name)
{
    FILE *fp;
    struct stat sbuf;

    if (*name == '\0') {	/* Null filename */
	return(NULL);
    }
    if (strcmp(name, "-")) {
	if ((fp = fopen(name, "r")) == NULL) {
	    String buf = XtMalloc(strlen(app_res.open_fail) +
				  /*strlen(sys_errlist[errno])*/ + 1);
	    strcpy(buf, app_res.open_fail);
	    return buf;
	} else {
	    if (oldfilename) XtFree(oldfilename);
	    oldfilename = filename;
	    filename = XtNewString(name);
	    if (psfile) fclose(psfile);
	    psfile = fp;
	    stat(filename, &sbuf);
	    mtime = sbuf.st_mtime;
	    new_file(0);
	    show_page(0);
	    return(NULL);
	}
    } else {
	if (oldfilename) XtFree(oldfilename);
	oldfilename = filename;
	filename = XtNewString(name);
	if (psfile) fclose(psfile);
	psfile = NULL;
	new_file(0);
	show_page(0);
	return(NULL);
    }
}

/* Attempt to save file, return error message string on failure */
String save_file(String name)
{
    FILE *pswrite;

    if (*name == '\0') {	/* Null filename */
	return(NULL);
    }
    if ((pswrite = fopen(name, "w")) == NULL) {
    	char *se = strerror(errno);
	String buf = XtMalloc(strlen(app_res.save_fail) +
			      strlen(se/*sys_errlist[errno]*/) + 1);
	strcpy(buf, app_res.save_fail);
	strcat(buf, se);
	return buf;
    } else {
	fclose(pswrite);
	return(NULL);
    }
}

/* Set new pagemedia */
Boolean set_new_pagemedia(int number)
{
    int new_pagemedia;
    int new_llx;
    int new_lly;
    int new_urx;
    int new_ury;
    Boolean changed = False;
    Boolean from_doc = False;
    Arg args[4];

    if (force_document_media) {
	new_pagemedia = document_media;
    } else if (app_res.force_pagemedia) {
	new_pagemedia = default_pagemedia;
    } else {
	if (doc) {
	    if (doc->default_page_media != NULL) {
		new_pagemedia = doc->default_page_media - doc->media;
		from_doc = True;
	    } else {
		new_pagemedia = default_pagemedia;
	    }
	} else {
	    new_pagemedia = default_pagemedia;
	}
    }

    /* If pagemedia changed, remove the old marker. */
    if (new_pagemedia != current_pagemedia) {
	current_pagemedia = new_pagemedia;
    }

    /* Compute bounding box */
    if (!force_document_media && !app_res.force_pagemedia &&
	doc && doc->epsf &&
	/* Ignore malformed bounding boxes */
	(doc->boundingbox[URX] > doc->boundingbox[LLX]) &&
	(doc->boundingbox[URY] > doc->boundingbox[LLY])) {
	new_llx = doc->boundingbox[LLX];
	new_lly = doc->boundingbox[LLY];
	new_urx = doc->boundingbox[URX];
	new_ury = doc->boundingbox[URY];
    } else {
	new_llx = new_lly = 0;
	if (new_pagemedia < base_papersize) {
	    new_urx = doc->media[new_pagemedia].width;
	    new_ury = doc->media[new_pagemedia].height;
	} else {
	    new_urx = papersizes[new_pagemedia-base_papersize].width;
	    new_ury = papersizes[new_pagemedia-base_papersize].height;
	}
    }

    /* If bounding box changed, setup for new size. */
    if ((new_llx != current_llx) || (new_lly != current_lly) ||
	(new_urx != current_urx) || (new_ury != current_ury)) {
	GhostviewDisableInterpreter(page);
	reset_size_hints();
	changed = True;
	current_llx = new_llx;
	current_lly = new_lly;
	current_urx = new_urx;
	current_ury = new_ury;
	XtSetArg(args[0], XtNllx, current_llx);
	XtSetArg(args[1], XtNlly, current_lly);
	XtSetArg(args[2], XtNurx, current_urx);
	XtSetArg(args[3], XtNury, current_ury);
	XtSetValues(page, args, FOUR);
    }

    return changed;
}

static void new_file(int number)
{
    Boolean layout_changed = False;

    if (setup_ghostview()) layout_changed = True;
    if (set_new_pagemedia(number)) layout_changed = True;
}

/* Catch X errors die gracefully if one occurs */
int catch_Xerror(dpy, err)
    Display *dpy;
    XErrorEvent *err;
{
int n, *p = NULL;
n = *p;
return 0;

    if (err->error_code == BadImplementation) {
	old_Xerror(dpy, err);
	return 0;
    }
    if (dying) return 0;
    dying = True;
    bomb = *err;
    XtDestroyWidget(toplevel);
    return 0;
}

