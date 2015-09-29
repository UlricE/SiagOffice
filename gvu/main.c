/*
 * main.c -- Main routine for ghostview.
 * Copyright (C) 1992  Timothy O. Theisen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *   Author: Tim Theisen           Systems Programmer
 * Internet: tim@cs.wisc.edu       Department of Computer Sciences
 *     UUCP: uwvax!tim             University of Wisconsin-Madison
 *    Phone: (608)262-0438         1210 West Dayton Street
 *      FAX: (608)262-9777         Madison, WI   53706
 *
 */

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/xpm.h>

#include "blank.xpm"
#include "dot.xpm"

#include <Mowitz/Mowitz.h>

#include "../common/common.h"
#include "../xcommon/xcommon.h"

#include "gv.h"
#include "../common/ps.h"

extern int find_page(String);

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
    {"antialiasing", "Antialiasing", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, antialiasing), XtRImmediate, (XtPointer)True},
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
    {"-alpha", "*Ghostview.alpha", XrmoptionNoArg, "True"},
    {"-noalpha", "*Ghostview.alpha", XrmoptionNoArg, "False"},
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
};

#define MENUBAR (1)
#define TOOLBAR (2)
#define FORMATBAR (4)

#define APPNAME "Gvu"

String fallback_resources[] = {
#include "../xcommon/xcommon-ad.h"
#include "../xcommon/filesel-ad.h"
#include "../xcommon/dialogs-ad.h"
#include "../xcommon/nws-ad.h"
#include "gvu-ad.h"
    NULL
};

#define ROWS 22
#define COLS 68

static void Syntax();

float	default_xdpi;	/* default xdpi from ghostview widget */
float	default_ydpi;	/* default ydpi from ghostview widget */

int	num_ghosts;	/* number of ghostview widgets active */
FILE	*psfile;	/* file to display */
String	filename;	/* its filename */
String	oldfilename;	/* previous filename */
int	current_page;	/* current page being displayed */
int	current_magstep;/* current magnification */
XtPageOrientation	current_orientation; /* current orientation */
int	default_pagemedia;	/* index into document media or papersizes */
int	current_pagemedia;	/* index into document media or papersizes */
Boolean	force_document_media;	/* Whether to force a document media */
int	document_media;		/* document media being forced */
int	current_llx;		/* current bounding box */
int	current_lly;
int	current_urx;
int	current_ury;
int	base_papersize;		/* tells where papersizes begins */
Boolean	info_up;		/* tells if information window is up */
int	force_setting;		/* tells if new setting override %%comments */
Icon	*blank_bitmap;
Icon	*dot_bitmap;		/* pixmap used to mark default setting */
Icon	*tie_fighter_bitmap;	/* bitmap used to mark forced setting */
String	toc_text;	/* page labels (Table of Contents) */
int	toc_length;	/* length of page label text */
int	toc_entry_length; /* length of one entry */
int	info_length;	/* number of bytes in infotext window */
int	mode;	/* tells what type of popup */
time_t	mtime;		/* last modified time of input file */
struct document *doc;	/* document structure */
struct document *olddoc;	/* document structure */
Atom	wm_delete_window;	/* Atom sent to destroy a window */
XErrorHandler	old_Xerror;	/* standard error handler */
Boolean	dying;		/* whether an X error caused our exit */
XErrorEvent	bomb;	/* what the error was */

XtAppContext app_con;
AppResources app_res;
int bars = 0;

/* Widgets used.  Indentation indicates hierarchy */
        Widget toplevel;
        Widget     topbox;
	Widget         menubar;
	Widget		   menubox;
	Widget	       toolbar;
	Widget		   toolbox;
	Widget	       mainarea;
        Widget         titlebutton;
        Widget             titlemenu;
        Widget         datebutton;
        Widget             datemenu;
        Widget         box;
        Widget             filebutton;
        Widget                 filemenu;
        Widget                     openbutton;
        Widget                     reopenbutton;
        Widget                     printwholebutton;
        Widget                     printmarkedbutton;
        Widget                     savebutton;
        Widget                     quitbutton;
        Widget             pagebutton;
        Widget                 pagemenu;
        Widget                     nextbutton;
        Widget                     showbutton;
        Widget                     prevbutton;
        Widget                     centerbutton;
        Widget                     markbutton;
        Widget                     unmarkbutton;
        Widget             magstepbutton;
        Widget                 magstepmenu;
        Widget                     *magstepentry;
        Widget             orientationbutton;
        Widget                 orientationmenu;
        Widget                     portraitbutton;
        Widget                     landscapebutton;
        Widget                     upsidedownbutton;
        Widget                     seascapebutton;
        Widget                     swapbutton;
        Widget             pagemediabutton;
        Widget                 pagemediamenu;
        Widget                     *pagemediaentry;
        Widget             antialiasingbutton;
        Widget                 antialiasingmenu;
        Widget                     *antialiasingentry;
	Widget		   helpbutton;
	Widget		       helpmenu;
	Widget			   contentsbutton;
        Widget                     copyrightbutton;
        Widget         toc;
        Widget         pageview;
        Widget             page;
	Widget	       status;
	Widget		   label2;
        Widget         	   locator;

/* Popup children */
        Widget infopopup;
        Widget     infoform;
        Widget         infotext;
        Widget         infodismiss;
        Widget dialogpopup;
        Widget     dialog;

Widget tooltip;

static Widget make_command(void (*cmd)(), Widget pw, char *pm, char *tt)
{
        Widget w;
        Pixmap pm_return;
        Pixel color;

        XtVaGetValues(pw, XtNbackground, &color, (char *)0);

        w = XtVaCreateManagedWidget("toolbar_command",
                commandWidgetClass, pw,
                XtNforeground, color,
                (char *)NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);

        XtVaSetValues(w,
                XtNbitmap, pm_return,
                (char *)0);

        XtAddCallback(w,
                XtNcallback, cmd, (XtPointer)NULL);
	MwTooltipAdd(tooltip, w, _(tt));
        return w;
}

static void make_vsep(Widget pw)
{
	unsigned long bg;

	XtVaGetValues(topbox,
		XtNbackground, &bg,
		(char *)0);
	XtVaCreateManagedWidget("vsep",
		labelWidgetClass, pw,
		XtNborderColor, bg,
		(char *)0);
}

/* There is plenty to do here */
static void create_toolbar(Widget pw)
{
	make_command(open_cb, pw, "fld_open.xpm",
		     "Open a PostScript file");
	make_command(save_cb, pw, "save.xpm",
		     "Save the marked pages");
	make_command(printmarked_cb, pw, "printer.xpm",
		     "Print the marked pages");
	make_vsep(pw);
	make_command(prev_page, pw, "previous.xpm", "Previous page");
	make_command(next_page, pw, "next.xpm", "Next page");
	make_command(center_page, pw, "center.xpm", "Center page");
	make_vsep(pw);
	make_command(larger_cb, pw, "larger.xpm", "Increase magstep");
	make_command(smaller_cb, pw, "smaller.xpm", "Decrease magstep");
	make_command(portrait_cb, pw, "portrait.xpm", "Portrait");
	make_command(landscape_cb, pw, "landscape.xpm", "Landscape");
	make_vsep(pw);
	make_command(help_cb, pw, "info.xpm",
		     "Display online help");
	make_command(copyright_cb, pw, "copyright.xpm",
		     "Display copyright information");
}

static Widget add_menu_entry(Widget pw, char *name, char *label,
	     void (*cb) (Widget, XtPointer, XtPointer), XtPointer * data)
{
	Widget w;
	w = XtVaCreateManagedWidget(name,
		mwLabelMEObjectClass, pw,
		XtNlabel, _(label),
		(char *)0);
	XtAddCallback(w, XtNcallback, cb, data);
	return w;
}

static void remake_ylayout(void)
{
	char b[100];
	sprintf(b, "%s %s 100%% 30",
		(bars&MENUBAR) ? "30" : "0",
		(bars&TOOLBAR) ? "30" : "0");
	XtVaSetValues(topbox,
		XtNyLayout, b,
		(char *)0);
}

static void attach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget vw = (Widget)call_data;
	if (vw == menubar) bars |= MENUBAR;
	if (vw == toolbar) bars |= TOOLBAR;
	remake_ylayout();
}

static void detach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget vw = (Widget)call_data;
	if (vw == menubar) bars &= ~MENUBAR;
	if (vw == toolbar) bars &= ~TOOLBAR;
	remake_ylayout();
}

int main(int argc, char **argv)
{
    struct stat sbuf;
    Display *dpy;
    Screen *scr;
    Arg args[20];
    Cardinal num_args;
    Widget above_toc;
    Widget left_of_page;
    Widget line;
    Widget aboutbutton;
    int i;
    Boolean set_vert_dist;
    String s1, s2;
    XFontStruct *font;
    Dimension bottomMargin, leftMargin, rightMargin, topMargin;
    Dimension width, height;
    static String nothing = "";
    static XawTextSelectType sarry[] =
	    {XawselectLine, XawselectAll, XawselectNull};

    common_init("Gvu %s. No Warranty.");

    XtToolkitInitialize();
    XtSetTypeConverter(XtRString, XtRPageOrientation,
		       XmuCvtStringToPageOrientation, NULL, 0,
		       XtCacheAll, NULL);
    app_con = XtCreateApplicationContext();
    add_actions(app_con);
    XtAppSetFallbackResources(app_con, fallback_resources);
    dpy = XtOpenDisplay(app_con, NULL, NULL, "Gvu",
			options, XtNumber(options), &argc, argv);
    if (dpy == NULL) {
	fprintf(stderr, "%s: cannot open DISPLAY.\n", argv[0]);
	exit(1);
    }
    if (argc > 2) Syntax(argv[0]);
    if (argc == 2) {
	filename = XtNewString(argv[1]);
	if (strcmp(filename, "-")) {
#ifdef VMS
	    if ((psfile = fopen(argv[1], "r", "mbc=100")) == NULL) {
#else
	    if ((psfile = fopen(argv[1], "r")) == NULL) {
#endif
		fprintf(stderr, "Cannot open ");
		perror(argv[1]);
		exit(1);
	    }
	    stat(filename, &sbuf);
	    mtime = sbuf.st_mtime;
	}
    }

    old_Xerror = XSetErrorHandler(catch_Xerror);

    scr = DefaultScreenOfDisplay(dpy);
    wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);

    toplevel = XtAppCreateShell(NULL, "Gvu", applicationShellWidgetClass,
				dpy, NULL, ZERO);

    theme_init(dpy);

    XtGetApplicationResources(toplevel, (XtPointer) &app_res,
			      resources, XtNumber(resources), NULL, ZERO);
    if ((s1 = getenv(app_res.printer_variable))) app_res.default_printer = s1;

    /* Open Windows sometimes hands me a bad bitmap */
    if (app_res.openwindows) {
	blank_bitmap = dot_bitmap = tie_fighter_bitmap = None;
    } else {
	blank_bitmap = X_XpmDataToIcon(XtDisplay(toplevel), blank_xpm);
	dot_bitmap = X_XpmDataToIcon(XtDisplay(toplevel), dot_xpm);
	tie_fighter_bitmap = X_XpmDataToIcon(XtDisplay(toplevel), dot_xpm);
    }

	MwHighlightInit(toplevel);
	tooltip = XtVaCreatePopupShell("tooltip",
		mwTooltipWidgetClass, toplevel,
		(char *)0);
    
    /* Instantiate Popup children */
    infopopup = XtCreatePopupShell("information", topLevelShellWidgetClass,
				   toplevel, NULL, ZERO);

    infoform = XtCreateManagedWidget("form", formWidgetClass,
				     infopopup, NULL, ZERO);

							num_args = 0;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    XtSetArg(args[num_args], XtNscrollHorizontal, XawtextScrollWhenNeeded);
							num_args++;
    XtSetArg(args[num_args], XtNscrollVertical, XawtextScrollWhenNeeded);
							num_args++;
    XtSetArg(args[num_args], XtNdisplayCaret, False);	num_args++;
    infotext = XtCreateManagedWidget("text", asciiTextWidgetClass,
				     infoform, args, num_args);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, infotext);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    infodismiss = XtCreateManagedWidget("dismiss", commandWidgetClass,
				       infoform, args, num_args);
    XtAddCallback(infodismiss, XtNcallback, dismiss, (XtPointer)infopopup);

							num_args = 0;
    XtSetArg(args[num_args], XtNfont, &font);		num_args++;
    XtSetArg(args[num_args], XtNbottomMargin, &bottomMargin);num_args++;
    XtSetArg(args[num_args], XtNleftMargin, &leftMargin);num_args++;
    XtSetArg(args[num_args], XtNrightMargin, &rightMargin);num_args++;
    XtSetArg(args[num_args], XtNtopMargin, &topMargin);	num_args++;
    XtGetValues(infotext, args, num_args);

    width = font->max_bounds.width * 80 + leftMargin + rightMargin;
    height = (font->ascent + font->descent) * ROWS + topMargin + bottomMargin;

    XtSetArg(args[0], XtNwidth, width);
    XtSetArg(args[1], XtNheight, height);
    XtSetValues(infotext, args, TWO);
    XtSetValues(infodismiss, args, ONE);
    XtRealizeWidget(infopopup);
    XSetWMProtocols(dpy, XtWindow(infopopup), &wm_delete_window, 1);

    dialogpopup = XtCreatePopupShell("popup", transientShellWidgetClass,
				     toplevel, NULL, ZERO);

    dialog = CreateDialog(dialogpopup, "dialog", okay, dismiss);
    XtRealizeWidget(dialogpopup);
    XSetWMProtocols(dpy, XtWindow(dialogpopup), &wm_delete_window, 1);


    /* Instantiate Widgets */

	topbox = XtVaCreateManagedWidget("topbox",
		mwRudegridWidgetClass, toplevel,
		(char *)0);
	menubar = XtVaCreateManagedWidget("menubar",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(menubar, menubar, detach_bar, attach_bar);
	menubox = XtVaCreateManagedWidget("menubox",
		mwMenuBarWidgetClass, menubar,
		XtNgridx, 1,
		(char *)0);
	bars |= MENUBAR;
	toolbar = XtVaCreateManagedWidget("toolbar",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(toolbar, toolbar, detach_bar, attach_bar);
	toolbox = XtVaCreateManagedWidget("frame2",
		mwFrameWidgetClass, toolbar,
		XtNgridx, 1,
		XtNshadowWidth, 1,
		(char *)0);
	toolbox = XtVaCreateManagedWidget("toolbox",
		boxWidgetClass, toolbox,
		(char *)0);
	bars |= TOOLBAR;
	create_toolbar(toolbox);
	mainarea = XtVaCreateManagedWidget("mainarea",
		mwRudegridWidgetClass, topbox,
		(char *)0);
	status = XtVaCreateManagedWidget("status",
		mwRudegridWidgetClass, topbox,
		(char *)0);
	label2 = XtVaCreateManagedWidget("label2",
		labelWidgetClass, status,
		(char *)0);

    above_toc = NULL;
    left_of_page = NULL;
    set_vert_dist = False;

    if (app_res.show_locator) {
	locator = XtCreateManagedWidget("locator", labelWidgetClass,
					status, NULL, ZERO);
	above_toc = locator;
	if (!left_of_page) left_of_page = locator;
    }

    XtSetArg(args[0], XtNresize, False);
    filebutton = XtVaCreateManagedWidget("fileButton",
			mwMBButtonObjectClass, menubox,
			XtNlabel, _("File"),
			XtNmenu_name, "file_menu",
			(char *)0);

    filemenu = XtCreatePopupShell("file_menu", mwMenuWidgetClass,
				  menubox, NULL, ZERO);

	openbutton = add_menu_entry(filemenu, "open", "Open...",
		open_cb, NULL);
	reopenbutton = add_menu_entry(filemenu, "reopen", "Reopen",
		reopen_file, NULL);
	printwholebutton = add_menu_entry(filemenu, "printwhole", "Print...",
		printwhole_cb, NULL);
	printmarkedbutton = add_menu_entry(filemenu, "printmarked",
		"Print marked pages...", printmarked_cb, NULL);
	savebutton = add_menu_entry(filemenu, "save", "Save marked pages...",
		save_cb, NULL);


    line = XtCreateManagedWidget("line", mwLineMEObjectClass,
				 filemenu, NULL, ZERO);

	quitbutton = add_menu_entry(filemenu, "quit", "Quit",
		quit_ghostview, NULL);

    pagebutton = XtVaCreateManagedWidget("pageButton",
			mwMBButtonObjectClass, menubox,
			XtNlabel, _("Page"),
			XtNmenu_name, "page_menu",
			(char *)0);

    pagemenu = XtCreatePopupShell("page_menu", mwMenuWidgetClass,
				  menubox, NULL, ZERO);

	nextbutton = add_menu_entry(pagemenu, "next", "Next",
		next_page, NULL);
	showbutton = add_menu_entry(pagemenu, "show", "Redisplay",
		this_page, NULL);
	prevbutton = add_menu_entry(pagemenu, "prev", "Previous",
		prev_page, NULL);

    line = XtCreateManagedWidget("line", mwLineMEObjectClass,
				 pagemenu, NULL, ZERO);

	centerbutton = add_menu_entry(pagemenu, "center", "Center",
		center_page, NULL);

    line = XtCreateManagedWidget("line", mwLineMEObjectClass,
				 pagemenu, NULL, ZERO);

	markbutton = add_menu_entry(pagemenu, "mark", "Mark",
		mark_page, NULL);
	unmarkbutton = add_menu_entry(pagemenu, "unmark", "Unmark",
		unmark_page, NULL);

    magstepbutton = XtVaCreateManagedWidget("magstepButton",
		mwMBButtonObjectClass, menubox,
		XtNlabel, _("Magstep"),
		XtNmenu_name, "magstep_menu",
		(char *)0);

    magstepmenu = XtCreatePopupShell("magstep_menu", mwMenuWidgetClass,
				     menubox, NULL, ZERO);

    magstepentry = (Widget *) XtMalloc(
	    (app_res.maximum_magstep - app_res.minimum_magstep + 1) *
	    sizeof(Widget));
    for (i = app_res.minimum_magstep; i <= app_res.maximum_magstep; i++) {
	char buf[16];
	sprintf(buf, "%d", i);
	magstepentry[i-app_res.minimum_magstep] =
		XtVaCreateManagedWidget(buf,
			mwLabelMEObjectClass, magstepmenu,
			XtNlabel, buf,
			XtNleft_icon, blank_bitmap,
			(char *)0);
	XtAddCallback(magstepentry[i-app_res.minimum_magstep], XtNcallback,
		      set_magstep, (XtPointer)i);
    }

    orientationbutton = XtVaCreateManagedWidget("orientationButton",
				mwMBButtonObjectClass, menubox,
				XtNmenu_name, "orientation_menu",
				XtNlabel, _("Orientation"),
				(char *)0);

    orientationmenu = XtVaCreatePopupShell("orientation_menu",
				mwMenuWidgetClass, menubox,
				(char *)0);

    portraitbutton = XtVaCreateManagedWidget("portrait",
				mwLabelMEObjectClass, orientationmenu,
				XtNlabel, _("Portrait"),
				XtNleft_icon, blank_bitmap,
				(char *)0);
    XtAddCallback(portraitbutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationPortrait);

    landscapebutton = XtVaCreateManagedWidget("landscape",
				mwLabelMEObjectClass, orientationmenu,
				XtNlabel, _("Landscape"),
				XtNleft_icon, blank_bitmap,
				(char *)0);
    XtAddCallback(landscapebutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationLandscape);

    upsidedownbutton = XtVaCreateManagedWidget("upsidedown",
				mwLabelMEObjectClass, orientationmenu,
				XtNlabel, _("Upside-down"),
				XtNleft_icon, blank_bitmap,
				(char *)0);
    XtAddCallback(upsidedownbutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationUpsideDown);

    seascapebutton = XtVaCreateManagedWidget("seascape",
				mwLabelMEObjectClass, orientationmenu,
				XtNlabel, _("Seascape"),
				XtNleft_icon, blank_bitmap,
				(char *)0);
    XtAddCallback(seascapebutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationSeascape);

    line = XtCreateManagedWidget("line", mwLineMEObjectClass,
				 orientationmenu, NULL, ZERO);

    swapbutton = XtVaCreateManagedWidget("swap",
				mwLabelMEObjectClass, orientationmenu,
				XtNlabel, _("Swap Landscape"),
				XtNleft_icon, blank_bitmap,
				(char *)0);
    XtAddCallback(swapbutton, XtNcallback, swap_landscape, (XtPointer)0);

    if (app_res.swap_landscape) {
	XtSetArg(args[0], XtNlabel, &s1);
	XtGetValues(landscapebutton, args, ONE);
	s1 = XtNewString(s1);
	XtSetArg(args[0], XtNlabel, &s2);
	XtGetValues(seascapebutton, args, ONE);
	s2 = XtNewString(s2);
	XtSetArg(args[0], XtNlabel, s2);
	XtSetValues(landscapebutton, args, ONE);
	XtSetArg(args[0], XtNlabel, s1);
	XtSetValues(seascapebutton, args, ONE);
	XtFree(s1);
	XtFree(s2);
    }

    pagemediabutton = XtVaCreateManagedWidget("pagemediaButton",
			mwMBButtonObjectClass, menubox,
			XtNlabel, _("Media"),
			XtNmenu_name, "pagemedia_menu",
			(char *)0);

    default_pagemedia = 0;
    for (i = 0; papersizes[i].name; i++) {
        if (!strcmp(app_res.pagemedia, papersizes[i].name)) {
	    default_pagemedia = i;
            break;
        }
    }

    antialiasingbutton = XtVaCreateManagedWidget("antialiasingButton",
			mwMBButtonObjectClass, menubox,
			XtNlabel, _("Antialiasing"),
			XtNmenu_name, "antialiasing_menu",
			(char *)0);

    helpbutton = XtVaCreateManagedWidget("helpButton",
		mwMBButtonObjectClass, menubox,
		XtNlabel, _("Help"),
		XtNmenu_name, "help_menu",
		XtNgravitation, XtCright,
		(char *)0);
    helpmenu = XtCreatePopupShell("help_menu", mwMenuWidgetClass,
	        menubox, NULL, ZERO);
    helpbutton = XtVaCreateManagedWidget("help",
		mwLabelMEObjectClass, helpmenu,
		XtNlabel, _("Contents"),
		(char *)0);
    XtAddCallback(helpbutton, XtNcallback, help_cb, NULL);
    copyrightbutton = XtVaCreateManagedWidget("copyright",
		mwLabelMEObjectClass, helpmenu,
		XtNlabel, _("Copyright"),
		(char *)0);
    XtAddCallback(copyrightbutton, XtNcallback, copyright_cb, NULL);

    XtVaCreateManagedWidget("line",
		mwLineMEObjectClass, helpmenu,
		(char *)0);

    aboutbutton = XtVaCreateManagedWidget("aboutgvu",
		mwLabelMEObjectClass, helpmenu,
		XtNlabel, _("About Gvu..."),
		(char *)0);
    XtAddCallback(aboutbutton, XtNcallback, aboutgvu_cb, NULL);

    aboutbutton = XtVaCreateManagedWidget("aboutsiag",
		mwLabelMEObjectClass, helpmenu,
		XtNlabel, _("About Siag Office..."),
		(char *)0);
    XtAddCallback(aboutbutton, XtNcallback, aboutsiag_cb, NULL);

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

							num_args = 0;
    XtSetArg(args[num_args], XtNdisplayCaret, False);	num_args++;
    XtSetArg(args[num_args], XtNuseStringInPlace, True);num_args++;
    XtSetArg(args[num_args], XtNlength, 0);		num_args++;
    XtSetArg(args[num_args], XtNstring, nothing);	num_args++;
    XtSetArg(args[num_args], XtNselectTypes, sarry);	num_args++;
    XtSetArg(args[num_args], XtNscrollVertical, XawtextScrollAlways);num_args++;
    toc = XtCreateManagedWidget("toc", asciiTextWidgetClass,
				mainarea, args, num_args);
    if (!left_of_page) left_of_page = toc;

							num_args = 0;
    XtSetArg(args[num_args], XtNallowHoriz, True);	num_args++;
    XtSetArg(args[num_args], XtNallowVert, True);	num_args++;
    pageview = XtCreateManagedWidget("pageview", viewportWidgetClass,
				     mainarea, args, num_args);

    page = XtCreateManagedWidget("page", ghostviewWidgetClass,
				 pageview, NULL, ZERO);
    num_ghosts++;
    XtAddCallback(page, XtNcallback, track_and_zoom, (XtPointer)0);
    XtAddCallback(page, XtNdestroyCallback, destroy_ghost, (XtPointer)page);
    XtAddCallback(page, XtNmessageCallback, message, (XtPointer)page);
    XtAddCallback(page, XtNoutputCallback, output, (XtPointer)0);
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

    /* Coerce page number to fall in range */
    if (toc_text) {
	if (i >= doc->numpages) i = doc->numpages - 1;
	if (i < 0) i = 0;
    }
    /* Coerce magstep to fall in range */
    if (app_res.magstep < app_res.minimum_magstep)
	app_res.magstep = app_res.minimum_magstep;
    if (app_res.magstep > app_res.maximum_magstep)
	app_res.magstep = app_res.maximum_magstep;
    set_new_magstep();
    set_new_orientation(i);
    set_new_pagemedia(i);
    layout_ghostview();

    XtSetMappedWhenManaged(toplevel, False);
    XtRealizeWidget(toplevel);

    XtSetArg(args[0], XtNtransientFor, toplevel);
    XtSetValues(dialogpopup, args, ONE);
    XSetWMProtocols(dpy, XtWindow(toplevel), &wm_delete_window, 1);

    /* This sets the sizes on widget that were created during the realize. */
    layout_ghostview();
    XtMapWidget(toplevel);

    show_page(i);
    XtAppMainLoop(app_con);

    /* should never get here */
    return 1;
}

static void
Syntax(call)
char *call;
{
    XtDestroyApplicationContext(app_con);
    fprintf(stderr, "Usage: %s\n", call);
    fprintf(stderr,
		"    [-monochrome] [-grayscale] [-color]\n"
		"    [-[no]title] [-[no]date] [-[no]locator] [-[no]labels]\n"
		"    [-resolution <dpi>] [-dpi <dpi>]\n"
		"    [-xdpi <dpi>] [-ydpi <dpi>] [-magstep <n>]\n"
		"    [-[no]safer] [-[no]alpha] [-[no]quiet] [-[no]center]\n"
		"    [-arguments <arguments>]\n"
		"    [-portrait] [-landscape] [-upsidedown] [-seascape] [-[no]swap]\n"
		"    [-letter] [-tabloid] [-ledger] [-legal] [-statement]\n"
		"    [-executive] [-a3] [-a4] [-a5] [-b4] [-b5]\n"
		"    [-folio] [-quarto] [-10x14]\n"
		"    [-force] [-forceorientation] [-forcemedia]\n"
		"    [-[no]openwindows] [-[no]ncdwm]\n"
		"    [-page <label>] [toolkitoption]\n"
		"    [filename]\n");
    exit(1);
}
