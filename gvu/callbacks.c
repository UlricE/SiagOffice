/*
 * callbacks.c -- X11 callbacks for ghostview.
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
 */

#include <stdio.h>
#include <stdlib.h>
#ifndef BUFSIZ
#define BUFSIZ 1024
#endif

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../xcommon/xcommon.h"

#include "gv.h"
#include "../common/ps.h"
#include <Mowitz/Mowitz.h>

/* from dialogs.c */
static String okay_accelerators =
    "#override\n\
     <Key>Return: set() notify() unset()\n";

/* Create a dialog widget */
/* It is just a form widget with
 *   a label prompt
 *   a text response
 *   an oky button
 *   a cancel button */
Widget
CreateDialog(parent, name, okay_callback, cancel_callback)
    Widget parent;
    String name;
    XtCallbackProc okay_callback;
    XtCallbackProc cancel_callback;
{
    Widget form, prompt, response, okay, cancel;
    Arg args[20];
    Cardinal num_args;

    form = XtCreateManagedWidget(name, formWidgetClass, parent, NULL, ZERO);

							num_args = 0;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    XtSetArg(args[num_args], XtNborderWidth, 0);	num_args++;
    prompt = XtCreateManagedWidget("prompt", labelWidgetClass,
				   form, args, num_args);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, prompt);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    XtSetArg(args[num_args], XtNeditType, XawtextEdit);	num_args++;
    XtSetArg(args[num_args], XtNresize, XawtextResizeWidth);	num_args++;
    XtSetArg(args[num_args], XtNstring, "");		num_args++;
    response = XtCreateManagedWidget("response", asciiTextWidgetClass,
				     form, args, num_args);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, response);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    XtSetArg(args[num_args], XtNaccelerators,
	     XtParseAcceleratorTable(okay_accelerators));	num_args++;
    okay = XtCreateManagedWidget("okay", commandWidgetClass,
				 form, args, num_args);
    MwLabelSet(okay, "OK");
    XtAddCallback(okay, XtNcallback, okay_callback, form);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, response);	num_args++;
    XtSetArg(args[num_args], XtNfromHoriz, okay);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    cancel = XtCreateManagedWidget("cancel", commandWidgetClass,
				   form, args, num_args);
    MwLabelSet(cancel, "Cancel");
    XtAddCallback(cancel, XtNcallback, cancel_callback, parent);

    XtInstallAccelerators(response, okay);
    XtSetKeyboardFocus(form, response);

    return form;
}

/* set the prompt.  This is used to put error information in the prompt */
static void
SetDialogPrompt(w, newprompt)
    Widget w;
    String newprompt;
{
    Arg args[1];
    Widget label;

    label = XtNameToWidget(w, "prompt");
    XtSetArg(args[0], XtNlabel, _(newprompt));
    XtSetValues(label, args, ONE);
}

/* get what the user typed */
static String
GetDialogResponse(w)
    Widget w;
{
    Arg args[1];
    Widget response;
    String s;

    response = XtNameToWidget(w, "response");
    XtSetArg(args[0], XtNstring, &s);
    XtGetValues(response, args, ONE);
    return XtNewString(s);
}

/* set the default reponse */
static void
SetDialogResponse(w, s)
    Widget w;
    String s;
{
    Arg args[3];
    Widget response;
    XFontStruct *font;
    Dimension width, leftMargin, rightMargin;

    response = XtNameToWidget(w, "response");
    XtSetArg(args[0], XtNfont, &font);
    XtSetArg(args[1], XtNleftMargin, &leftMargin);
    XtSetArg(args[2], XtNrightMargin, &rightMargin);
    XtGetValues(response, args, THREE);
    width = font->max_bounds.width * strlen(s) + leftMargin + rightMargin;

    XtSetArg(args[0], XtNstring, s);
    XtSetArg(args[1], XtNwidth, width);
    XtSetValues(response, args, TWO);
    XawTextSetInsertionPoint(response, strlen(s));

}

/* clear the response */
static void
ClearDialogResponse(w)
    Widget w;
{
    Arg args[2];
    Widget response;

    response = XtNameToWidget(w, "response");
    XtSetArg(args[0], XtNstring, "");
    XtSetArg(args[1], XtNwidth, 100);
    XtSetValues(response, args, TWO);
}

/* end of dialogs.c */

/* Start application folding up by Destroying the top level widget. */
/* The application exits when the last interpreter is killed during */
/* a destroy callback from ghostview widgets. */
void
quit_ghostview(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XtDestroyWidget(toplevel);
}

/* Popup a window. */
void
popup(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    positionpopup((Widget)client_data);
    XtPopup((Widget)client_data, XtGrabNone);
    XRaiseWindow(XtDisplay((Widget)client_data), XtWindow((Widget)client_data));
}

/* Popup a dialog box. */
void
popup_dialog(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    mode = (int) client_data;
    switch (mode) {

    case PRINT_WHOLE:
    case PRINT_MARKED:
	SetDialogPrompt(dialog, app_res.print_prompt);
	if (app_res.default_printer) SetDialogResponse(dialog, app_res.default_printer);
	else ClearDialogResponse(dialog);
	popup(w, (XtPointer)dialogpopup, call_data);
	break;

    case OPEN:
fprintf(stderr, "Open file here. Is this code strange or what.\n");
	break;

    case SAVE:
	SetDialogPrompt(dialog, app_res.save_prompt);
	ClearDialogResponse(dialog);
	popup(w, dialogpopup, call_data);
	break;
    }
}

static char *select_file(int e)
{
	static char path[1024], name[1024];
	static char *patterns[] = {
		"PostScript files (*.ps)",
		"Encapsulated PostScript (*.eps*)",
		"All files (*)",
		NULL
	};
	static int need_init = 1;
	int n;
	char fmt[80];
	char extra[1024];
	char fn[1024];

	sprintf(extra, "Home=%s", getenv("HOME"));
	if (need_init) {
		getcwd(path, sizeof path);
		need_init = 0;
		name[0] = '\0';
	}
	n = MwFileselInput(toplevel, path, name, patterns, fmt, extra, e);

	if (n) {
		sprintf(fn, "%s/%s", path, name);
		return MwStrdup(fn);
	}
	return NULL;
}

/* callbacks to wrap the above function */
void open_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	FILE *fd;
	struct stat st;
	char *fn = select_file(0);
	if (fn && ((fd = fopen(fn, "r")))) {
		if (oldfilename) XtFree(oldfilename);
		oldfilename = NULL;
		if (filename) {
			oldfilename = XtNewString(filename);
			XtFree(filename);
		}
		filename = XtNewString(fn);
		MwFree(fn);
		if (psfile) fclose(psfile);
		psfile = fd;
		stat(filename, &st);
		mtime = st.st_mtime;
		new_file(0);
		show_page(0);
	}
}

void save_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	String error;
	char *fn = select_file(1);
	if (!fn) return;
	if ((error = save_file(fn))) {
	    char *buf = XtMalloc(strlen(error) + strlen(fn) + 2);
	    sprintf(buf, "%s\n%s", error, fn);
	    MwErrorBox(toplevel, buf);
	    XtFree(error);
	    XtFree(buf);
	}
	MwFree(fn);
}

void printmarked_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	popup_dialog(w, (XtPointer)PRINT_MARKED, call_data);
}

void printwhole_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	popup_dialog(w, (XtPointer)PRINT_WHOLE, call_data);
}

void larger_cb(Widget w, XtPointer client, XtPointer call)
{
	int i = current_magstep+1;
	if (i <= app_res.maximum_magstep) set_magstep(w, (XtPointer)i, call);
}

void smaller_cb(Widget w, XtPointer client, XtPointer call)
{
	int i = current_magstep-1;
	if (i >= app_res.minimum_magstep) set_magstep(w, (XtPointer)i, call);
}

void portrait_cb(Widget w, XtPointer client, XtPointer call)
{
	set_orientation(w, (XtPointer)XtPageOrientationPortrait, call);
}

void landscape_cb(Widget w, XtPointer client, XtPointer call)
{
	set_orientation(w, (XtPointer)XtPageOrientationLandscape, call);
}

static void do_help(char *p)
{
	char b[1024];

	sprintf(b, "siagrun help file:%s/%s", docdir, p);
	MwSpawn(b);
}

void help_cb(Widget w, XtPointer client, XtPointer call)
{
	do_help("gvu/gvu.html");
}

void copyright_cb(Widget w, XtPointer client, XtPointer call)
{
	do_help("gvu/copyright.html");
}

void aboutgvu_cb(Widget w, XtPointer client, XtPointer call)
{
	char msg[1024];
	sprintf(msg, "%s\n\n%s\n\nUlric Eriksson, ulric@siag.nu\n\n%s\n",
		version,
		_("A postscript viewer for X"),
		_("Part of Siag Office"));
	MwAboutBox(toplevel, "gvu.xpm", msg);
}

void aboutsiag_cb(Widget w, XtPointer client, XtPointer call)
{
	MwAboutSiag(toplevel);
}

/* Explicitly reopen the file. */
void reopen_file(Widget w, XtPointer client_data, XtPointer call_data)
{
    struct stat sbuf;
    int number = current_page;

    fclose(psfile);
    psfile = fopen(filename, "r");
    mtime = sbuf.st_mtime;
    if (oldfilename) XtFree(oldfilename);
    oldfilename = XtNewString(filename);
    new_file(number);
    show_page(number);
}

/* Get the selection, if no selection, get the insertion point. */
/* If the new_page is different from the current page show it.  */
/* If not at the first page, show the previous page. */
void
prev_page(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition pos, end;
    int new_page;

    XawTextGetSelectionPos(toc, &pos, &end);
    if (pos == end) {		/* Nothing selected */
	pos = XawTextGetInsertionPoint(toc);
    }
    if ((new_page = pos/toc_entry_length) == current_page) {
	new_page = current_page - 1;
    }
    if (new_page < 0) return;
    show_page(new_page);
}

/* Get the selection, if no selection, get the insertion point. */
/* Show this page.  */
void
this_page(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    if (toc_text) {
	XawTextPosition pos, end;
	int new_page;

	XawTextGetSelectionPos(toc, &pos, &end);
	if (pos == end) {		/* Nothing selected */
	    pos = XawTextGetInsertionPoint(toc);
	}
	new_page = pos/toc_entry_length;
	show_page(new_page);
    } else {
	GhostviewDisableInterpreter(page);
	show_page(0);
    }
}

/* Get the selection, if no selection, get the insertion point. */
/* If the new_page is different from the current page show it.  */
/* If not at the last page, show the next page. */
void
next_page(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition pos, end;
    int new_page = 0;

    if (toc_text) {
	XawTextGetSelectionPos(toc, &pos, &end);
	if (pos == end) {		/* Nothing selected */
	    pos = XawTextGetInsertionPoint(toc);
	}
	if ((new_page = pos/toc_entry_length) == current_page) {
	    new_page = current_page + 1;
	}
	if (new_page >= doc->numpages) return;
    }
    show_page(new_page);
}

/* Center the viewport over the page */
void
center_page(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[2];
    Widget scroll;
    float top, shown;

    scroll = XtNameToWidget(pageview, "vertical");
    if (scroll) {
	XtSetArg(args[0], XtNshown, &shown);
	XtSetArg(args[1], XtNtopOfThumb, &top);
	XtGetValues(scroll, args, TWO);

	top = (1.0 - shown) / 2.0;
	XtCallCallbacks(scroll, XtNjumpProc, &top);
    }

    scroll = XtNameToWidget(pageview, "horizontal");
    if (scroll) {
	XtSetArg(args[0], XtNshown, &shown);
	XtSetArg(args[1], XtNtopOfThumb, &top);
	XtGetValues(scroll, args, TWO);

	top = (1.0 - shown) / 2.0;
	XtCallCallbacks(scroll, XtNjumpProc, &top);
    }
}

/* Get the selection, if no selection, get the insertion point. */
/* Mark all pages in range, and cause toc to update. */
void
mark_page(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition begin, end;
    int i;

    XawTextGetSelectionPos(toc, &begin, &end);
    if (begin == end) {		/* Nothing selected */
	begin = end = XawTextGetInsertionPoint(toc);
    } else {
	end--;			/* Sometimes end spills onto next line */
    }
    for (i = begin/toc_entry_length; i <= end/toc_entry_length; i++) {
	toc_text[i*toc_entry_length] = '*';
	XawTextInvalidate(toc, i*toc_entry_length, i*toc_entry_length+1);
    }
}

/* Get the selection, if no selection, get the insertion point. */
/* Unmark all pages in range, and cause toc to update. */
void
unmark_page(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XawTextPosition begin, end;
    int i;

    XawTextGetSelectionPos(toc, &begin, &end);
    if (begin == end) {		/* Nothing selected */
	begin = end = XawTextGetInsertionPoint(toc);
    } else {
	end--;			/* Sometimes end spills onto next line */
    }
    for (i = begin/toc_entry_length; i <= end/toc_entry_length; i++) {
	toc_text[i*toc_entry_length] = ' ';
	XawTextInvalidate(toc, i*toc_entry_length, i*toc_entry_length+1);
    }
}

/* Set new magstep.  Reshow the current page if magstep changed. */
void
set_magstep(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    app_res.magstep = (int)client_data;
    if (set_new_magstep()) {
	layout_ghostview();
	show_page(current_page);
    }
}

/* Set new orientation.  Reshow the current page if orientation changed. */
void
set_orientation(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    app_res.force_orientation = force_setting;
    app_res.orientation = (XtPageOrientation) client_data;
    if (set_new_orientation(current_page)) {
	layout_ghostview();
	show_page(current_page);
    }
}

/* Swap the landscape labels and change the flag. */
void
swap_landscape(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[1];
    String s1, s2;

    app_res.swap_landscape = !app_res.swap_landscape;

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

    if (set_new_orientation(current_page)) {
	layout_ghostview();
	show_page(current_page);
    }
}

/* Set new page media.  If new page media is different, update app_resources */
/* and redisplay page. */
void
set_pagemedia(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    int new_pagemedia = (int) client_data;
    if (new_pagemedia >= base_papersize) {
	default_pagemedia = new_pagemedia;
	app_res.force_pagemedia = force_setting;
    } else {
	document_media = new_pagemedia;
	force_document_media = force_setting;
    }
    if (set_new_pagemedia(current_page)) {
	layout_ghostview();
	show_page(current_page);
    }
}

void
set_antialiasing(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[4];
    int changed, old;

    old = app_res.antialiasing;
    app_res.antialiasing = 1 - (int) client_data;
    changed = (app_res.antialiasing != old);

    XtSetArg(args[0], XtNleft_icon, blank_bitmap);
    XtSetValues(antialiasingentry[(int)app_res.antialiasing], args, ONE);
    XtSetArg(args[0], XtNleft_icon, dot_bitmap);
    XtSetValues(antialiasingentry[1-app_res.antialiasing], args, ONE);
    if (changed) {
	if(app_res.antialiasing)
	  GhostviewSetAntialiasing(page); 
        else
	  GhostviewUnsetAntialiasing(page); 
	GhostviewDisableInterpreter(page);
	layout_ghostview();
	show_page(current_page);
    }
}


/* track mouse pointer and popup zoom window */
void
track_and_zoom(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[20];
    Cardinal num_args;
    Dimension width, height;
    Widget zoom;
    Widget zoomform;
    Widget zoompage;
    Widget zoomdismiss;
    FILE *zoomfile;
    struct stat sbuf;
    GhostviewReturnStruct *p = (GhostviewReturnStruct *)call_data;
    int llx;
    int lly;
    int urx;
    int ury;
    int bottom_margin;
    int left_margin;
    int right_margin;
    int top_margin;
    int i;

    /* locator events have zero width and height */
    if ((p->width == 0) || (p->height == 0)) {
	char buf[32];
	if (!app_res.show_locator) return;
	sprintf(buf, "(%d, %d)", p->psx, p->psy);
	XtSetArg(args[0], XtNlabel, buf);
	XtSetValues(locator, args, ONE);
	return;
    }

    /* If no file, nothing to zoom. */
    if (!psfile) return;

    /* If the file changed, cannot zoom */
    stat(filename, &sbuf);
    if (mtime != sbuf.st_mtime) return;
    zoom = XtCreatePopupShell("zoom", topLevelShellWidgetClass,
			      toplevel, NULL, ZERO);

    zoomform = XtCreateManagedWidget("form", formWidgetClass,
				     zoom, NULL, ZERO);

    llx = p->psx - p->width/2;
    lly = p->psy - p->height/2;
    urx = p->psx + p->width/2;
    ury = p->psy + p->height/2;

    /* Make sure zoom window doesn't go off the edge of the page */
    if (llx < current_llx) {
	llx = current_llx;
	urx = llx + p->width;
    }
    if (lly < current_lly) {
	lly = current_lly;
	ury = lly + p->height;
    }
    if (urx > current_urx) {
	urx = current_urx;
	llx = urx - p->width;
    }
    if (ury > current_ury) {
	ury = current_ury;
	lly = ury - p->height;
    }
    if (llx < current_llx) {
	llx = current_llx;
    }
    if (lly < current_lly) {
	lly = current_lly;
    }
    bottom_margin = lly - current_lly;
    left_margin = llx - current_llx;
    right_margin = current_urx - urx;
    top_margin = current_ury - ury;

							num_args = 0;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    XtSetArg(args[num_args], XtNorientation, current_orientation);
							num_args++;
    XtSetArg(args[num_args], XtNllx, llx);      	num_args++;
    XtSetArg(args[num_args], XtNlly, lly);      	num_args++;
    XtSetArg(args[num_args], XtNurx, urx);      	num_args++;
    XtSetArg(args[num_args], XtNury, ury);      	num_args++;
    XtSetArg(args[num_args], XtNbottomMargin, bottom_margin);
							num_args++;
    XtSetArg(args[num_args], XtNleftMargin, left_margin);
							num_args++;
    XtSetArg(args[num_args], XtNrightMargin, right_margin);
							num_args++;
    XtSetArg(args[num_args], XtNtopMargin, top_margin); num_args++;
    XtSetArg(args[num_args], XtNbottomMargin, bottom_margin);
							num_args++;
    XtSetFloatArg(args[num_args], XtNxdpi, p->xdpi);	num_args++;
    XtSetFloatArg(args[num_args], XtNydpi, p->ydpi);	num_args++;
    if (!toc_text) {
        XtSetArg(args[num_args], XtNfilename, filename);	num_args++;
    }
    zoompage = XtCreateManagedWidget("page", ghostviewWidgetClass,
				     zoomform, args, num_args);
    num_ghosts++;
    XtAddCallback(zoompage, XtNcallback, track_and_zoom, (XtPointer)0);
    XtAddCallback(zoompage, XtNmessageCallback, message, (XtPointer)zoompage);
    XtAddCallback(zoompage, XtNdestroyCallback, destroy_ghost,
		  (XtPointer)zoompage);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, zoompage);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    zoomdismiss = XtCreateManagedWidget("dismiss", commandWidgetClass,
				       zoomform, args, num_args);
    XtAddCallback(zoomdismiss, XtNcallback, destroy, (XtPointer)zoom);

    XtSetArg(args[0], XtNwidth, &width);
    XtGetValues(zoompage, args, ONE);
    XtSetArg(args[0], XtNwidth, width);
    XtSetValues(zoomdismiss, args, ONE);

    XtRealizeWidget(zoom);
    positionpopup(zoom);

							num_args = 0;
    XtSetArg(args[num_args], XtNwidth, &width);		num_args++;
    XtSetArg(args[num_args], XtNheight, &height);	num_args++;
    XtGetValues(zoom, args, num_args);

							      	num_args = 0;
    XtSetArg(args[num_args], XtNminWidth, width);		num_args++;
    XtSetArg(args[num_args], XtNminHeight, height);		num_args++;
    XtSetArg(args[num_args], XtNmaxWidth, width);		num_args++;
    XtSetArg(args[num_args], XtNmaxHeight, height);		num_args++;
    XtSetValues(zoom, args, num_args);
    XSetWMProtocols(XtDisplay(zoom), XtWindow(zoom), &wm_delete_window, 1);
    XtPopup(zoom, XtGrabNone);

    if (toc_text) {
	zoomfile = fopen(filename, "r");
	if (zoomfile == NULL) return;
	GhostviewSendPS(zoompage, zoomfile, doc->beginprolog,
			doc->lenprolog, False);
	GhostviewSendPS(zoompage, zoomfile, doc->beginsetup,
			doc->lensetup, False);
	if (doc->pageorder == DESCEND)
	    i = (doc->numpages - 1) - current_page;
	else
	    i = current_page;
	GhostviewSendPS(zoompage, zoomfile, doc->pages[i].begin,
			doc->pages[i].len, True);
    }
}

/* Process messages from ghostscript */
/* Refresh occurs when window was resized unexpectedly */
void
message(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    int i;
    char *error;

    if (!strcmp((char *) call_data, "Failed")) {
	if ((Widget)client_data == page) {
	    error = "Error: PostScript interpreter failed in main window.\n\n";
	} else {
	    error = "Error: PostScript interpreter failed in zoom window.\n\n";
	}
	output(w, NULL, error);
    } else if (!strcmp((char *) call_data, "BadAlloc")) {
	if ((Widget)client_data == page) {
	    error = 
	       "Warning: Could not allocate backing pixmap in main window.\n\n";
	} else {
	    error = 
	       "Warning: Could not allocate backing pixmap in zoom window.\n\n";
	}
	output(w, NULL, error);
    } else if (!strcmp((char *) call_data, "Refresh")) {
	if (toc_text) {
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
}

/* Take output from ghostscript and display it in the infotext popup window */
void
output(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    Arg args[2];
    XawTextBlock message_block;

    message_block.firstPos = 0;
    message_block.length = strlen(call_data);
    message_block.ptr = call_data;
    message_block.format = FMT8BIT;

    XawTextDisableRedisplay(infotext);

    XtSetArg(args[0], XtNeditType, XawtextAppend);
    XtSetValues(infotext, args, ONE);
    XawTextReplace(infotext, info_length, info_length, &message_block);
    info_length = info_length + message_block.length;

    XtSetArg(args[0], XtNeditType, XawtextRead);
    XtSetArg(args[1], XtNinsertPosition, info_length);
    XtSetValues(infotext, args, TWO);
    XawTextEnableRedisplay(infotext);
    if (!info_up) XtPopup(infopopup, XtGrabNone);
    info_up = True;
}

/* Dismiss popup dialog */
void
okay(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    String name, error;
    Widget dialog;

    dialog = (Widget)client_data;
    name = GetDialogResponse(dialog);
    switch (mode) {
    case PRINT_WHOLE:
    case PRINT_MARKED:
	if ((error = print_file(name, (mode == PRINT_WHOLE)))) {
	    char *buf = XtMalloc(strlen(error) +
				 strlen(app_res.print_prompt) + 2);
	    sprintf(buf, "%s\n%s", error, app_res.print_prompt);
	    SetDialogPrompt(dialog, buf);
	    XtFree(error);
	    XtFree(buf);
	} else {
	    XtPopdown(XtParent(dialog));
	}
	break;
    case OPEN:
	if ((error = open_file(name))) {
	    char *buf = XtMalloc(strlen(error) +
				 strlen(app_res.open_prompt) + 2);
	    sprintf(buf, "%s\n%s", error, app_res.open_prompt);
	    SetDialogPrompt(dialog, buf);
	    XtFree(error);
	    XtFree(buf);
	} else {
	    XtPopdown(XtParent(dialog));
	}
	break;
    case SAVE:
fprintf(stderr, "This is handled elsewhere\n");
	if ((error = save_file(name))) {
	    char *buf = XtMalloc(strlen(error) +
				 strlen(app_res.save_prompt) + 2);
	    sprintf(buf, "%s\n%s", error, app_res.save_prompt);
	    SetDialogPrompt(dialog, buf);
	    XtFree(error);
	    XtFree(buf);
	} else {
	    XtPopdown(XtParent(dialog));
	}
	break;
    }
    XtFree(name);
}

/* Dismiss popup window */
void
dismiss(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XtPopdown((Widget)client_data);
    if ((Widget)client_data == infopopup) info_up = False;
}

/* Destroy popup window */
void
destroy(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    XtDestroyWidget((Widget)client_data);
}

/* destroy callback for Ghostview widgets. */
/* The disable interpreter call ensures that ghostscript is killed. */
/* One the count goes to 0, we are sure that all forked processes have */
/* been killed and that we can safely exit. */
void
destroy_ghost(w, client_data, call_data)
    Widget w;
    XtPointer client_data, call_data;
{
    GhostviewDisableInterpreter((Widget) client_data);
    num_ghosts--;
    if (num_ghosts) return;
    if (dying) old_Xerror(XtDisplay(w), &bomb);
    XtDestroyApplicationContext(app_con);
    exit(0);
}

/* from actions.c */

/* Popup the copyright window */
static void gv_copyright(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    copyright_cb(w, NULL, NULL);
}

/* Call the quit callback to stop ghostview */
static void gv_quit(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    quit_ghostview(w, NULL, NULL);
}

/* Popup the open file dialog box. */
static void gv_open(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    popup_dialog(w, (XtPointer)OPEN, NULL);
}

/* Popup the open file dialog box. */
static void gv_reopen(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(reopenbutton)) return;
    reopen_file(w, NULL, NULL);
}

/* Popup the save file dialog box. */
static void gv_save(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(savebutton)) return;
    popup_dialog(w, (XtPointer)SAVE, NULL);
}

/* Popup the print file dialog box. */
static void gv_print_whole(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(printwholebutton)) return;
    popup_dialog(w, (XtPointer)PRINT_WHOLE, NULL);
}

/* Popup the print file dialog box. */
static void gv_print_marked(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(printmarkedbutton)) return;
    popup_dialog(w, (XtPointer)PRINT_MARKED, NULL);
}

/* Call the prev_page callback */
static void gv_prev(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(prevbutton)) return;
    prev_page(w, NULL, NULL);
}

/* Call the this_page callback */
static void gv_show(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(showbutton)) return;
    this_page(w, NULL, NULL);
}

/* Call the next_page callback */
static void gv_next(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(nextbutton)) return;
    next_page(w, NULL, NULL);
}

/* Call the center_page callback */
static void
gv_center(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(centerbutton)) return;
    center_page(w, NULL, NULL);
}

/* Call the mark_page callback */
static void
gv_mark(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(markbutton)) return;
    mark_page(w, NULL, NULL);
}

/* Call the unmark_page callback */
static void
gv_unmark(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (!XtIsSensitive(unmarkbutton)) return;
    unmark_page(w, NULL, NULL);
}

/* Get the magstep from the parameter string and
 * call the set_magstep callback with that magstep */
static void
gv_set_magstep(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int i;

    if (*num_params < 1) return;
    i = atoi(params[0]);
    set_magstep(w, (XtPointer)i, NULL);
}

/* Increment the magstep and
 * call the set_magstep callback with that magstep */
static void
gv_increase_magstep(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int i;

    i = app_res.magstep + 1;
    if (i <= app_res.maximum_magstep)
	set_magstep(w, (XtPointer)i, NULL);
}

/* Decrement the magstep and
 * call the set_magstep callback with that magstep */
static void
gv_decrease_magstep(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int i;

    i = app_res.magstep - 1;
    if (i >= app_res.minimum_magstep)
	set_magstep(w, (XtPointer)i, NULL);
}

/* Set orientation action routine.  Converts text parameter
 * to XtPageOrientation and all set_orientation callback */
static void
gv_set_orientation(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XrmValue from, to;
    XtPageOrientation orient;

    if (*num_params < 1) return;
    from.size = sizeof(String);
    from.addr = params[0];
    to.size = 0;
    to.addr = NULL;
    if (XmuCvtStringToPageOrientation(XtDisplay(w), NULL, ZERO,
				      &from, &to, NULL)) {
	orient = *(XtPageOrientation *)(to.addr);
	set_orientation(w, (XtPointer)orient, NULL);
    }
}

/* Call the swap_landscape callback */
static void
gv_swap_landscape(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    swap_landscape(w, NULL, NULL);
}

/* Set pagemedia action routine.  Converts text parameter
 * to index into the pagemedia widgets and calls the set_pagemedia
 * callback. */
static void
gv_set_pagemedia(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    int i;

    if (*num_params < 1) return;

    /* First check pagemedia defined within the document */
    if (doc && doc->nummedia) {
	for (i = 0; i < doc->nummedia; i++) {
	    if (!strcmp(params[0], doc->media[i].name)) {
		set_pagemedia(w, (XtPointer)i, NULL);
		break;
	    }
	}
    }

    /* Then check the standard ones */
    for (i = 0; papersizes[i].name; i++) {
	if (!strcmp(params[0], papersizes[i].name)) {
    	    set_pagemedia(w, (XtPointer)(base_papersize+i), NULL);
	    break;
	}
    }
}


/* Reset the force flag.  */
/* (force flag is checked when setting orientaion and pagemedia) */
static void
gv_default(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    force_setting = False;
}

/* Set the force flag.  */
/* (force flag is checked when setting orientaion and pagemedia) */
static void
gv_force(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    force_setting = True;
}

/* Implement WM_DELETE_WINDOW protocol */
static void
gv_delete_window(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    if (event->type == ClientMessage &&
	event->xclient.data.l[0] != wm_delete_window) return;
    XtDestroyWidget(w);
}


/* Destroy popup zoom window */
static void
gv_delete_zoom(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XtDestroyWidget(XtParent(w));
}

/* dismiss a popup window */
static void
gv_dismiss(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XtPopdown(w);
    if (w == infopopup) info_up = False;
}

/* scroll main viewport up */
static void
gv_scroll_up(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg args[2];
    Widget scroll;
    float top, shown;

    scroll = XtNameToWidget(pageview, "vertical");
    if (scroll) {
	XtSetArg(args[0], XtNshown, &shown);
	XtSetArg(args[1], XtNtopOfThumb, &top);
	XtGetValues(scroll, args, TWO);

	shown *= 0.3;

	top = top - shown;
	if (top < 0.0) top = 0.0;
	XtCallCallbacks(scroll, XtNjumpProc, &top);
    }
}

/* scroll main viewport down */
static void
gv_scroll_down(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg args[2];
    Widget scroll;
    float top, shown;

    scroll = XtNameToWidget(pageview, "vertical");
    if (scroll) {
	XtSetArg(args[0], XtNshown, &shown);
	XtSetArg(args[1], XtNtopOfThumb, &top);
	XtGetValues(scroll, args, TWO);

	shown *= 0.3;

	top = top + shown;
	if (top > (1.0 - shown)) top = (1.0 - shown);
	XtCallCallbacks(scroll, XtNjumpProc, &top);
    }
}

/* scroll main viewport left */
static void
gv_scroll_left(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg args[2];
    Widget scroll;
    float top, shown;

    scroll = XtNameToWidget(pageview, "horizontal");
    if (scroll) {
	XtSetArg(args[0], XtNshown, &shown);
	XtSetArg(args[1], XtNtopOfThumb, &top);
	XtGetValues(scroll, args, TWO);

	shown *= 0.3;

	top = top - shown;
	if (top < 0.0) top = 0.0;
	XtCallCallbacks(scroll, XtNjumpProc, &top);
    }
}

/* scroll main viewport right */
static void
gv_scroll_right(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg args[2];
    Widget scroll;
    float top, shown;

    scroll = XtNameToWidget(pageview, "horizontal");
    if (scroll) {
	XtSetArg(args[0], XtNshown, &shown);
	XtSetArg(args[1], XtNtopOfThumb, &top);
	XtGetValues(scroll, args, TWO);

	shown *= 0.3;

	top = top + shown;
	if (top > (1.0 - shown)) top = (1.0 - shown);
	XtCallCallbacks(scroll, XtNjumpProc, &top);
    }
}

/* Pop down locator window */
static void
gv_erase_locator(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Arg args[1];

    if (!app_res.show_locator) return;
    XtSetArg(args[0], XtNlabel, "");
    XtSetValues(locator, args, ONE);
}

/* Check to see if file was updated */
static void
gv_check_file(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    struct stat sbuf;

    if (psfile) {
	if (!stat(filename, &sbuf) && mtime != sbuf.st_mtime) {
	    show_page(current_page);
	}
    }
}

static void dummy_action(Widget w, XEvent *e, String *p, Cardinal *n)
{
	;
}

static XtActionsRec actions[] = {
    {"GhostviewCopyright",	gv_copyright},
    {"GhostviewQuit",		gv_quit},
    {"GhostviewOpen",		gv_open},
    {"GhostviewReopen",		gv_reopen},
    {"GhostviewSave",		gv_save},
    {"GhostviewPrintWhole",	gv_print_whole},
    {"GhostviewPrintMarked",	gv_print_marked},
    {"GhostviewPrevious",	gv_prev},
    {"GhostviewShow",		gv_show},
    {"GhostviewNext",		gv_next},
    {"GhostviewCenter",		gv_center},
    {"GhostviewMark",		gv_mark},
    {"GhostviewUnmark",		gv_unmark},
    {"GhostviewSetMagstep",	gv_set_magstep},
    {"GhostviewIncreaseMagstep",gv_increase_magstep},
    {"GhostviewDecreaseMagstep",gv_decrease_magstep},
    {"GhostviewSetOrientation",	gv_set_orientation},
    {"GhostviewSwapLandscape",	gv_swap_landscape},
    {"GhostviewSetPageMedia",	gv_set_pagemedia},
    {"GhostviewDefault",	gv_default},
    {"GhostviewForce",		gv_force},
    {"GhostviewDeleteWindow",	gv_delete_window},
    {"GhostviewDeleteZoom",	gv_delete_zoom},
    {"GhostviewDismiss",	gv_dismiss},
    {"GhostviewScrollUp",	gv_scroll_up},
    {"GhostviewScrollDown",	gv_scroll_down},
    {"GhostviewScrollLeft",	gv_scroll_left},
    {"GhostviewScrollRight",	gv_scroll_right},
    {"GhostviewEraseLocator",	gv_erase_locator},
    {"GhostviewCheckFile",	gv_check_file},
    {"menu-motion",		dummy_action},
};

void add_actions(XtAppContext ac)
{
   	XtAppAddActions(ac, actions, XtNumber(actions));
}
