/*
 * misc.c -- Everything that isn't a callback or action.
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

#include <stdio.h>
#include <stdlib.h>
#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#include <X11/Xos.h>
#include <signal.h>
#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

#include <math.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Form.h>

#include <Mowitz/Mowitz.h>

#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/AsciiText.h>
/* Yuck, cannot get vScrollbar via the usual methods */
#include <X11/IntrinsicP.h>
#include <X11/Xaw/TextP.h>
#include <X11/Xmu/StdCmap.h>

#include <errno.h>
/* BSD 4.3 errno.h does not declare errno */
extern int errno;
#ifdef VMS
#include <perror.h>
#endif

#include "gv.h"
#include "../common/ps.h"

#ifndef max
#define max(a, b)	((a) > (b) ? (a) : (b))
#endif

extern Widget menubox;

/* Translate orientations defined by the enum in "ps.h" to
 * XtPageOrientations defined in "Ghostview.h".
 */
static XtPageOrientation
xorient(psorient)
    int psorient;
{
    switch (psorient) {
    case PORTRAIT: return XtPageOrientationPortrait;
    case LANDSCAPE:
	if (app_res.swap_landscape) {
	    return XtPageOrientationSeascape;
	} else {
	    return XtPageOrientationLandscape;
	}
    }
    return XtPageOrientationPortrait;
}

static void
break_chains()
{
    Arg args[2];
    XtSetArg(args[0], XtNbottom, XtChainTop);
    XtSetArg(args[1], XtNright, XtChainLeft);
    XtSetValues(toc, args, ONE);
    XtSetValues(pageview, args, TWO);
}

static void
reset_size_hints()
{
    Arg args[4];
    if (app_res.ncdwm) return;
    XtSetArg(args[0], XtNmaxWidth, XtUnspecifiedShellInt);
    XtSetArg(args[1], XtNmaxHeight, XtUnspecifiedShellInt);
    XtSetArg(args[2], XtNminWidth, XtUnspecifiedShellInt);
    XtSetArg(args[3], XtNminHeight, XtUnspecifiedShellInt);
    XtSetValues(toplevel, args, FOUR);
}

static Boolean horiz_scroll_saved = False;
static Boolean vert_scroll_saved = False;
static float horiz_top;
static float vert_top;

static void
reset_scroll_bars()
{
    Arg args[1];
    Widget scroll;
    float zero = 0.0;
    
    if (horiz_scroll_saved || vert_scroll_saved) return;

    scroll = XtNameToWidget(pageview, "horizontal");
    if (scroll) {
	XtSetArg(args[0], XtNtopOfThumb, &horiz_top);
	XtGetValues(scroll, args, ONE);
	XtCallCallbacks(scroll, XtNjumpProc, &zero);
	horiz_scroll_saved = True;
    }

    scroll = XtNameToWidget(pageview, "vertical");
    if (scroll) {
	XtSetArg(args[0], XtNtopOfThumb, &vert_top);
	XtGetValues(scroll, args, ONE);
	XtCallCallbacks(scroll, XtNjumpProc, &zero);
	vert_scroll_saved = True;
    }
}

/* Start rendering a new page */
void
show_page(number)
    int number;
{
    struct stat sbuf;
    int i;

    if (!filename) return;

    /* Unmark current_page as current */
    if (toc_text && (current_page >= 0)) {
	int marker = current_page*toc_entry_length + toc_entry_length-2;
	toc_text[marker] = ' ';
	XawTextInvalidate(toc, marker, marker+1);
    }

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
    if (toc_text) {
	if (number >= doc->numpages) number = doc->numpages - 1;
	if (number < 0) number = 0;
    }

    if (set_new_orientation(number) || set_new_pagemedia(number))
	layout_ghostview();

    if (toc_text) {
	int marker;
	current_page = number;
	XawTextUnsetSelection(toc);
	XawTextSetInsertionPoint(toc, current_page * toc_entry_length);
	marker = current_page*toc_entry_length + toc_entry_length-2;
	toc_text[marker] = '<';
	XawTextInvalidate(toc, marker, marker+1);
	if (GhostviewIsInterpreterReady(page)) {
	    GhostviewNextPage(page);
	} else {
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

    if (toc_text) {
	XtSetSensitive(prevbutton, current_page != 0);
	XtSetSensitive(nextbutton, current_page != doc->numpages-1);
	XtSetSensitive(showbutton, True);
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
Boolean
setup_ghostview()
{
    Arg args[20];
    Cardinal num_args;
    int oldtoc_entry_length;
    char *tocp;
    XawTextBlock message_block;
    static String nothing = "";
    String label;

    /* Reset to a known state. */
    psfree(olddoc);
    olddoc = doc;
    doc = NULL;
    current_page = -1;
    if (toc_text) XtFree(toc_text);
    oldtoc_entry_length = toc_entry_length;
    toc_text = NULL;

    /* Scan document and start setting things up */
    if (psfile) doc = psscan(psfile);

    if (app_res.show_title) {
	if (doc && doc->title) label = doc->title;
	else {
	    if (filename) label = filename;
	    else label = "";
	}
	XtVaSetValues(toplevel, XtNtitle, label, (char *)0);
    }

    if (app_res.show_date) {
	if (doc && doc->date) label = doc->date;
	else {
	    if (psfile) label = ctime(&mtime);
	    else label = "";
	}
	XtVaSetValues(label2, XtNlabel, label, (char *)0);
    }

    build_pagemedia_menu();
    build_antialiasing_menu();

    /* Reset ghostscript and output messages popup */
    if (!doc || !olddoc ||
	strcmp(oldfilename, filename) ||
	olddoc->beginprolog != doc->beginprolog ||
	olddoc->endprolog != doc->endprolog ||
	olddoc->beginsetup != doc->beginsetup ||
	olddoc->endsetup != doc->endsetup) {

	GhostviewDisableInterpreter(page);
	XtPopdown(infopopup);
	info_up = False;
	XtSetArg(args[0], XtNeditType, XawtextEdit);
	XtSetArg(args[1], XtNinsertPosition, 0);
	XtSetValues(infotext, args, TWO);
	message_block.length = 0;
	XawTextReplace(infotext, 0, info_length, &message_block);
	info_length = 0;
	XtSetArg(args[0], XtNeditType, XawtextRead);
	XtSetValues(infotext, args, ONE);
    }

    /* Build table of contents */
    if (doc && ((!doc->epsf && doc->numpages > 0) ||
		 (doc->epsf && doc->numpages > 1))) {
	int maxlen = 0;
	int i, j;
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
	toc_entry_length = maxlen + 3;
	toc_length = doc->numpages * toc_entry_length - 1;
	toc_text = XtMalloc(toc_length + 2); /* include final NULL */

	for (i = 0, tocp = toc_text; i < doc->numpages;
	     i++, tocp += toc_entry_length) {
	    if (useful_page_labels) {
		if (doc->pageorder == DESCEND) {
		    j = (doc->numpages - 1) - i;
		} else {
		    j = i;
		}
		sprintf(tocp, " %*s \n", maxlen, doc->pages[j].label);
	    } else {
		sprintf(tocp, " %*d \n", maxlen, i+1);
	    }
	}
	toc_text[toc_length] = '\0';
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
    if (toc_text) {
	XtSetArg(args[num_args], XtNstring, toc_text);		num_args++;
    } else {
	/* Text widget sometime blows up when given a NULL pointer */
	XtSetArg(args[num_args], XtNstring, nothing);		num_args++;
    }
    XtSetValues(toc, args, num_args);

    XtSetSensitive(reopenbutton, (psfile != NULL));
    XtSetSensitive(printwholebutton, (psfile != NULL));
    XtSetSensitive(printmarkedbutton, (psfile != NULL));
    XtSetSensitive(savebutton, (toc_text != NULL));
    XtSetSensitive(nextbutton, (filename != NULL));
    XtSetSensitive(showbutton, (filename != NULL));
    XtSetSensitive(prevbutton, (toc_text != NULL));
    XtSetSensitive(centerbutton, (filename != NULL));
    XtSetSensitive(markbutton, (toc_text != NULL));
    XtSetSensitive(unmarkbutton, (toc_text != NULL));

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

static void
try_try_again(client_data, timer)
    XtPointer client_data;
    XtIntervalId *timer;
{
    XSync(XtDisplay(toplevel), False);	/* Push everything out */
    if (XtAppPending(app_con)) {
	XtAppAddTimeOut(app_con, delay, try_try_again, NULL);
	/* fprintf(stderr, "Delaying(%d)...\n",delay); */
	delay *= 2;
    } else {
	/* fprintf(stderr, "Trying again...\n"); */
	layout_ghostview();
    }
}

/* set the dimensions for items in the main form widget. */
/* set foreground and background color in scrollbars. */
/* (The scroll bars come and go as size changes.) */
/* Set window manager hints to keep window manager from causing main */
/* viewport from growing too large */
void
layout_ghostview()
{
	;	/* do nothing */
}

/* Compute new dpi from magstep */
void
magnify(dpi, magstep)
    float *dpi;
    int    magstep;
{
    if (magstep < 0) {
	while (magstep++) *dpi /= 1.2;
    } else {
	while (magstep--) *dpi *= 1.2;
    }
}

/* Attempt to open file, return error message string on failure */
String
open_file(name)
    String name;
{
    FILE *fp;
    struct stat sbuf;

    if (*name == '\0') {	/* Null filename */
	return(NULL);
    }
    if (strcmp(name, "-")) {
	if ((fp = fopen(name, "r")) == NULL) {
	    String buf = XtMalloc(strlen(app_res.open_fail) +
				  /*strlen(sys_errlist[errno]) +*/ 1);
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
String
save_file(name)
    String name;
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
	/*if (errno <= sys_nerr)*/ strcat(buf, se/*sys_errlist[errno]*/);
	return buf;
    } else {
	pscopydoc(pswrite);
	fclose(pswrite);
	return(NULL);
    }
}

/* Attempt to print file.  Return error string on failure */ 
String
print_file(name, whole_mode)
    String name;
    Boolean whole_mode;
{
    FILE *printer;
    SIGVAL (*oldsig)();
    int bytes;
    char buf[BUFSIZ];
#ifdef VMS
    char fnam[64], *p;
#endif
    Boolean failed;
    String ret_val;

#ifdef VMS
    sprintf(fnam, "sys$scratch:%s.tmp", tmpnam(NULL));
    printer = fopen(fnam, "w");
#else /* VMS */
    if (*name != '\0') {
	char b[1024];
	sprintf(b, "%s=%s", app_res.printer_variable, name);
	putenv(b);
    }
    oldsig = signal(SIGPIPE, SIG_IGN);
    printer = popen(app_res.print_command, "w");
#endif /* VMS */
    if (toc_text && !whole_mode) {
	pscopydoc(printer);
    } else {
	FILE *psfile = fopen(filename, "r");
	while ((bytes = read(fileno(psfile), buf, BUFSIZ)))
	    bytes = write(fileno(printer), buf, bytes);
	fclose(psfile);
    }
#ifdef VMS
    sprintf(buf, "%s %s %s", app_res.print_command, name, fnam);
    failed = fclose(printer) != 0 || system(buf) != 1;
#else /* VMS */
    failed = pclose(printer) != 0;
#endif /* VMS */
    if (failed) {
	sprintf(buf, app_res.print_fail, app_res.print_command);
	ret_val = XtNewString(buf);
    } else {
	ret_val = NULL;
    }
#ifndef VMS
    signal(SIGPIPE, oldsig);
#endif /* VMS */
    return(ret_val);
}

/* length calculates string length at compile time */
/* can only be used with character constants */
#define length(a) (sizeof(a)-1)

/* Copy the headers, marked pages, and trailer to fp */
void
pscopydoc(fp)
    FILE *fp;
{
    FILE *psfile;
    char text[PSLINELENGTH];
    char *comment;
    Boolean pages_written = False;
    Boolean pages_atend = False;
    Boolean marked_pages = False;
    int pages = 0;
    int page = 1;
    int i, j;
    long here;

    psfile = fopen(filename, "r");

    for (i = 0; i < doc->numpages; i++) {
	if (toc_text[toc_entry_length * i] == '*') pages++;
    }

    if (pages == 0) {	/* User forgot to mark the pages */

	mark_page(topbox, NULL, NULL);	/* FIXME */

	marked_pages = True;
	for (i = 0; i < doc->numpages; i++) {
	    if (toc_text[toc_entry_length * i] == '*') pages++;
	}
    }

    here = doc->beginheader;
    while ((comment = pscopyuntil(psfile, fp, here,
				 doc->endheader, "%%Pages:"))) {
	here = ftell(psfile);
	if (pages_written || pages_atend) {
	    free(comment);
	    continue;
	}
	sscanf(comment+length("%%Pages:"), "%s", text);
	if (strcmp(text, "(atend)") == 0) {
	    fputs(comment, fp);
	    pages_atend = True;
	} else {
	    switch (sscanf(comment+length("%%Pages:"), "%*d %d", &i)) {
		case 1:
		    fprintf(fp, "%%%%Pages: %d %d\n", pages, i);
		    break;
		default:
		    fprintf(fp, "%%%%Pages: %d\n", pages);
		    break;
	    }
	    pages_written = True;
	}
	free(comment);
    }
    pscopy(psfile, fp, doc->beginpreview, doc->endpreview);
    pscopy(psfile, fp, doc->begindefaults, doc->enddefaults);
    pscopy(psfile, fp, doc->beginprolog, doc->endprolog);
    pscopy(psfile, fp, doc->beginsetup, doc->endsetup);

    for (i = 0; i < doc->numpages; i++) {
	if (doc->pageorder == DESCEND) 
	    j = (doc->numpages - 1) - i;
	else
	    j = i;
	if (toc_text[toc_entry_length * j] == '*') {
	    comment = pscopyuntil(psfile, fp, doc->pages[i].begin,
				  doc->pages[i].end, "%%Page:");
	    fprintf(fp, "%%%%Page: %s %d\n",
		    doc->pages[i].label, page++);
	    free(comment);
	    pscopy(psfile, fp, -1, doc->pages[i].end);
	}
    }

    here = doc->begintrailer;
    while ((comment = pscopyuntil(psfile, fp, here,
				 doc->endtrailer, "%%Pages:"))) {
	here = ftell(psfile);
	if (pages_written) {
	    free(comment);
	    continue;
	}
	switch (sscanf(comment+length("%%Pages:"), "%*d %d", &i)) {
	    case 1:
		fprintf(fp, "%%%%Pages: %d %d\n", pages, i);
		break;
	    default:
		fprintf(fp, "%%%%Pages: %d\n", pages);
		break;
	}
	pages_written = True;
	free(comment);
    }
    fclose(psfile);

	if (marked_pages) unmark_page(topbox, NULL, NULL);
}
#undef length

/* position popup window under the cursor */
void
positionpopup(w)
    Widget w;
{
    Arg args[3];
    Cardinal num_args;
    Dimension width, height, b_width;
    int x, y, max_x, max_y;
    Window root, child;
    int dummyx, dummyy;
    unsigned int dummymask;
    
    XQueryPointer(XtDisplay(w), XtWindow(w), &root, &child, &x, &y,
		  &dummyx, &dummyy, &dummymask);
    num_args = 0;
    XtSetArg(args[num_args], XtNwidth, &width); num_args++;
    XtSetArg(args[num_args], XtNheight, &height); num_args++;
    XtSetArg(args[num_args], XtNborderWidth, &b_width); num_args++;
    XtGetValues(w, args, num_args);

    width += 2 * b_width;
    height += 2 * b_width;

    x -= ( (Position) width/2 );
    if (x < 0) x = 0;
    if ( x > (max_x = (Position) (XtScreen(w)->width - width)) ) x = max_x;

    y -= ( (Position) height/2 );
    if (y < 0) y = 0;
    if ( y > (max_y = (Position) (XtScreen(w)->height - height)) ) y = max_y;
    
    num_args = 0;
    XtSetArg(args[num_args], XtNx, x); num_args++;
    XtSetArg(args[num_args], XtNy, y); num_args++;
    XtSetValues(w, args, num_args);
}

/* Set new magstep */
Boolean
set_new_magstep()
{
    int new_magstep;
    Boolean changed = False;
    Arg args[20];
    Cardinal num_args;
    float xdpi, ydpi;

    new_magstep = app_res.magstep;
    /* If magstep changed, stop interpreter and setup for new dpi. */
    if (new_magstep != current_magstep) {
	GhostviewDisableInterpreter(page);
	reset_size_hints();
	reset_scroll_bars();
	break_chains();
	changed = True;
	xdpi = default_xdpi;
	ydpi = default_ydpi;
	magnify(&xdpi, new_magstep);
	magnify(&ydpi, new_magstep);
							num_args = 0;
	XtSetFloatArg(args[num_args], XtNxdpi, xdpi);	num_args++;
	XtSetFloatArg(args[num_args], XtNydpi, ydpi);	num_args++;
	XtSetValues(page, args, num_args);

	XtSetArg(args[0], XtNleft_icon, blank_bitmap);
	XtSetValues(magstepentry[current_magstep - app_res.minimum_magstep], args, ONE);
	current_magstep = new_magstep;
    }
    XtSetArg(args[0], XtNleft_icon, dot_bitmap);
    XtSetValues(magstepentry[current_magstep - app_res.minimum_magstep], args, ONE);

    return changed;
}

/* Set new orientation */
Boolean
set_new_orientation(number)
    int number;
{
    Boolean changed = False;
    Boolean from_doc = False;
    Arg args[2];
    XtPageOrientation new_orientation;

    if (app_res.force_orientation) {
	new_orientation = app_res.orientation;
    } else {
	if (doc) {
	    if (toc_text && doc->pages[number].orientation != NONE) {
		new_orientation = xorient(doc->pages[number].orientation);
		from_doc = True;
	    } else if (doc->default_page_orientation != NONE) {
		new_orientation = xorient(doc->default_page_orientation);
		from_doc = True;
	    } else if (doc->orientation != NONE) {
		new_orientation = xorient(doc->orientation);
		from_doc = True;
	    } else {
		new_orientation = app_res.orientation;
	    }
	} else {
	    new_orientation = app_res.orientation;
	}
    }

    /* If orientation changed,
     * stop interpreter and setup for new orientation. */
    if (new_orientation != current_orientation) {
	GhostviewDisableInterpreter(page);
	reset_size_hints();
	reset_scroll_bars();
	break_chains();
	changed = True;
	XtSetArg(args[0], XtNorientation, new_orientation);
	XtSetValues(page, args, ONE);
	XtSetArg(args[0], XtNleft_icon, blank_bitmap);
	if (current_orientation == XtPageOrientationPortrait) 
	    XtSetValues(portraitbutton, args, ONE);
	else if (current_orientation == XtPageOrientationLandscape)
            XtSetValues(landscapebutton, args, ONE);
        else if (current_orientation == XtPageOrientationUpsideDown)
            XtSetValues(upsidedownbutton, args, ONE);
        else if (current_orientation == XtPageOrientationSeascape)
            XtSetValues(seascapebutton, args, ONE);
	current_orientation = new_orientation;
    }

    /* mark forced orientation with tie fighter. ("Use the force, Luke") */
    if (app_res.force_orientation) {
	XtSetArg(args[0], XtNleft_icon, tie_fighter_bitmap);
    } else {
	XtSetArg(args[0], XtNleft_icon, dot_bitmap);
    }
    if (current_orientation == XtPageOrientationPortrait) 
	XtSetValues(portraitbutton, args, ONE);
    else if (current_orientation == XtPageOrientationLandscape)
	XtSetValues(landscapebutton, args, ONE);
    else if (current_orientation == XtPageOrientationUpsideDown)
	XtSetValues(upsidedownbutton, args, ONE);
    else if (current_orientation == XtPageOrientationSeascape)
	XtSetValues(seascapebutton, args, ONE);
    
    return changed;
}

/* Set new pagemedia */
Boolean
set_new_pagemedia(number)
    int number;
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
	    if (toc_text && doc->pages[number].media != NULL) {
		new_pagemedia = doc->pages[number].media - doc->media;
		from_doc = True;
	    } else if (doc->default_page_media != NULL) {
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
	XtSetArg(args[0], XtNleft_icon, blank_bitmap);
	if (pagemediaentry[current_pagemedia])
	    XtSetValues(pagemediaentry[current_pagemedia], args, ONE);
	else
	    XtSetValues(pagemediaentry[current_pagemedia-1], args, ONE);

	current_pagemedia = new_pagemedia;
    }

    /* mark forced page media with tie fighter. ("Use the force, Luke") */
    if (force_document_media || app_res.force_pagemedia) {
	XtSetArg(args[0], XtNleft_icon, tie_fighter_bitmap);
    } else {
	XtSetArg(args[0], XtNleft_icon, dot_bitmap);
    }
    if (pagemediaentry[current_pagemedia])
	XtSetValues(pagemediaentry[current_pagemedia], args, ONE);
    else
	XtSetValues(pagemediaentry[current_pagemedia-1], args, ONE);

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
	reset_scroll_bars();
	break_chains();
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

static Boolean
same_document_media()
{
    int i;

    if (olddoc == NULL && doc == NULL) return True;
    if (olddoc == NULL || doc == NULL) return False;
    if (olddoc->nummedia != doc->nummedia) return False;
    for (i = 0; i < doc->nummedia; i++)
	if (strcmp(olddoc->media[i].name, doc->media[i].name)) return False;
    return True;
}

void
build_pagemedia_menu()
{
    Widget w;
    int i;

    if (pagemediamenu && same_document_media()) return;
    if (pagemediamenu) XtDestroyWidget(pagemediamenu);
    force_document_media = False;

    pagemediamenu = XtCreatePopupShell("pagemedia_menu", mwMenuWidgetClass,
				       menubox, NULL, ZERO);

    /* Build the Page Media menu */
    /* the Page media menu has two parts.
     *  - the document defined page medias
     *  - the standard page media defined from Adobe's PPD
     */
    base_papersize = 0;
    if (doc) base_papersize = doc->nummedia;
    for (i = 0; papersizes[i].name; i++) {}	/* Count the standard entries */
    i += base_papersize;
    pagemediaentry = (Widget *) XtMalloc(i * sizeof(Widget));

    if (doc && doc->nummedia) {
	for (i = 0; i < doc->nummedia; i++) {
	    pagemediaentry[i] = XtVaCreateManagedWidget(doc->media[i].name,
				mwLabelMEObjectClass, pagemediamenu,
				XtNlabel, doc->media[i].name,
				XtNleft_icon, blank_bitmap,
				(char *)0);
	    XtAddCallback(pagemediaentry[i], XtNcallback,
			  set_pagemedia, (XtPointer)i);
	}

	w = XtCreateManagedWidget("line", mwLineMEObjectClass, pagemediamenu,
				  NULL, 0);
    }

    for (i = 0; papersizes[i].name; i++) {
	pagemediaentry[i+base_papersize] = NULL;
	if (i > 0) {
	    /* Skip over same paper size with small imageable area */
	    if ((papersizes[i].width == papersizes[i-1].width) &&
		(papersizes[i].height == papersizes[i-1].height)) {
		continue;
	    }
	}
	pagemediaentry[i+base_papersize] = XtVaCreateManagedWidget(
					papersizes[i].name,
					mwLabelMEObjectClass, pagemediamenu,
					XtNlabel, papersizes[i].name,
					XtNleft_icon, blank_bitmap,
					(char *)0);
	XtAddCallback(pagemediaentry[i+base_papersize], XtNcallback,
		      set_pagemedia, (XtPointer)(i+base_papersize));
    }
}

void
build_antialiasing_menu()
{
    int i;
    char *onoff[2] = { "On", "Off" };

    if (antialiasingmenu) XtDestroyWidget(antialiasingmenu);
    force_document_media = False;

    antialiasingmenu = XtCreatePopupShell("antialiasing_menu", mwMenuWidgetClass,
				       menubox, NULL, ZERO);

    /* Build the Antialiasing menu */

    antialiasingentry = (Widget *) XtMalloc(2 * sizeof(Widget));

    for (i = 0; i <= 1; i++) {
	    antialiasingentry[i] = XtVaCreateManagedWidget(onoff[i],
				mwLabelMEObjectClass, antialiasingmenu,
				XtNlabel, onoff[i],
				XtNleft_icon, 
                                   (i==app_res.antialiasing)?
				     blank_bitmap : dot_bitmap,
				(char *)0);
	    XtAddCallback(antialiasingentry[i], XtNcallback,
			  set_antialiasing, (XtPointer)i);
	}
}

void new_file(int number)
{
    Boolean layout_changed = False;

    if (setup_ghostview()) layout_changed = True;

    /* Coerce page number to fall in range */
    if (toc_text) {
	if (number >= doc->numpages) number = doc->numpages - 1;
	if (number < 0) number = 0;
    }

    if (set_new_orientation(number)) layout_changed = True;
    if (set_new_pagemedia(number)) layout_changed = True;
    if (layout_changed) layout_ghostview();
}

/* Catch X errors die gracefully if one occurs */
int
catch_Xerror(dpy, err)
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
