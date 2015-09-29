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

/*******************************************************************
 * Module "special.c" containing stuff for commands in the Special *
 * Menu								   *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *******************************************************************/

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "xedit.h"
#include <limits.h>
#include "../common/common.h"
#include "../xcommon/xcommon.h"

/* Used global widgets */
extern Widget textwindow, option_popup;
extern Widget wrap_group;
extern Widget indent_group, autofill_group;
extern Widget tabsize_text;
extern Widget sed_popup, sed_text;
extern Widget help_popup, help_text;
extern Widget about_popup;
extern char **wrapmode;
extern char **indentmode, **autofillmode;
extern XtTranslations inserttrans, overwritetrans;
extern Widget modelabel;

/*******************************************************************
 * Calllback for "Options" menu entry				   *
 *******************************************************************/
void DoSpezialOptions(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	XawTextWrapMode mode;
	caddr_t data = NULL;
	Boolean fillmode;

/* Get Wrapmode */
	XtSetArg(args[0], XtNwrap, &mode);
	XtGetValues(textwindow, args, 1);
	switch (mode) {
	case XawtextWrapNever:{
			data = wrapmode[0];
			break;
		}
	case XawtextWrapLine:{
			data = wrapmode[1];
			break;
		}
	case XawtextWrapWord:{
			data = wrapmode[2];
			break;
		}
	default:
		break;
	}

/* Autoindent mode */
	XawToggleSetCurrent(wrap_group, data);
	if (app_resources.autoIndent)
		XawToggleSetCurrent(indent_group, indentmode[0]);
	else
		XawToggleSetCurrent(indent_group, indentmode[1]);

/* Autofill mode */
	XtSetArg(args[0], XtNautoFill, &fillmode);
	XtGetValues(textwindow, args, 1);
	if (fillmode)
		XawToggleSetCurrent(autofill_group, autofillmode[0]);
	else
		XawToggleSetCurrent(autofill_group, autofillmode[1]);

	centerpopup(textwindow, option_popup);
	XtPopup(option_popup, XtGrabExclusive);
	set_wm_delete(option_popup);
}


/*******************************************************************
 * Set tabsize for a textwidget					   *
 *******************************************************************/
void set_tabsize(widget, tabsize)
Widget widget;
int tabsize;
{
	Widget sink;
	int tabs[100], i;
	Arg args[2];

	XtSetArg(args[0], XtNtextSink, &sink);
	XtGetValues(widget, args, 1);
	for (i = 0; i < 100; i++)
		tabs[i] = tabsize * i;
	XawTextSinkSetTabs(sink, 100, tabs);
}


/*******************************************************************
 * Callback for option popup OK and CANCEL button		   *
 *******************************************************************/
void option_ready(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	static int tabsize = 8;
	Arg args[1];
	char *inputstring, outputstring[256];
	XawTextPosition end;
	XtTranslations trans;

	XtPopdown(option_popup);
	if ((int) closure == CANCEL) {
		sprintf(outputstring, "%d", tabsize);
		XtSetArg(args[0], XtNstring, outputstring);
		XtSetValues(tabsize_text, args, 1);
		XawTextSetInsertionPoint(tabsize_text, 255);
		return;
	}
/* Set Wrapmode */
	if (XawToggleGetCurrent(wrap_group) == wrapmode[0])
		XtSetArg(args[0], XtNwrap, XawtextWrapNever);
	if (XawToggleGetCurrent(wrap_group) == wrapmode[1])
		XtSetArg(args[0], XtNwrap, XawtextWrapLine);
	if (XawToggleGetCurrent(wrap_group) == wrapmode[2])
		XtSetArg(args[0], XtNwrap, XawtextWrapWord);
	XtSetValues(textwindow, args, 1);

/* Set Tabsize */
	XtSetArg(args[0], XtNstring, &inputstring);
	XtGetValues(tabsize_text, args, 1);
	tabsize = atoi(inputstring);
	if (tabsize < 1)
		tabsize = 1;
	sprintf(outputstring, "%d", tabsize);
	XtSetArg(args[0], XtNstring, outputstring);
	XtSetValues(tabsize_text, args, 1);
	XawTextSetInsertionPoint(tabsize_text, 255);
	set_tabsize(textwindow, tabsize);

/* Set Indent Mode */
	if (XawToggleGetCurrent(indent_group) == indentmode[0]) {
		app_resources.autoIndent = True;
		trans = XtParseTranslationTable(":<Key>Return:autoindent()");
		XtOverrideTranslations(textwindow, trans);
	} else {
		app_resources.autoIndent = False;
		trans = XtParseTranslationTable(":<Key>Return:newline()");
		XtOverrideTranslations(textwindow, trans);
	}

/* Set Autofill Mode */
	if (XawToggleGetCurrent(autofill_group) == autofillmode[0])
		XtSetArg(args[0], XtNautoFill, True);
	else
		XtSetArg(args[0], XtNautoFill, False);
	XtSetValues(textwindow, args, 1);

/* Redisplay all */
	end = XawTextSourceScan(XawTextGetSource(textwindow), INT_MAX, XawstEOL, XawsdRight, 1, True);
	XawTextInvalidate(textwindow, 0, end);
	XawTextEnableRedisplay(textwindow);
}


/*******************************************************************
 * Callback procedure for toggle between insert and overwrite mode *
 *******************************************************************/
void xedtoggleoverwrite(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	static int overwritemode = 0;
	Arg args[1];
	XtTranslations trans;
	Pixmap pm;
	Pixel color;

	XtVaGetValues(widget, XtNbackground, &color, (char *)0);
	if (overwritemode) {
		pm = load_pixmap(XtDisplay(modelabel), color, "insert.xpm");
		XtSetArg(args[0], XtNtranslations, inserttrans);
		overwritemode = 0;
	} else {
		pm = load_pixmap(XtDisplay(modelabel), color, "overwrite.xpm");
		XtSetArg(args[0], XtNtranslations, overwritetrans);
		overwritemode = 1;
	}
	XtVaSetValues(modelabel, XtNbitmap, pm, (char *)0);
	XtSetValues(textwindow, args, 1);
	if (app_resources.autoIndent && !overwritemode) {
		trans = XtParseTranslationTable(":<Key>Return:autoindent()");
		XtOverrideTranslations(textwindow, trans);
	}
}


/*******************************************************************
 * Action procedure for skipping newline in textwindow             *
 * (used for not overwriting newlines in overwrite mode)	   *
 *******************************************************************/
void xedskiplineend(widget, event, params, n_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *n_params;
{
	XawTextPosition pos;
	XawTextBlock block;

	pos = XawTextGetInsertionPoint(widget);
	XawTextSourceRead(XawTextGetSource(widget), pos, &block, 1);
	if (block.length > 0)
		if (block.ptr[0] == '\n')
			XawTextSetInsertionPoint(widget, pos + 1);
}


/*******************************************************************
 * Action procedure for autoindent in a textwidget                 *
 *******************************************************************/
void autoindent(widget, event, params, n_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *n_params;
{
	XawTextPosition end, begin, begin2, nbytes;
	char *pos, line_buf[1024];
	XawTextBlock buf;

	buf.firstPos = 0;
	buf.format = FMT8BIT;
	*line_buf = '\0';

/* Get current Position and Line Begin */
	end = XawTextGetInsertionPoint(widget);
	begin = XawTextSourceScan(XawTextGetSource(widget), end, XawstEOL, XawsdLeft, 1, True);
	if (begin == XawTextSearchError)
		begin = 0;
	if (begin > 0)
		begin++;

/* Insert Newline */
	buf.ptr = "\n";
	buf.length = 1;
	XawTextReplace(widget, end, end, &buf);

	begin2 = begin;
	while (begin2 < end) {
		nbytes = end - begin;
		XawTextSourceRead(XawTextGetSource(widget), begin2, &buf, nbytes);
		begin2 += buf.length;
		strncat(line_buf, buf.ptr, buf.length);
	}
	buf.length = 0;
	pos = line_buf;
	while (((*pos == ' ') || (*pos == '\t')) && (*pos != '\0')) {
		buf.length++;
		pos++;
	}
	buf.ptr = line_buf;
	buf.firstPos = 0;
	XawTextReplace(widget, end + 1, end + 1, &buf);
	XawTextSetInsertionPoint(widget, end + buf.length + 1);
}


/*******************************************************************
 * Callback for "Call sed" menu entry				   *
 *******************************************************************/
void DoSpezialCallSed(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	centerpopup(textwindow, sed_popup);
	XtPopup(sed_popup, XtGrabNone);
	set_wm_delete(sed_popup);
	XRaiseWindow(XtDisplay(sed_popup), XtWindow(sed_popup));
}

static char tmpin_file[L_xedtmpnam + 3], tmpout_file[L_xedtmpnam + 3],
 tmpsed_file[L_xedtmpnam + 3], tmpsel_file[L_xedtmpnam + 3];
static Boolean undo_possible = False;


/*******************************************************************
 * Internal function for replacing current text with contens of    *
 * a file							   *
 *******************************************************************/
static void replace_file(filename)
char *filename;
{
	XawTextBlock block;
	XawTextPosition position, textend;
	char buffer[1024];
	int file_d;

	block.firstPos = 0;
	block.ptr = buffer;
	block.length = 0;
	block.format = FMT8BIT;
	position = 0;

	file_d = open(filename, O_RDONLY);
	if (file_d == -1)
		return;
	textend = XawTextSourceScan(XawTextGetSource(textwindow), INT_MAX, XawstEOL,
				    XawsdRight, 1, True);
	XawTextReplace(textwindow, 0, textend, &block);
	while ((block.length = read(file_d, block.ptr, 1024)) > 0) {
		XawTextReplace(textwindow, position, position, &block);
		position += 1024;
	}
	close(file_d);
}


/*******************************************************************
 * Callback for "Do it" button in sed popup			   *
 *******************************************************************/
void sed_do(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *commandstring, command[1024];
	Arg args[2];
	FILE *fp;

	if (undo_possible) {
		unlink(tmpin_file);
	} else
		undo_possible = True;
	xed_tmpnam(tmpin_file);
	strcat(tmpin_file, "in");
	XawAsciiSaveAsFile(XawTextGetSource(textwindow), tmpin_file);
	xed_tmpnam(tmpout_file);
	strcat(tmpout_file, "out");
	xed_tmpnam(tmpsed_file);
	strcat(tmpsed_file, "sed");
	XtSetArg(args[0], XtNstring, &commandstring);
	XtGetValues(sed_text, args, 1);
	fp = fopen(tmpsed_file, "w");
	fprintf(fp, "%s\n", commandstring);
	fclose(fp);
	sprintf(command, "cat %s | sed -f %s 1> %s 2> %s", tmpin_file, tmpsed_file, tmpout_file, tmpout_file);
	system(command);
	replace_file(tmpout_file);
	unlink(tmpout_file);
	unlink(tmpsed_file);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
}


/*******************************************************************
 * Callback for "Do it Selection" button in sed popup		   *
 *******************************************************************/
void sed_do_sel(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *commandstring, command[1024];
	Arg args[2];
	FILE *fp;
	XawTextBlock buf;
	XawTextPosition begin, beginmerk, end, length;
	int file_d, nbytes;
	XawTextBlock insertblock;
	XawTextPosition position;
	char buffer[1024];

	XawTextGetSelectionPos(textwindow, &begin, &end);
	beginmerk = begin;
	length = end - begin;
	if (length <= 0) {
		Feep();
		return;
	}
	if (undo_possible) {
		unlink(tmpin_file);
	} else
		undo_possible = True;
	xed_tmpnam(tmpin_file);
	strcat(tmpin_file, "in");
	XawAsciiSaveAsFile(XawTextGetSource(textwindow), tmpin_file);
	xed_tmpnam(tmpsel_file);
	strcat(tmpsel_file, "sel");
	file_d = open(tmpsel_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
	while (begin < end) {
		if (length > 1024)
			nbytes = 1024;
		else
			nbytes = length;
		XawTextSourceRead(XawTextGetSource(textwindow), begin, &buf, nbytes);
		write(file_d, buf.ptr, buf.length);
		begin += buf.length;
		length -= buf.length;
	}
	close(file_d);
	xed_tmpnam(tmpout_file);
	strcat(tmpout_file, "out");
	xed_tmpnam(tmpsed_file);
	strcat(tmpsed_file, "sed");
	XtSetArg(args[0], XtNstring, &commandstring);
	XtGetValues(sed_text, args, 1);
	fp = fopen(tmpsed_file, "w");
	fprintf(fp, "%s\n", commandstring);
	fclose(fp);
	sprintf(command, "cat %s | sed -f %s 1> %s 2> %s", tmpsel_file, tmpsed_file, tmpout_file, tmpout_file);
	system(command);
	file_d = open(tmpout_file, O_RDONLY);
	insertblock.firstPos = 0;
	insertblock.length = 0;
	insertblock.ptr = buffer;
	insertblock.format = FMT8BIT;
	XawTextReplace(textwindow, beginmerk, end, &insertblock);
	position = beginmerk;
	while ((insertblock.length = read(file_d, insertblock.ptr, 1024)) > 0) {
		XawTextReplace(textwindow, position, position, &insertblock);
		position += 1024;
	}
	close(file_d);
	unlink(tmpout_file);
	unlink(tmpsel_file);
	unlink(tmpsed_file);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
}


/*******************************************************************
 * Callback for "Undo it" button in sed popup			   *
 *******************************************************************/
void sed_undo(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{

	if (undo_possible) {
		replace_file(tmpin_file);
		unlink(tmpin_file);
		undo_possible = False;
	} else
		Feep();
}


/*******************************************************************
 * Callback for "Close" button in sed popup			   *
 *******************************************************************/
void sed_close(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{

	XtPopdown(sed_popup);
	if (undo_possible) {
		unlink(tmpin_file);
	}
}


/*******************************************************************
 * Callback for "Help" menu entry				   *
 *******************************************************************/
void DoSpezialHelp(Widget w, XtPointer client_data, XtPointer call_data)
{
	char b[1024];

	sprintf(b, "siagrun help file:%s/xedplus/xedplus.html", docdir);
	MwSpawn(b);
}

void DoFileNew(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *editor = NULL /*resources.default_editor */ ;
	if (editor == NULL)
		editor = "xedplus";
	MwSpawn(editor);
}


/*******************************************************************
 * Callback for "About" menu entry			    	   *
 *******************************************************************/
void DoSpezialAbout(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	centerpopup(textwindow, about_popup);
	XtPopup(about_popup, XtGrabExclusive);
	set_wm_delete(about_popup);
}

extern Widget topbox;

void DoSpezialAboutXedplus(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char b[1024];
	sprintf(b, "XEDPLUS\n"
		"%s\n"
		"\n"
		"%s\n"
		"Ulric Eriksson, ulric@siag.nu\n"
		"Randolf Werner, University Koblenz\n"
		"Chris D. Peterson, MIT X Consortium\n"
		"\n%s\n",
		_("The interactive text editor for X"),
		_("Authors:"),
		_("Part of Siag Office"));
	MwAboutBox(topbox, "xedplus.xpm", b);
}

void DoSpezialAboutSiagOffice(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	MwAboutSiag(topbox);
}


/*******************************************************************
 * Callback for "close" button in about popup			   *
 *******************************************************************/
void about_close(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(about_popup);
}
