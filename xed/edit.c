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
 * Module "edit.c" containing stuff for the "Edit" menu             *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *******************************************************************/


#include "xedit.h"

extern Widget textwindow;
extern Display *CurDpy;


#ifdef SCROLLBREAK
void cursor_down(widget, event, params, n_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *n_params;
{
	static long lastnr;
	static int ntimes;
	static int first = 1;

	if (first) {
		first = 0;
		ntimes = app_resources.maxScrollbreak;
	}
	if ((lastnr == event->xany.serial) && (ntimes > 0)) {
		ntimes--;
		return;
	} else
		ntimes = app_resources.maxScrollbreak;
	lastnr = event->xany.serial;
	XtCallActionProc(textwindow, "next-line", event, params, n_params);
	XFlush(CurDpy);
}


void cursor_up(widget, event, params, n_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *n_params;
{
	static long lastnr;
	static int ntimes;
	static int first = 1;

	if (first) {
		first = 0;
		ntimes = app_resources.maxScrollbreak;
	}
	if ((lastnr == event->xany.serial) && (ntimes > 0)) {
		ntimes--;
		return;
	} else
		ntimes = app_resources.maxScrollbreak;
	lastnr = event->xany.serial;
	XtCallActionProc(textwindow, "previous-line", event, params, n_params);
	XFlush(CurDpy);
}
#endif				/* SCROLLBREAK */


/*******************************************************************
 * Callback for "Cut" menu entry				   *
 *******************************************************************/
void DoEditCut(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
#if 0
	XawTextBlock cutblock;
	XawTextPosition begin, end;

	cutblock.firstPos = 0;
	cutblock.length = 0;
	cutblock.ptr = NULL;
	cutblock.format = FMT8BIT;
	XawTextGetSelectionPos(textwindow, &begin, &end);
	XawTextReplace(textwindow, begin, end, &cutblock);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
#else
	XtCallActionProc(textwindow, "kill-selection",
			NULL, NULL, 0);
#endif
}


/*******************************************************************
 * Callback for "Paste" menu entry				   *
 *******************************************************************/
void DoEditPaste(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
#if 0
	XawTextBlock pasteblock;
	XawTextPosition begin, end, pos;

	XawTextGetSelectionPos(textwindow, &begin, &end);
	pasteblock.firstPos = 0;
	pasteblock.ptr = XFetchBytes(CurDpy, &pasteblock.length);
	pasteblock.format = FMT8BIT;
	pos = XawTextGetInsertionPoint(textwindow);
	XawTextReplace(textwindow, pos, pos, &pasteblock);
	XFree(pasteblock.ptr);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
#else
	String p[2] = {"CLIPBOARD", "CLIPBOARD"};
	XtCallActionProc(textwindow, "insert-selection",
			NULL, p, 2);
#endif
}

void DoEditCopy(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	String p[2] = {"CLIPBOARD", "CLIPBOARD"};
	XtCallActionProc(textwindow, "kill-selection",
			NULL, NULL, 0);
	XtCallActionProc(textwindow, "insert-selection",
			NULL, p, 2);
}

/*******************************************************************
 * Callback for "Shift Selection Right" menu entry		   *
 *******************************************************************/
void DoEditShiftSelRight(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextBlock tabblock;
	XawTextPosition begin, end, pos;

	tabblock.firstPos = 0;
	tabblock.ptr = "\t";
	tabblock.length = 1;
	tabblock.format = FMT8BIT;

	XawTextGetSelectionPos(textwindow, &begin, &end);
	XawTextSetInsertionPoint(textwindow, begin);
	XawTextDisableRedisplay(textwindow);
	XawTextReplace(textwindow, begin, begin, &tabblock);
	while ((pos = XawTextSourceScan(XawTextGetSource(textwindow), begin, XawstEOL, XawsdRight, 1, True)) < end) {
		XawTextReplace(textwindow, pos, pos, &tabblock);
		begin = pos + 1;
		end++;
	}
	XawTextEnableRedisplay(textwindow);
	XawTextSetInsertionPoint(textwindow, end);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
}


/*******************************************************************
 * Callback for "Shift Selection Left" menu entry		   *
 *******************************************************************/
void DoEditShiftSelLeft(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextBlock search, search2, replace, replace2;
	XawTextPosition begin, end, pos;

	replace2.firstPos = search2.firstPos = search.firstPos = replace.firstPos = 0;
	replace2.format = search2.format = search.format = replace.format = FMT8BIT;
	replace2.ptr = "";
	search2.ptr = "\t";
	search.ptr = "\n\t";
	replace.ptr = "\n";
	replace2.length = 0;
	search2.length = 1;
	search.length = 2;
	replace.length = 1;

	XawTextGetSelectionPos(textwindow, &begin, &end);
	XawTextSetInsertionPoint(textwindow, begin);
	XawTextDisableRedisplay(textwindow);

	pos = XawTextSourceSearch(XawTextGetSource(textwindow), begin, XawsdRight, &search2);
	if (pos == begin) {
		XawTextReplace(textwindow, pos, pos + 1, &replace2);
		end--;
	}
	while ((pos = XawTextSourceSearch(XawTextGetSource(textwindow), begin, XawsdRight, &search)) < end) {
		if (pos == XawTextSearchError)
			break;
		XawTextReplace(textwindow, pos, pos + 2, &replace);
		begin = pos + 1;
		end--;
	}
	XawTextEnableRedisplay(textwindow);
	XawTextSetInsertionPoint(textwindow, end);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
}
