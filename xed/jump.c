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
 * Module "jump.c" containing stuff for jump menu                  *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *******************************************************************/


#include <stdlib.h>
#include "xedit.h"

extern Widget textwindow;
extern Widget line_popup;
extern Widget line_text;


/*******************************************************************
 * Function "show_position" sets insertion point to "position"     *
 * and scrolls it to the middle of the displayed text              *
 *******************************************************************/
void show_position(position)
XawTextPosition position;
{
	Arg args[2];
	int nlines;
	Dimension height;
	Widget sink;
	XawTextPosition pos;

	XtSetArg(args[0], XtNheight, &height);
	XtSetArg(args[1], XtNtextSink, &sink);
	XtGetValues(textwindow, args, 2);
	nlines = XawTextSinkMaxLines(sink, height) / 2;
	pos = XawTextSourceScan(XawTextGetSource(textwindow), position, XawstEOL, XawsdLeft, nlines, True);
	if (pos > 0)
		pos++;
	XtSetArg(args[0], XtNdisplayPosition, pos);
	XtSetValues(textwindow, args, 1);
	XawTextSetInsertionPoint(textwindow, position);
}


/*******************************************************************
 * Callback for cancel line jump                                   *
 *******************************************************************/
void jumpline_close(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(line_popup);
}

/*******************************************************************
 * Callback for "Line" menu entry				   *
 *******************************************************************/
void DoJumpLine(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	Position x, y;
	XawTextPosition insert_pos, pos;
	int length;
	char linestring[256];
	XawTextBlock block;
	register char *c;
	register int n;
	register int line_nr;
	Dimension w, h;

/* Get actual Line Number */
	insert_pos = XawTextGetInsertionPoint(textwindow);
	pos = 0;
	line_nr = 1;

	length = insert_pos - pos;
	while (length > 0) {
		XawTextSourceRead(XawTextGetSource(textwindow), pos, &block, length);
		pos += block.length;
		length -= block.length;
		for (c = block.ptr, n = 0; n < block.length; c++, n++)
			if (*c == '\n')
				line_nr++;
	}

	sprintf(linestring, "%d", line_nr);
	XtSetArg(args[0], XtNstring, linestring);
	XtSetValues(line_text, args, 1);
	XawTextSetInsertionPoint(line_text, 255);

/* Popup Dialog */
	centerpopup(textwindow, line_popup);
	XtSetArg(args[0], XtNwidth, &w);
	XtSetArg(args[1], XtNheight, &h);
	XtGetValues(line_text, args, 2);
	XtTranslateCoords(line_text, 0, 0, &x, &y);
	XWarpPointer(XtDisplay(line_popup), None, DefaultRootWindow(XtDisplay(line_popup)),
		     0, 0, 0, 0, x + w / 2, y + h / 2);
	XtPopup(line_popup, XtGrabExclusive);
	set_wm_delete(line_popup);
}


/*******************************************************************
 * Callback for "Begin" menu entry				   *
 *******************************************************************/
void DoJumpBegin(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextSetInsertionPoint(textwindow, 0);
}


/*******************************************************************
 * Callback for "End" menu entry				   *
 *******************************************************************/
void DoJumpEnd(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextSetInsertionPoint(textwindow, 9999999);
}


/*******************************************************************
 * Callback for "Selection Start" menu entry			   *
 *******************************************************************/
void DoJumpSelStart(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextPosition begin, end;

	XawTextGetSelectionPos(textwindow, &begin, &end);
	show_position(begin);
}


/*******************************************************************
 * Callback for "Selection End" menu entry			   *
 *******************************************************************/
void DoJumpSelEnd(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextPosition begin, end;

	XawTextGetSelectionPos(textwindow, &begin, &end);
	show_position(end);
}


/*******************************************************************
 * Jump to line number                                             *
 *******************************************************************/
void goto_line2(line_nr)
int line_nr;
{
	int pos, nr;
	register int n;
	register char *c;
	XawTextBlock block;

	pos = 0;
	nr = 1;
	block.length = 1;
	while (block.length > 0) {
		XawTextSourceRead(XawTextGetSource(textwindow), pos, &block, 9999999);
		for (c = block.ptr, n = 0; n < block.length; c++, n++) {
			if (*c == '\n')
				nr++;
			if (nr == line_nr) {
				show_position(pos + n + 1);
				return;
			}
		}
		pos += block.length;
	}
}


/*******************************************************************
 * Action Procedure for pressing <RETURN> in linenumber popup      *
 *******************************************************************/
void goto_line(widget, event, params, n_params)
Widget widget;
XEvent *event;
String *params;
Cardinal *n_params;
{
	Arg args[2];
	char *inputstring;
	int line_nr;

	XtSetArg(args[0], XtNstring, &inputstring);
	XtGetValues(widget, args, 1);
	line_nr = atoi(inputstring);
	XtPopdown(line_popup);

	goto_line2(line_nr);
}
