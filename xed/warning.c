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
 * Module "warning.c" containing stuff for warning and error       *
 * popups.							   *
 *								   *
 * Randolf Werner						   *
 * University Koblenz						   *
 * Germany , 6.3.91						   *
 *******************************************************************/

#include "xedit.h"

#include "../common/common.h"

/* Used global widgets */
extern Widget error_label, error_popup;
extern Widget warn_label, warn_popup;
extern Widget textwindow;
extern Display *CurDpy;


/*******************************************************************
 * Display an error message 					   *
 *******************************************************************/
void error(message)
char *message;
{
	Arg args[2];

	if (error_popup == NULL)
		return;
	XtSetArg(args[0], XtNlabel, _(message));
	XtSetValues(error_label, args, 1);
	centerpopup(textwindow, error_popup);
	XtPopup(error_popup, XtGrabExclusive);
	set_wm_delete(error_popup);
	Feep();
}


/*******************************************************************
 * Callback for Ok button in the error popup			   *
 *******************************************************************/
void error_ready(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(error_popup);
}


static int warn_result = NORESULT;


/*******************************************************************
 * Display an warning message.					   *
 * Return "ABORT" or "CONTINUE"					   *
 *******************************************************************/
int warning(message)
char *message;
{
	Arg args[2];
	XtAppContext app_context;
	XEvent event;
	int result;

	XtSetArg(args[0], XtNlabel, _(message));
	XtSetValues(warn_label, args, 1);
	centerpopup(textwindow, warn_popup);

	XtPopup(warn_popup, XtGrabExclusive);
	set_wm_delete(warn_popup);
	Feep();
	app_context = XtWidgetToApplicationContext(warn_popup);
	while (warn_result == NORESULT) {
		XtAppNextEvent(app_context, &event);
		XtDispatchEvent(&event);
	}
	result = warn_result;
	warn_result = NORESULT;
	return (result);
}


/*******************************************************************
 * Callback for ABORT and CONTINUE Button in the warning popup     *
 *******************************************************************/
void warn_ready(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(warn_popup);
	warn_result = (int) closure;
}
