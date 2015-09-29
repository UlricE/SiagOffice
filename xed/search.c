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
 * Module "search.c" containing stuff for the Search menu          *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *******************************************************************/


#include "xedit.h"
#include <limits.h>

extern Widget textwindow;
extern Widget search_popup;
extern Widget start_group, direction_group;
extern Widget search_text, replace_text;
extern Widget veto_popup;
extern Display *CurDpy;

typedef struct bracketpair {
	char *open;
	char *close;
} BRACKETPAIR;
static BRACKETPAIR brackets[] =
{
	{"(", ")"},
	{"{", "}"},
	{"[", "]"},
	{"/*", "*/"},
	{NULL, NULL}};

#define CURSOR 0
#define BEGIN  1
#define END    2


/*******************************************************************
 * Center a popup in the middle/top of another widget if possible, *
 * otherwise make shure that the popup is visible on the screen    *
 *******************************************************************/
static void centertoppopup(centerin, tocenter)
Widget centerin, tocenter;
{
	Display *CurDpy;
	Position xin, yin, x, y;
	Dimension win, hin, wto, hto;
	Arg args[4];

	CurDpy = XtDisplay(centerin);
	XtRealizeWidget(tocenter);
	XtSetArg(args[0], XtNwidth, &wto);
	XtSetArg(args[1], XtNheight, &hto);
	XtGetValues(tocenter, args, 2);

	XtSetArg(args[0], XtNwidth, &win);
	XtSetArg(args[1], XtNheight, &hin);
	XtGetValues(centerin, args, 2);

	XtTranslateCoords(centerin, 0, 0, &xin, &yin);

	x = xin + win / 2 - wto / 2;
	y = yin;
	if (x < 0)
		x = 0;
	if (y < 0)
		y = 0;
	if ((x + wto) > DisplayWidth(CurDpy, DefaultScreen(CurDpy)))
		x = DisplayWidth(CurDpy, DefaultScreen(CurDpy)) - wto;
	if ((y + hto) > DisplayHeight(CurDpy, DefaultScreen(CurDpy)))
		y = DisplayHeight(CurDpy, DefaultScreen(CurDpy)) - hto;

	XtSetArg(args[0], XtNx, x);
	XtSetArg(args[1], XtNy, y);
	XtSetValues(tocenter, args, 2);
	XWarpPointer(CurDpy, None, DefaultRootWindow(CurDpy), 0, 0, 0, 0, x + wto / 2, y + hto / 2);
}


/*******************************************************************
 * Finding corresponding bracket				   *
 *******************************************************************/
static XawTextPosition find_bracket(openbracket, closebracket, position, direction)
XawTextBlock *openbracket, *closebracket;
XawTextPosition position;
XawTextScanDirection direction;
{
	XawTextPosition posopen, posclose;
	int n = 1;

	if (direction == XawsdRight) {
		while (n > 0) {
			posopen = XawTextSourceSearch(XawTextGetSource(textwindow), position, direction, openbracket);
			posclose = XawTextSourceSearch(XawTextGetSource(textwindow), position, direction, closebracket);
			if ((posopen != XawTextSearchError) && (posclose != XawTextSearchError)) {
				if (posclose < posopen) {
					position = posclose + 1;
					n--;
				} else {
					position = posopen + 1;
					n++;
				}
			}
			if (posclose == XawTextSearchError)
				return (XawTextSearchError);
			if (posopen == XawTextSearchError) {
				position = posclose + 1;
				n--;
			}
		}
		return (position - 1);
	} else {
		while (n > 0) {
			posopen = XawTextSourceSearch(XawTextGetSource(textwindow), position, direction, openbracket);
			posclose = XawTextSourceSearch(XawTextGetSource(textwindow), position, direction, closebracket);
			if ((posopen != XawTextSearchError) && (posclose != XawTextSearchError)) {
				if (posclose < posopen) {
					position = posopen;
					n--;
				} else {
					position = posclose;
					n++;
				}
			}
			if (posopen == XawTextSearchError)
				return (XawTextSearchError);
			if (posclose == XawTextSearchError) {
				position = posopen;
				n--;
			}
		}
		return (position);
	}
}



/*******************************************************************
 Callback for "Search" menu entry				   *
 *******************************************************************/
void DoSearchSearch(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	centertoppopup(textwindow, search_popup);
	XtPopup(search_popup, XtGrabNone);
	set_wm_delete(search_popup);
	XRaiseWindow(XtDisplay(search_popup), XtWindow(search_popup));
}


/*******************************************************************
 * Callback for "Search Selection" menu entry			   *
 *******************************************************************/
void DoSearchSearchselection(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	XawTextBlock block;

	block.firstPos = 0;
	block.ptr = XFetchBytes(CurDpy, &block.length);
	block.format = FMT8BIT;
	XtSetArg(args[0], XtNstring, block.ptr);
	XtSetValues(search_text, args, 1);
	XFree(block.ptr);

	centertoppopup(textwindow, search_popup);
	XtPopup(search_popup, XtGrabNone);
	set_wm_delete(search_popup);
	XRaiseWindow(XtDisplay(search_popup), XtWindow(search_popup));
}


/*******************************************************************
 * Callback for "Replace" menu entry				   *
 *******************************************************************/
void DoSearchReplace(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	centertoppopup(textwindow, search_popup);
	XtPopup(search_popup, XtGrabNone);
	set_wm_delete(search_popup);
	XRaiseWindow(XtDisplay(search_popup), XtWindow(search_popup));
}


/*******************************************************************
 * Callback for "Replace Selection" menu entry			   *
 *******************************************************************/
void DoSearchReplaceselection(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	XawTextBlock block;

	block.firstPos = 0;
	block.ptr = XFetchBytes(CurDpy, &block.length);
	block.format = FMT8BIT;
	XtSetArg(args[0], XtNstring, block.ptr);
	XtSetValues(search_text, args, 1);
	XFree(block.ptr);

	centertoppopup(textwindow, search_popup);
	XtPopup(search_popup, XtGrabNone);
	set_wm_delete(search_popup);
	XRaiseWindow(XtDisplay(search_popup), XtWindow(search_popup));
}


/*******************************************************************
 * Callback for "Cancel" button in search popup			   *
 *******************************************************************/
void search_ready(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(search_popup);
	XtPopdown(veto_popup);
}


/*******************************************************************
 * Callback for "Search" button in search popup			   *
 *******************************************************************/
void search(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextPosition startat = 0, current_pos, position;
	XawTextBlock searchblock;
	XawTextScanDirection direction;
	char *searchfor;
	Arg args[2];

	if (strcmp(XawToggleGetCurrent(start_group), "Textbegining") == 0)
		startat = 0;
	if (strcmp(XawToggleGetCurrent(start_group), "Cursor position") == 0)
		startat = XawTextGetInsertionPoint(textwindow);
	if (strcmp(XawToggleGetCurrent(start_group), "Textending") == 0)
		startat = XawTextSourceScan(XawTextGetSource(textwindow), INT_MAX, XawstEOL, XawsdRight, 1, True);

	if (strcmp(XawToggleGetCurrent(direction_group), "Forward") == 0)
		direction = XawsdRight;
	else
		direction = XawsdLeft;

	XtSetArg(args[0], XtNstring, &searchfor);
	XtGetValues(search_text, args, 1);

	searchblock.firstPos = 0;
	searchblock.length = strlen(searchfor);
	searchblock.ptr = searchfor;
	searchblock.format = FMT8BIT;

	current_pos = XawTextGetInsertionPoint(textwindow);
	XawTextSetInsertionPoint(textwindow, startat);
	position = XawTextSearch(textwindow, direction, &searchblock);
	if (position == XawTextSearchError) {
		XawTextSetInsertionPoint(textwindow, current_pos);
		Feep();
	} else {
		if (direction == XawsdRight)
			show_position(position + searchblock.length);
		else
			show_position(position);
		XawTextSetSelection(textwindow, position, position + searchblock.length);
	}
}

static int veto_result;

/*******************************************************************
 * Callback for all buttons in veto popup			   *
 *******************************************************************/
void veto_ready(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	veto_result = (int) closure;
}


/*******************************************************************
 * Manage veto popup						   * 
 *******************************************************************/
static int veto()
{
	XtAppContext app_context;
	XEvent event;

	veto_result = -1;
	app_context = XtWidgetToApplicationContext(veto_popup);
	while (veto_result == -1) {
		XtAppNextEvent(app_context, &event);
		XtDispatchEvent(&event);
	}
	return (veto_result);
}


/*******************************************************************
 * Callback for "Replace", "Replace veto" and "Replace all"        *
 * button in search popup					   *
 *******************************************************************/
void replace(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextPosition startat = 0, current_pos, position;
	XawTextBlock searchblock, replaceblock;
	XawTextScanDirection direction;
	char *searchfor, *replacewith;
	int answer;
	Boolean first_time;
	Arg args[2];

	if (strcmp(XawToggleGetCurrent(start_group), "Textbegining") == 0)
		startat = 0;
	if (strcmp(XawToggleGetCurrent(start_group), "Cursor position") == 0)
		startat = XawTextGetInsertionPoint(textwindow);
	if (strcmp(XawToggleGetCurrent(start_group), "Textending") == 0)
		startat = XawTextSourceScan(XawTextGetSource(textwindow), INT_MAX, XawstEOL, XawsdRight, 1, True);

	if (strcmp(XawToggleGetCurrent(direction_group), "Forward") == 0)
		direction = XawsdRight;
	else
		direction = XawsdLeft;

	XtSetArg(args[0], XtNstring, &searchfor);
	XtGetValues(search_text, args, 1);

	searchblock.firstPos = 0;
	searchblock.length = strlen(searchfor);
	searchblock.ptr = searchfor;
	searchblock.format = FMT8BIT;

	XtSetArg(args[0], XtNstring, &replacewith);
	XtGetValues(replace_text, args, 1);

	replaceblock.firstPos = 0;
	replaceblock.length = strlen(replacewith);
	replaceblock.ptr = replacewith;
	replaceblock.format = FMT8BIT;

	if ((int) closure == REPLACE) {
		current_pos = XawTextGetInsertionPoint(textwindow);
		XawTextSetInsertionPoint(textwindow, startat);
		position = XawTextSearch(textwindow, direction, &searchblock);
		if (position == XawTextSearchError) {
			XawTextSetInsertionPoint(textwindow, current_pos);
			Feep();
		} else {
			if (direction == XawsdRight) {
				XawTextReplace(textwindow, position, position + searchblock.length, &replaceblock);
				show_position(position + replaceblock.length);
			} else {
				XawTextReplace(textwindow, position, position + searchblock.length, &replaceblock);
				show_position(position);
			}
			XawTextSetSelection(textwindow, position, position + replaceblock.length);
		}
	}
	if ((int) closure == REPLACE_ALL) {
		position = startat;
		do {
			position = XawTextSourceSearch(XawTextGetSource(textwindow), position, direction, &searchblock);
			if (position != XawTextSearchError) {
				if (direction == XawsdRight) {
					XawTextSourceReplace(XawTextGetSource(textwindow), position, position + searchblock.length,
							  &replaceblock);
					position += replaceblock.length;
				} else {
					XawTextSourceReplace(XawTextGetSource(textwindow), position, position + searchblock.length,
							  &replaceblock);
				}
			}
		} while (position != XawTextSearchError);
		/* Redisplay all */
		position = XawTextSourceScan(XawTextGetSource(textwindow), INT_MAX, XawstEOL, XawsdRight, 1, True);
		XawTextInvalidate(textwindow, 0, position);
		XawTextEnableRedisplay(textwindow);
	}
	if ((int) closure == REPLACE_VETO) {
		first_time = True;
		XawTextSetInsertionPoint(textwindow, startat);
		while ((position = XawTextSearch(textwindow, direction, &searchblock)) != XawTextSearchError) {
			XawTextSetSelection(textwindow, position, position + searchblock.length);
			show_position(position);
			if (first_time) {
				centerpopup(search_popup, veto_popup);
				XtPopup(veto_popup, XtGrabExclusive);
				set_wm_delete(veto_popup);
				first_time = False;
			}
			answer = veto();
			if (answer == YES) {
				if (direction == XawsdRight) {
					XawTextReplace(textwindow, position, position + searchblock.length, &replaceblock);
					show_position(position + replaceblock.length);
				} else {
					XawTextReplace(textwindow, position, position + searchblock.length, &replaceblock);
					show_position(position);
				}
			}
			if (answer == NO) {
				if (direction == XawsdRight)
					show_position(position + searchblock.length);
				else
					show_position(position);
			}
			if (answer == CANCEL) {
				XtPopdown(veto_popup);
				return;
			}
		}
		XtPopdown(veto_popup);
	}
}


/*******************************************************************
 * Callback for "Find bracket" menu entry			   *
 *******************************************************************/
void DoSearchFindbracket(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextBlock openbracket, closebracket, buf;
	XawTextPosition begin, end, pos, readbegin, sellength;
	XawTextScanDirection direction = XawsdRight;
	int i;
	char buffer[1024], selstring[1024];
	Boolean found;

	openbracket.firstPos = closebracket.firstPos = buf.firstPos = 0;
	buf.length = 1024;
	buf.ptr = buffer;
	*buf.ptr = '\0';
	*selstring = '\0';
	openbracket.format = closebracket.format = buf.format = FMT8BIT;

	XawTextGetSelectionPos(textwindow, &begin, &end);
	if (begin == end)
		end++;
	sellength = end - begin;
	if (sellength > buf.length)
		sellength = buf.length;
	readbegin = begin;
	while (sellength > 0) {
		XawTextSourceRead(XawTextGetSource(textwindow), readbegin, &buf, sellength);
		strncat(selstring, buf.ptr, buf.length);
		readbegin += buf.length;
		sellength -= buf.length;
	}

	for (found = False, i = 0; (!found && (brackets[i].open != NULL)); i++) {
		if (strcmp(brackets[i].open, selstring) == 0) {
			found = True;
			direction = XawsdRight;
		}
		if (strcmp(brackets[i].close, selstring) == 0) {
			found = True;
			direction = XawsdLeft;
		}
	}
	i--;
	if (!found)
		return;

	openbracket.length = strlen(brackets[i].open);
	openbracket.ptr = brackets[i].open;
	closebracket.length = strlen(brackets[i].close);
	closebracket.ptr = brackets[i].close;

	if (direction == XawsdRight)
		pos = find_bracket(&openbracket, &closebracket, begin + 1, direction);
	else
		pos = find_bracket(&openbracket, &closebracket, end - 1, direction);
	if (pos != XawTextSearchError) {
		XawTextSetInsertionPoint(textwindow, pos);
		if (direction == XawsdRight)
			XawTextSetSelection(textwindow, begin, pos + closebracket.length);
		else
			XawTextSetSelection(textwindow, pos, end);
	} else {
	}
}


/*******************************************************************
 * Callback for "Check brackets" menu entry			   *
 *******************************************************************/
void DoSearchCheckbrackets(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XawTextBlock openbracket, closebracket;
	XawTextPosition position;
	int i = 0;
	char bracketstring[256], errorstring[256], mess[256];

	strcpy(errorstring, "");
	openbracket.firstPos = closebracket.firstPos = 0;
	openbracket.format = closebracket.format = FMT8BIT;

	while (brackets[i].open != NULL) {
		openbracket.ptr = brackets[i].open;
		openbracket.length = strlen(brackets[i].open);
		closebracket.ptr = brackets[i].close;
		closebracket.length = strlen(brackets[i].close);
		position = 0;
		while ((position = XawTextSourceSearch(XawTextGetSource(textwindow), position, XawsdRight, &openbracket)) != XawTextSearchError) {
			if (find_bracket(&openbracket, &closebracket, position + 1, XawsdRight) == XawTextSearchError) {
				sprintf(bracketstring, " '%s'", brackets[i].close);
				strcat(errorstring, bracketstring);
				break;
			}
			position++;
		}
		position = XawTextSourceScan(XawTextGetSource(textwindow), INT_MAX, XawstEOL,
					     XawsdRight, 1, True);
		while ((position = XawTextSourceSearch(XawTextGetSource(textwindow), position, XawsdLeft, &closebracket)) != XawTextSearchError) {
			if (find_bracket(&openbracket, &closebracket, position, XawsdLeft) == XawTextSearchError) {
				sprintf(bracketstring, " '%s'", brackets[i].open);
				strcat(errorstring, bracketstring);
				break;
			}
		}
		i++;
	}
	if (strcmp(errorstring, "") != 0) {
		sprintf(mess, "Missing: %s", errorstring);
		error(mess);
	}
}
