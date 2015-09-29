/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2003  Ulric Eriksson <ulric@siag.nu>

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <X11/cursorfont.h>
#include <X11/keysym.h>


/* solve keysym problems with ancient X11/keysymdef.h file on Solaris 2.5 */

#ifndef XK_KP_Left
#define XK_KP_Left 0xFF96
#endif
#ifndef XK_KP_Right
#define XK_KP_Right 0xFF98
#endif
#ifndef XK_KP_Up
#define XK_KP_Up 0xFF97
#endif
#ifndef XK_KP_Down
#define XK_KP_Down 0xFF99
#endif

#ifndef XK_KP_Home
#define XK_KP_Home 0xFF95
#endif
#ifndef XK_KP_End
#define XK_KP_End 0xFF9C
#endif
#ifndef XK_Page_Up
#define XK_Page_Up XK_Prior
#endif
#ifndef XK_KP_Page_Up
#define XK_KP_Page_Up 0xFF9A
#endif
#ifndef XK_Page_Down
#define XK_Page_Down XK_Next
#endif
#ifndef XK_KP_Page_Down
#define XK_KP_Page_Down 0xFF9B
#endif

#ifndef XK_KP_Delete
#define XK_KP_Delete 0xFF9F
#endif


#include <X11/IntrinsicP.h>	/* for XtResizeWidget */
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>		/* for XtNtitle */
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Repeater.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/List.h>
#include <X11/xpm.h>

#include <Mowitz/Mowitz.h>

#include "../siod/siod.h"

#include "../common/common.h"

#include "../common/bitmaps/siag.xpm"

#include "../xcommon/embed.h"
#include "../xcommon/xcommon.h"
#include "../xcommon/plugin.h"

#include "../siag/calc.h"

#include "xsiag.h"
/*#include "drop.h"*/

#define MULTI_CLICK_TIME 500

#define MENUBAR (1)
#define TOOLBAR (2)
#define FORMATBAR (4)

#define APPNAME "Siag"

String fallback_resources[] = {
#include "../xcommon/xcommon-ad.h"
#include "../xcommon/filesel-ad.h"
#include "../xcommon/dialogs-ad.h"
#include "../xcommon/nws-ad.h"
#include "app-defaults.h"
	NULL
};

typedef struct {
	Boolean plugin;
	Boolean grid_only;
} AppData;

static AppData app_data;

#define XtNplugin "plugin"
#define XtCPlugin "Plugin"
#define XtNgridOnly "gridOnly"
#define XtCGridOnly "GridOnly"

static XtResource resources[] = {
	{
	 XtNplugin,
	 XtCPlugin,
	 XtRBoolean,
	 sizeof(Boolean),
	 XtOffsetOf(AppData, plugin),
	 XtRImmediate,
	 (XtPointer) False,
	 }
	, {
	   XtNgridOnly,
	   XtCGridOnly,
	   XtRBoolean,
	   sizeof(Boolean),
	   XtOffsetOf(AppData, grid_only),
	   XtRImmediate,
	   (XtPointer) False}
};

static XrmOptionDescRec options[] = {
	{"-plugin", "*plugin", XrmoptionNoArg, "True"},
	{"-gridonly", "*gridOnly", XrmoptionNoArg, "True"}
};

static XtAppContext app_context;

Widget topLevel;

static Widget topbox, frame1, frame2, frame3, menubox, toolbox, formatbox;
static Widget textbox, statusbox;
static Widget gridpane;
static Widget textframe, text1, label1, label2, label3;

static Widget btnFont, btnSize, btnStyle, btnColor;
static Widget cmdBold, cmdItalic, cmdUline, cmdHLeft, cmdHCenter,
    cmdHRight;
static Widget cmdBorders, cmdGrid, cmdUline, cmdLline, cmdRline, cmdNone;
static Widget shortcuts, tooltip;

float zoom = 1.0;

static int bars = 0;

static int status;
static int we_have_selection, start_row, start_col;
static int we_have_clipboard;
static int clip_size;
static char *clip_buffer, *clip_string;

static MwFmt fmt0 =
    { "Helvetica", 100, 0, 0, 0, 0, "black", "white", 0, 0, 0, 0 };
static MwFmt fmt1 =
    { "Helvetica", 120, 0, 0, 0, 0, "black", "grey", 0, MW_VADJ_CENTER,
MW_HADJ_CENTER, 0 };

/* Selection stuff */

/* ---
   Module name:    selection.c

   This module handles selection: grabbing, releasing, cutting, pasting.

   The selection uses a custom target SIAG_BLOCK which has this format:

   <HEIGHT>\0<WIDTH>\0<TYPE><TEXT>\0<TYPE><TEXT>\0...

   HEIGHT = numbers of rows in selection
   WIDTH = number of columns in selection
   TYPE = type char where:
	'"' = label
	'#' = empty
	'=' = expression
	'$' = string
	'+' = expression with named interpreter
	'm' = image
	'n' = new line (don't waste effort copying empty cells)
	'z' = end of selection (don't copy empty rows)
   TEXT = verbatim text from the cell

   This is pretty similar to the file format but different enough to
   be confusing.
--- */

#if 0
#include "../siag/selection.h"
#endif

Atom target_atom;		/* used for selection */
Atom clipboard;
Atom *drop_types;

Boolean convert_proc(Widget w,
		     Atom * selection, Atom * target, Atom * type_return,
		     XtPointer * value_return,
		     unsigned long *length_return, int *format_return)
{
	unsigned int lr;
	window *wl = find_window_by_widget(w);
	XSelectionRequestEvent *req = XtGetSelectionRequest(w,
							    *selection,
							    (XtRequestId)
							    NULL);

	/* handle all required atoms, and the one that we use */
	/* Xt already handles MULTIPLE, no branch necessary */
	if (*target == XA_TARGETS(XtDisplay(w))) {
		Atom *targetP;
		Atom *std_targets;
		unsigned long std_length;
		XmuConvertStandardSelection(w, req->time, selection,
					    target, type_return,
					    (XPointer *) & std_targets,
					    &std_length, format_return);
		*value_return = XtMalloc(sizeof(Atom) * (std_length + 1));
		targetP = *(Atom **) value_return;
		*length_return = std_length + 1;
		*targetP++ = target_atom;
		memcpy(std_targets, targetP, sizeof(Atom) * std_length);
		XtFree((char *) std_targets);
		*type_return = XA_ATOM;
		*format_return = sizeof(Atom) * 8;
		return True;
	} else if (*target == target_atom) {
		/* handle normal selection */
		char *data = pack_area(wl->buf, wl->bsht,
				       block_upper(wl).row,
				       block_upper(wl).col,
				       block_lower(wl).row,
				       block_lower(wl).col,
				       &lr);
		*length_return = lr;
		*value_return = XtMalloc(*length_return);
		memcpy(*value_return, data, *length_return);
		MwFree(data);

		*type_return = target_atom;

		*format_return = 8;
		return True;
	} else if (*target == XA_STRING) {
		/* handle string selections (from outside Siag) */
		char *data;
#if 0
		*length_return = 32000;
		data = XtMalloc(*length_return);
		pack_string_selection(wl->buf, data,
				      wl->bsht,
				      block_upper(wl).row,
				      block_upper(wl).col,
				      block_lower(wl).row,
				      block_lower(wl).col);
		*value_return = data;
		*length_return = strlen(data);
#else
		data = pack_string(wl->buf, wl->bsht,
				   block_upper(wl).row,
				   block_upper(wl).col,
				   block_lower(wl).row,
				   block_lower(wl).col);
		*length_return = strlen(data);
		*value_return = XtMalloc(*length_return + 1);
		strcpy(*value_return, data);
		MwFree(data);
#endif
		*type_return = XA_STRING;
		*format_return = 8;
		return True;
	} else {
		if (XmuConvertStandardSelection(w, CurrentTime, selection,
						target, type_return,
						(XPointer *) value_return,
						length_return,
						format_return))
			return True;
		else {
			return False;
		}
	}
 /*NOTREACHED*/}

Boolean convert_clip_proc(Widget w,
			  Atom * selection, Atom * target,
			  Atom * type_return, XtPointer * value_return,
			  unsigned long *length_return, int *format_return)
{
	XSelectionRequestEvent *req = XtGetSelectionRequest(w,
							    *selection,
							    (XtRequestId)
							    NULL);

	/* handle all required atoms, and the one that we use */
	/* Xt already handles MULTIPLE, no branch necessary */
	if (*target == XA_TARGETS(XtDisplay(w))) {
		Atom *targetP;
		Atom *std_targets;
		unsigned long std_length;
		XmuConvertStandardSelection(w, req->time, selection,
					    target, type_return,
					    (XPointer *) & std_targets,
					    &std_length, format_return);
		*value_return = XtMalloc(sizeof(Atom) * (std_length + 1));
		targetP = *(Atom **) value_return;
		*length_return = std_length + 1;
		*targetP++ = target_atom;
		memcpy(std_targets, targetP, sizeof(Atom) * std_length);
		XtFree((char *) std_targets);
		*type_return = XA_ATOM;
		*format_return = sizeof(Atom) * 8;
		return True;
	} else if (*target == target_atom) {
		/* handle normal selection */
		char *data = clip_buffer;
		*length_return = clip_size;
		*value_return = XtMalloc(*length_return);
		memcpy(*value_return, data, *length_return);

		*type_return = target_atom;

		*format_return = 8;
		return True;
	} else if (*target == XA_STRING) {
		/* handle string selections (from outside Siag) */
		char *data = clip_string;
		*length_return = strlen(data);
		*value_return = XtMalloc(*length_return + 1);
		strcpy(*value_return, data);
		*type_return = XA_STRING;
		*format_return = 8;
		return True;
	} else {
		if (XmuConvertStandardSelection(w, CurrentTime, selection,
						target, type_return,
						(XPointer *) value_return,
						length_return,
						format_return))
			return True;
		else {
			return False;
		}
	}
 /*NOTREACHED*/}

void lose_ownership_proc(Widget w, Atom * selection)
{
	window *wl = find_window_by_widget(w);

	we_have_selection = 0;
	wl->bsht = -1;
	set_top(wl, make_position(-1, -1));
	pr_scr_flag = TRUE;	/* make sure it's redrawn */
	show_cur(wl);
}

static void drop_clipboard(void)
{
	if (!we_have_clipboard)
		return;
	we_have_clipboard = 0;
	XtDisownSelection(w_list->ui->grid, clipboard, CurrentTime);
	MwFree(clip_buffer);
	MwFree(clip_string);
	clip_buffer = clip_string = NULL;
}

void lose_clip_proc(Widget w, Atom * selection)
{
	drop_clipboard();
}

void string_requestor_callback(Widget w, XtPointer client_data,
			       Atom * selection, Atom * type,
			       XtPointer value, unsigned long *length,
			       int *format)
{
	if ((value == NULL) && (*length == 0)) {
		XBell(XtDisplay(w), 100);
		XtWarning("Siag: no selection or selection timed out\n");
	} else {
		unpack_string(w_list->buf, (char *) value,
			      w_list->sht,
			      get_point(w_list).row,
			      get_point(w_list).col);
		w_list->buf->change = TRUE;
		calc_matrix(w_list->buf);
		XtFree((char *) value);
		pr_scr_flag = TRUE;
	}
}

void requestor_callback(Widget w, XtPointer client_data,
			Atom * selection, Atom * type, XtPointer value,
			unsigned long *length, int *format)
{
	if ((value == NULL) && (*length == 0)) {
		/* if we asked for a SIAG_BLOCK and got a null response,
		   we'll ask again, this time for an XA_STRING */
		XtGetSelectionValue(w, *selection, XA_STRING, string_requestor_callback, NULL, CurrentTime);	/* NULL is bogus event */
	} else {
		unpack_area(w_list->buf, (char *) value,
			    w_list->sht,
			    get_point(w_list).row, get_point(w_list).col);
		w_list->buf->change = TRUE;
		calc_matrix(w_list->buf);
		XtFree((char *) value);
		pr_scr_flag = TRUE;
	}
}

static void drop_selection(void)
{
	if (!we_have_selection)
		return;
	we_have_selection = 0;
	XtDisownSelection(w_list->ui->grid, XA_PRIMARY, CurrentTime);
	w_list->bsht = -1;
	set_top(w_list, make_position(-1, -1));
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static void start_selection(void)
{
	if (we_have_selection)
		return;
	if (XtOwnSelection(w_list->ui->grid, XA_PRIMARY,
			   CurrentTime, convert_proc,
			   lose_ownership_proc, NULL) == False) {
		XtWarning("Siag failed to become selection owner\n");
		return;
	}
	we_have_selection = 1;
	start_row = get_point(w_list).row;
	start_col = get_point(w_list).col;
}

static void start_clipboard(void)
{
	position p1, p2;
	p1 = block_upper(w_list);
	p2 = block_lower(w_list);
	drop_clipboard();
	if (XtOwnSelection(w_list->ui->grid, clipboard,
			   CurrentTime, convert_clip_proc,
			   lose_clip_proc, NULL) == False) {
		XtWarning("Siag failed to become clipboard owner\n");
		return;
	}
	clip_buffer = pack_area(w_list->buf, w_list->sht,
				p1.row, p1.col, p2.row, p2.col,
				&clip_size);
	clip_string =
	    pack_string(w_list->buf, w_list->sht, p1.row, p1.col, p2.row,
			p2.col);

	we_have_clipboard = 1;
}

static void set_selection(void)
{
	int r1, c1, r2, c2;
	position p = get_point(w_list);
	if (start_row < p.row)
		r1 = start_row, r2 = p.row;
	else
		r1 = p.row, r2 = start_row;
	if (start_col < p.col)
		c1 = start_col, c2 = p.col;
	else
		c1 = p.col, c2 = start_col;
	w_list->bsht = w_list->sht;
	set_blku_row(w_list, r1);
	set_blku_col(w_list, c1);
	set_blkl_row(w_list, r2);
	set_blkl_col(w_list, c2);
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

/* End of selection stuff */

unsigned int rowcol_format(void)
{
	static int rowcol_fmt_index = -1;
	if (rowcol_fmt_index == -1)
		rowcol_fmt_index = MwEncodeFormat(~0, &fmt1);
	return rowcol_fmt_index;	/* allocate in init_windows */
}

static void DialogCancelAction(Widget w, XEvent * event,
			       String * params, Cardinal * n)
{
	status = ABORT;
}

static void DialogDoneAction(Widget w, XEvent * event,
			     String * params, Cardinal * n)
{
	status = DONE;
	if (*n == 1) {
		if (!strcmp(*params, "Left"))
			status = GOLEFT;
		else if (!strcmp(*params, "Right"))
			status = GORIGHT;
		else if (!strcmp(*params, "Up"))
			status = GOUP;
		else if (!strcmp(*params, "Down"))
			status = GODOWN;
	}
}

static XtActionsRec input_actions[] = {
	{"celledit-done", DialogDoneAction},
	{"celledit-cancel", DialogCancelAction}
};

static void draw_input(char *text)
{
	int r, c;
	int s;
	int type, intp;
	char b[100];

	if (label1 == None)
		return;

	r = get_point(w_list).row;
	c = get_point(w_list).col;
	s = w_list->sht;
	sprintf(b, "[%d,%d]", r, c);
	MwLabelSet(label1, b);
	type = ret_type(w_list->buf, s, r, c);
	intp = ret_interpreter(w_list->buf, s, r, c);
	switch (type) {
	case EMPTY:
		strcpy(b, "EMPTY");
		break;
	case LABEL:
		strcpy(b, "LABEL");
		break;
	case EXPRESSION:
	case STRING:
		sprintf(b, "%s", interpreter2name(intp));
		break;
	case CONSTANT:
		strcpy(b, "CONSTANT");
		break;
	case MNUMBER:
		strcpy(b, "MNUMBER");
		break;
	case MTEXT:
		strcpy(b, "MTEXT");
		break;
	default:
		strcpy(b, "ERROR");
	}
	MwLabelSet(label3, b);
	MwRichtextSetString(text1, text);
}

static void draw_status(char *text)
{
	if (label2 != None) {
		MwLabelSet(label2, text);
		XFlush(XtDisplay(label2));
	}
}

/* ---
Print the string p on the bottom line of the screen.  If p is empty and
   the last string printed was also empty, the string isn't printed.
*/

void llpr(char *p)
{
	static int isclear = FALSE;

	if (isclear && p[0] == '\0')
		return;
	isclear = (p[0] == '\0');

	draw_status(p);
}

/* assuming cells are 20 high */
static void draw_vbar(window * w)
{
	float pos, size;
	float visible_cells = w->buf->sht[w->sht].alloc_lines + 50;
	float wh = MwHeightGet(w->ui->grid) / 20;

	if (w->ui->vscroll != None) {
		pos = (get_top(w).row - 1) / visible_cells;
		size = wh / visible_cells;
		XawScrollbarSetThumb(w->ui->vscroll, pos, size);
	}
}

/* assuming cells are 80 wide */
static void draw_hbar(window * w)
{
	float pos, size;
	float visible_cells = w->buf->sht[w->sht].longest_line + 50;
	float ww = MwWidthGet(w->ui->grid) / 80;

	if (w->ui->hscroll != None) {
		pos = (get_top(w).col - 1) / visible_cells;
		size = ww / visible_cells;
		XawScrollbarSetThumb(w->ui->hscroll, pos, size);
	}
}

static void draw_scrollbars(window * w)
{
	draw_vbar(w);
	draw_hbar(w);
}

/* This is a wrapper, so that colnum_text can be shared */
static char *colnum_text_wrapper(XtPointer p, int row, int col)
{
	window *w = (window *) p;
	return colnum_text(w->buf, col);
}

/* ---
970427: prot
970812: MwTable widget
*/

static void draw_colnums(window * w)
{
	if (w->ui->colnum != None) {
		XtVaSetValues(w->ui->colnum,
			      XtNtableProtCol, get_prot(w).col,
			      XtNtableTopCol, get_top(w).col,
			      XtNtableData, w,
			      XtNtableRedisplay, True, (char *) NULL);
	}
}

static void draw_rownums(window * w)
{
	if (w->ui->rownum != None) {
		XtVaSetValues(w->ui->rownum,
			      XtNtableProtRow, get_prot(w).row,
			      XtNtableTopRow, get_top(w).row,
			      XtNtableData, w,
			      XtNtableRedisplay, True, (char *) NULL);
	}
}

/* ---
How to make Siag always draw the top row and/or the left column.

This is desirable to keep headers on the screen when the sheet
is scrolled. To make this possible, I need to add a field in
the window structure to specify the first unprotected cell.
Everything above and to the left of this cell (called w->prot)
should always be drawn, regardless of what top is.

While this sounds easy, it profoundly changes the mapping from
cell coordinates to screen coordinates and vice versa. For instance,
answering the question "is the cursor on the screen" is no longer
as easy.

Make top be the first unprotected column. Point can never leave
the unprotected area. Leave mark and other positions for now.
--- */

static int tcell_width(XtPointer p, int col)
{
	window *w = (window *) p;
	if (p == NULL) {
		fprintf(stderr, "Warning: data is NULL\n");
		return 80;
	}
	return cell_width(w->buf, w->sht, col);
}

static int tcell_height(XtPointer p, int row)
{
	window *w = (window *) p;
	if (p == NULL) {
		fprintf(stderr, "Warning: data is NULL\n");
		return 20;
	}
	if (w->buf == NULL || w->sht >= w->buf->nsht)
		return 20;
	return cell_height(w->buf, w->sht, row);
}

static int tcolnum_width(XtPointer p, int col)
{
	return zoom * tcell_width(p, col);
}

static int trownum_height(XtPointer p, int row)
{
	return zoom * tcell_height(p, row);
}

static int cell_type(XtPointer p, int row, int col)
{
	window *w = (window *) p;
	if (ret_type(w->buf, w->sht, row, col) == EMBED)
		return MW_TABLE_EMBED;

	return MW_TABLE_TEXT;
}

static char *cell_text(XtPointer p, int row, int col)
{
	window *w = (window *) p;
	static char s[1024];
	s[0] = '\0';
	ret_pvalue(s, w->buf, w->sht, row, col, -1);
	return s;
}

static unsigned int cell_format(XtPointer p, int row, int col)
{
	window *w = (window *) p;
	return ret_format(w->buf, w->sht, row, col);
}

/* ---
970427: First shot at implementing this protection
970812: Use the MwTable widget's own drawing routines
*/

static void draw_cells(window * w)
{
	int top_row, top_col;
	position prot, top, blku, blkl, point;

	prot = get_prot(w);
	top = get_top(w);
	if (w->sht == w->bsht) {
		blku = block_upper(w);
		blkl = block_lower(w);
	} else {
		blku.row = blku.col = blkl.row = blkl.col = -1;
	}
	point = get_point(w);

	/* this is more verbose than necessary, but I want to set
	   every resource there is just to see if they work ;-) */
	MwTableSetZoom(w->ui->grid, zoom);
	XtVaSetValues(w->ui->grid,
		      XtNtableMaxRow, BUFFER_ROWS,
		      XtNtableMaxCol, BUFFER_COLS,
		      XtNtableProtRow, prot.row,
		      XtNtableProtCol, prot.col,
		      XtNtableTopRow, top.row,
		      XtNtableTopCol, top.col,
		      XtNtableSelectTopRow, blku.row,
		      XtNtableSelectBottomRow, blkl.row,
		      XtNtableSelectLeftCol, blku.col,
		      XtNtableSelectRightCol, blkl.col,
		      XtNtablePointRow, point.row,
		      XtNtablePointCol, point.col,
		      XtNtableType, cell_type,
		      XtNtableText, cell_text,
		      XtNtableFormat, cell_format,
		      XtNtableData, w,
		      XtNtableRedisplay, True,
		      XtNtableGridLines, grid_lines, (char *) NULL);
	/* there is a chance that this moved top */
	XtVaGetValues(w->ui->grid,
		      XtNtableTopRow, &top_row,
		      XtNtableTopCol, &top_col, (char *) NULL);
	if (top_row != top.row && w->ui->rownum != None) {
		set_top_row(w, top_row);
		XtVaSetValues(w->ui->rownum,
			      XtNtableTopRow, top_row,
			      XtNtableRedisplay, !pr_scr_flag,
			      (char *) NULL);
	}
	if (top_col != top.col && w->ui->colnum != None) {
		set_top_col(w, top_col);
		XtVaSetValues(w->ui->colnum,
			      XtNtableTopCol, top_col,
			      XtNtableRedisplay, !pr_scr_flag,
			      (char *) NULL);
	}
}

/* ---
*/
int ask_for_str_comp(char *prompt, char *buffr, int (*comp) (char *))
{
	return MwDialogInput(topLevel, prompt, buffr);
}

/* ---
edit_text is the Richtext widget.
winw is the total zoomed width of the grid widget.
x is the zoomed offset.
tw is the unzoomed width of the text plus the unzoomed width of the left margin.

1. If the current width of edit_text is >= the zoomed text width,
   set top_col to 0 and return.
2. Otherwise calculate the required zoomed width for edit_text.
3. If the required width will fit in the grid, resize edit_text and set
   top_col to 0.
4. Otherwise make edit_text as large as possible.
*/

static void resize_edit_text(Widget edit_text, int winw, int x, int tw,
			     int h)
{
	Dimension w;

	XtVaGetValues(edit_text, XtNwidth, &w, (char *) 0);
	if (zoom * tw < w) {
		XtVaSetValues(edit_text, XtNrichtextTopCol, 0, (char *) 0);
	} else if (zoom * tw < winw - x) {
		XtResizeWidget(edit_text, zoom * tw, h, 1);
		XtVaSetValues(edit_text, XtNrichtextTopCol, 0, (char *) 0);
	} else {
		XtResizeWidget(edit_text, winw - x, h, 1);
	}
}

/*
A hack to allow the user to enter references by clicking in the grid.
We don't need another grab, one is in effect already.
*/
static void look_for_ref(XEvent * event, int *r1, int *c1, int *r2,
			 int *c2)
{
	int again = 1;
	XEvent event_return;
	int r, c;
	while (again) {
		XtAppNextEvent(app_context, &event_return);
		if (event_return.xany.type == ButtonRelease) {
			again = 0;
		} else {
			XtDispatchEvent(&event_return);
		}
	}
	get_coords_cell(w_list, get_top(w_list).row, get_top(w_list).col,
			r1, c1, event->xbutton.x / zoom,
			event->xbutton.y / zoom);
	get_coords_cell(w_list, get_top(w_list).row, get_top(w_list).col,
			r2, c2, event_return.xbutton.x / zoom,
			event_return.xbutton.y / zoom);
	if (*r1 > *r2)
		r = *r1, *r1 = *r2, *r2 = r;
	if (*c1 > *c2)
		c = *c1, *c1 = *c2, *c2 = c;
}

int edit_cell(char *prompt, char *buffr)
{
	XtAppContext app_context = XtWidgetToApplicationContext(topLevel);
	String string, string1;
	Widget edit_text, ew;
	int x, y;
	Dimension w, h, tw;
	buffer *buf = w_list->buf;
	int r = get_point(w_list).row;
	int c = get_point(w_list).col;
	int s = w_list->sht;
	char *p = ret_text(buf, s, r, c);
	MwRichchar *rp;
	int lm;
	Dimension winw;
	int fmt = ret_format(buf, s, r, c);
	int r1, c1, r2, c2;
	char ref[50];
	MwRichchar *rref;

	x = y = 0;

	/* make x the zoomed x coordinate of edit_text */
	MwTableZoomedCellToCoords(w_list->ui->grid, r, c, &x, &y);
	x--;
	y--;

	/* make w (approximately) the unzoomed cell width */
	w = cell_width(buf, s, c) - 1;
	h = zoom * cell_height(buf, s, r) - 1;

	if (!p)
		p = "";

	/* make winw the total zoomed grid width */
	XtVaGetValues(w_list->ui->grid, XtNwidth, &winw, (char *) 0);

	/* make tw the zoomed cell width */
	tw = zoom * w;

	/* if tw is too big, shrink to fit */
	if (tw > winw - x)
		tw = winw - x;

	edit_text = XtVaCreateManagedWidget("edit_text",
					    mwRichtextWidgetClass,
					    w_list->ui->grid, XtNx, x,
					    XtNy, y, XtNwidth, tw,
					    XtNheight, h,
					    XtNrichtextFormat, fmt,
					    (char *) 0);
	MwRichtextSetZoom(edit_text, zoom);

	status = WAITING;

	MwRichtextSetString(edit_text, buffr);
	XtVaGetValues(edit_text,
		      XtNrichtextLeftMargin, &lm,
		      XtNrichtextString, &rp, (char *) 0);
	resize_edit_text(edit_text, winw, x, lm + MwRcStrwidth(rp, -1), h);
	XtVaSetValues(edit_text,
		      XtNrichtextVisibleCursor, True,
		      XtNrichtextPointCol, strlen(buffr), (char *) NULL);

	XtAddGrab(edit_text, False, False);

	XtSetKeyboardFocus(topLevel, edit_text);

	XtVaSetValues(edit_text,
		      XtNrichtextPointCol, strlen(buffr), (char *) 0);

	if (text1 == None) {
		buffr[0] = '\0';
		while (status == WAITING) {
			XEvent event_return;

			string = MwRichtextGetString(edit_text);
			if (strcmp(string, buffr)) {
				XtVaGetValues(edit_text,
					      XtNrichtextLeftMargin, &lm,
					      XtNrichtextString, &rp,
					      (char *) 0);
				resize_edit_text(edit_text, winw,
						 x, lm + MwRcStrwidth(rp,
								      -1),
						 h);
			}
			strcpy(buffr, string);
			XtAppNextEvent(app_context, &event_return);
			XtDispatchEvent(&event_return);
		}
	} else {
		MwRichtextSetString(text1, buffr);
		XtVaSetValues(text1,
			      XtNrichtextPointCol, strlen(buffr),
			      (char *) 0);

		while (status == WAITING) {
			XEvent event_return;
			int n, n1;

			MwLabelSet(label1, prompt);
			string = MwRichtextGetString(edit_text);
			XtVaGetValues(edit_text,
				      XtNrichtextPointCol, &n,
				      (char *) NULL);
			string1 = MwRichtextGetString(text1);
			XtVaGetValues(text1,
				      XtNrichtextPointCol, &n1,
				      (char *) NULL);
			if (strcmp(string, string1)) {
				MwRichtextSetString(text1, string);
				XtVaSetValues(text1,
					      XtNrichtextPointCol, n,
					      (char *) NULL);
				XtVaGetValues(edit_text,
					      XtNrichtextLeftMargin, &lm,
					      XtNrichtextString, &rp,
					      (char *) 0);
				resize_edit_text(edit_text, winw,
						 x, lm + MwRcStrwidth(rp,
								      -1),
						 h);
			}
			XtAppNextEvent(app_context, &event_return);
#if 1				/* reference clicking stuff */
			ew = XtWindowToWidget(XtDisplay(topLevel),
					      event_return.xany.window);
			if (ew == w_list->ui->grid &&
			    event_return.xany.type == ButtonPress) {
				look_for_ref(&event_return,
					     &r1, &c1, &r2, &c2);
				if (w_list->buf->a1_refs) {
					sprintf(ref, "%s%d", a1coord(c1),
						r1);
					if (r1 != r2 || c1 != c2) {
						sprintf(ref + strlen(ref),
							"..%s%d",
							a1coord(c2), r2);
					}
				} else {
					if (r1 == r2 && c1 == c2) {
						sprintf(ref, "R%dC%d", r1,
							c1);
					} else {
						sprintf(ref,
							"R%dC%d..R%dC%d",
							r1, c1, r2, c2);
					}
				}
				XtVaGetValues(edit_text,
					      XtNrichtextPointCol, &n,
					      (char *) 0);
				rref = MwRcMakerich(ref, fmt);
				MwRichtextInsertText(edit_text,
						     rref, strlen(ref));
				XtVaSetValues(edit_text,
					      XtNrichtextPointCol,
					      n + strlen(ref), (char *) 0);
				MwFree(rref);
			} else {
#endif
				XtDispatchEvent(&event_return);
			}
		}
		MwRichtextSetString(text1, "");
		XtVaSetValues(text1,
			      XtNrichtextVisibleCursor, False,
			      (char *) NULL);
	}

	string = MwRichtextGetString(edit_text);

	strcpy(buffr, string);	/* no check on length... */

	XtRemoveGrab(edit_text);
	XtDestroyWidget(edit_text);

	activate_window(w_list);
	return status;
}

/* ---
*/
int add_str_to_input_queue(textbuf buf)
{
	return FALSE;
}

static void set_block(int row1, int col1, int row2, int col2)
{
	int ur = block_upper(w_list).row, uc = block_upper(w_list).col;
	int lr = block_lower(w_list).row, lc = block_lower(w_list).col;

	w_list->bsht = w_list->sht;

	if (row1 < 1)
		row1 = 1;
	if (row1 > BUFFER_ROWS)
		row1 = BUFFER_ROWS;
	if (col1 < 1)
		col1 = 1;
	if (col1 > BUFFER_COLS)
		col1 = BUFFER_COLS;
	if (row2 < 1)
		row2 = 1;
	if (row2 > BUFFER_ROWS)
		row2 = BUFFER_ROWS;
	if (col2 < 1)
		col2 = 1;
	if (col2 > BUFFER_COLS)
		col2 = BUFFER_COLS;

	if (row1 < row2) {
		set_blku_row(w_list, row1);
		set_blkl_row(w_list, row2);
	} else {
		set_blku_row(w_list, row2);
		set_blkl_row(w_list, row1);
	}
	if (col1 < col2) {
		set_blku_col(w_list, col1);
		set_blkl_col(w_list, col2);
	} else {
		set_blku_col(w_list, col2);
		set_blkl_col(w_list, col1);
	}

	/* Redraw if any change */
	if (ur != block_upper(w_list).row || uc != block_upper(w_list).col
	    || lr != block_lower(w_list).row
	    || lc != block_lower(w_list).col) {
		pr_scr_flag = TRUE;
	}

	/* Become selection owner */
	if (XtOwnSelection(w_list->ui->grid, XA_PRIMARY,
			   CurrentTime, convert_proc,
			   lose_ownership_proc, NULL) == False) {
		XtWarning("Siag: failed to become selection owner\n");
		set_blku_row(w_list, -1);
		set_blku_col(w_list, -1);
		set_blkl_row(w_list, -1);
		set_blkl_col(w_list, -1);
	}
#if 0
	if (XtOwnSelection(w_list->ui->grid, clipboard,
			   CurrentTime, convert_proc,
			   lose_ownership_proc, NULL) == False) {
		;		/* don't worry, be happy */
	}
#endif
}

static int colnum_grab(Widget w)
{
	int x = -1;
	int owner_events = True;
	unsigned int event_mask = ButtonReleaseMask | PointerMotionMask;
	int pointer_mode = GrabModeAsync;
	int keyboard_mode = GrabModeAsync;
	Window confine_to = XtWindow(w);
	static Cursor cursor = None;
	Time time = CurrentTime;
	int waiting = True;

	if (cursor == None)
		cursor = XCreateFontCursor(XtDisplay(w), XC_right_side);
	XtGrabPointer(w, owner_events, event_mask,
		      pointer_mode, keyboard_mode, confine_to, cursor,
		      time);
	while (waiting) {
		XEvent event_return;
		XtAppNextEvent(app_context, &event_return);
		if (event_return.type == ButtonRelease) {
			waiting = False;
		} else if (event_return.type == MotionNotify) {
			x = event_return.xmotion.x / zoom;
		} else {
			XtDispatchEvent(&event_return);
		}
	}
	XtUngrabPointer(w, CurrentTime);
	return x;
}

static void ColnumButtonAction(Widget w,
			       XEvent * event, String * params,
			       Cardinal * n)
{
	int col, row, x, y;
	static int col0;
	int s;
	static Time oldtime = 0;

	x = event->xbutton.x / zoom;
	y = 0;
	hide_cur(w_list);
	activate_window(find_window_by_widget(w));
	s = w_list->sht;
	get_coords_cell(w_list, get_top(w_list).row, get_top(w_list).col,
			&row, &col, x, y);
	if (*n < 1 || !strcmp(params[0], "set")) {
		/* If we are within 4 pixels from a cell boundary, grab the
		   pointer and move the border to wherever we release it. */
		int x1, y1, w1;
		Time newtime = event->xbutton.time;
		if (abs((long) newtime - (long) oldtime) <
		    MULTI_CLICK_TIME) {
			execute("(fit-block-width)");
			return;
		}
		oldtime = newtime;
		get_cell_coords(w_list,
				get_top(w_list).row,
				get_top(w_list).col, row, col, &x1, &y1);
		w1 = x - x1;
		if (cell_width(w_list->buf, s, col) - w1 < 5) {
			x = colnum_grab(w);
			w1 = x - x1;
			if (w1 > 5 && w1 < 500)
				set_width(w_list->buf, s, col, w1);
		} else {
			/* Otherwise do everything as before */
			set_point_col(w_list, col);
			set_block(1, col, BUFFER_ROWS, col);
			col0 = col;
		}
		pr_scr_flag = TRUE;

	} else if (!strcmp(params[0], "left"))
		set_blku_col(w_list, col);
	else if (!strcmp(params[0], "right"))
		set_blkl_col(w_list, col);
	else if (!strcmp(params[0], "select")) {
		int r1 = block_upper(w_list).row, c1 =
		    block_upper(w_list).col;
		int r2 = block_lower(w_list).row, c2 =
		    block_lower(w_list).col;
		/* is the selection set to something already? */
		if (r1 == -1 || c1 == -1 || r2 == -1 || c2 == -1)
			set_block(1, col,
				  BUFFER_ROWS, get_point(w_list).col);
		else {
			/* which corner is closer? */
			if (ABS(c1 - col) < ABS(c2 - col))
				set_block(1, col, BUFFER_ROWS, c2);
			else
				set_block(1, c1, BUFFER_ROWS, col);
		}
	} else if (!strcmp(params[0], "adjust")) {
		Dimension wid;

		XtVaGetValues(w, XtNwidth, &wid, (char *) 0);
		if (x < 0) {
			execute("(scroll-cell-left)");
		} else if (x > wid) {
#if 0
			execute("(scroll-cell-right)");
#endif
		}
		set_point_col(w_list, col);
		set_block(1, col0, BUFFER_ROWS, get_point(w_list).col);
	}
	show_cur(w_list);
}

static int rownum_grab(Widget w)
{
	int y = -1;
	int owner_events = True;
	unsigned int event_mask = ButtonReleaseMask | PointerMotionMask;
	int pointer_mode = GrabModeAsync;
	int keyboard_mode = GrabModeAsync;
	Window confine_to = XtWindow(w);
	static Cursor cursor = None;
	Time time = CurrentTime;
	int waiting = True;
	if (cursor == None)
		cursor = XCreateFontCursor(XtDisplay(w), XC_bottom_side);
	XtGrabPointer(w, owner_events, event_mask,
		      pointer_mode, keyboard_mode, confine_to, cursor,
		      time);
	while (waiting) {
		XEvent event_return;
		XtAppNextEvent(app_context, &event_return);
		if (event_return.type == ButtonRelease) {
			waiting = False;
		} else if (event_return.type == MotionNotify) {
			y = event_return.xmotion.y / zoom;
		} else {
			XtDispatchEvent(&event_return);
		}
	}
	XtUngrabPointer(w, CurrentTime);
	return y;
}

/* ---
Multi-click code inspired by Text.c/DoSelection from Xawd3.
*/

static void RownumButtonAction(Widget w,
			       XEvent * event, String * params,
			       Cardinal * n)
{
	int col, row, x, y;
	static int row0;
	static Time oldtime = 0;

	x = 0;
	y = event->xbutton.y / zoom;
	hide_cur(w_list);
	activate_window(find_window_by_widget(w));
	get_coords_cell(w_list, get_top(w_list).row, get_top(w_list).col,
			&row, &col, x, y);
	if (*n < 1 || !strcmp(params[0], "set")) {
		/* If we are within 4 pixels from a cell boundary, grab the
		   pointer and move the border to wherever we release it. */
		int x1, y1, h1;
		Time newtime = event->xbutton.time;
		if (abs((long) newtime - (long) oldtime) <
		    MULTI_CLICK_TIME) {
			execute("(fit-block-height)");
			return;
		}
		oldtime = newtime;
		get_cell_coords(w_list,
				get_top(w_list).row,
				get_top(w_list).col, row, col, &x1, &y1);
		h1 = y - y1;
		if (cell_height(w_list->buf, w_list->sht, row) - h1 < 5) {
			y = rownum_grab(w);
			h1 = y - y1;
			if (h1 > 5 && h1 < 500)
				set_height(w_list->buf, w_list->sht, row,
					   h1);
		} else {
			/* Otherwise do everything as before */
			set_point_row(w_list, row);
			set_block(row, 1, row, BUFFER_COLS);
			row0 = row;
		}
		pr_scr_flag = TRUE;
	} else if (!strcmp(params[0], "top"))
		set_blku_row(w_list, row);
	else if (!strcmp(params[0], "bottom"))
		set_blkl_row(w_list, row);
	else if (!strcmp(params[0], "select")) {
		int r1 = block_upper(w_list).row, c1 =
		    block_upper(w_list).col;
		int r2 = block_lower(w_list).row, c2 =
		    block_lower(w_list).col;
		/* is the selection set to something already? */
		if (r1 == -1 || c1 == -1 || r2 == -1 || c2 == -1)
			set_block(row, 1,
				  get_point(w_list).row, BUFFER_COLS);
		else {
			/* which corner is closer? */
			if (ABS(r1 - row) < ABS(r2 - row))
				set_block(row, 1, r2, BUFFER_COLS);
			else
				set_block(r1, 1, row, BUFFER_COLS);
		}
	} else if (!strcmp(params[0], "adjust")) {
		Dimension hei;

		XtVaGetValues(w, XtNheight, &hei, (char *) 0);
		if (y < 0) {
			execute("(scroll-cell-down)");
		}
		set_point_row(w_list, row);
		set_block(row0, 1, get_point(w_list).row, BUFFER_COLS);
	}
	show_cur(w_list);
}


static void DestroyNotifyAction(Widget w,
				XEvent * event, String * params,
				Cardinal * n)
{
	XDestroyWindowEvent dnevent = event->xdestroywindow;
	fprintf(stderr, "Window %lx was destroyed\n",
		(unsigned long) dnevent.window);
}

static void ExecuteAction(Widget w,
			  XEvent * event, String * params, Cardinal * n)
{
	char b[256];
	int i;

	strcpy(b, "(");
	strncat(b, params[0], 255);
	for (i = 1; i < *n; i++) {
		strncat(b, " ", 255);
		strncat(b, params[i], 255);
	}
	strncat(b, ")", 255);
	exec_expr(siod_interpreter, b);
}

static void KeyEventAction(Widget w,
			   XEvent * event, String * params, Cardinal * n)
{
	int count, bufsiz = 10;
	char buf[12];
	KeySym keysym;
	XKeyEvent kevent;
	kevent = event->xkey;
	count = XLookupString(&kevent, buf, bufsiz, &keysym, NULL);

	lastc = keysym;
	switch (lastc) {
	case XK_KP_Home:
	case XK_Home:
		lastc = CTRL('a');
		break;
	case XK_KP_Left:
	case XK_Left:
		lastc = CTRL('b');
		break;
	case XK_KP_Delete:
	case XK_Delete:
		lastc = CTRL('d');
		break;
	case XK_KP_Up:
	case XK_Up:
		lastc = CTRL('p');
		break;
	case XK_KP_Right:
	case XK_Right:
		lastc = CTRL('f');
		break;
	case XK_KP_Down:
	case XK_Down:
		lastc = CTRL('n');
		break;
	case XK_KP_Page_Up:
	case XK_Page_Up:
		lastc = ALT('v');
		break;
	case XK_KP_Page_Down:
	case XK_Page_Down:
		lastc = CTRL('v');
		break;
	case XK_KP_End:
	case XK_End:
		lastc = CTRL('e');
		break;
	case XK_KP_Tab:
	case XK_Tab:
		lastc = '\t';
		break;
	case XK_BackSpace:
		lastc = '\b';
		break;
	case XK_KP_Enter:
	case XK_Linefeed:
		lastc = '\n';
		break;
	case XK_Clear:
		lastc = '\f';
		break;
	case XK_Return:
		lastc = '\r';
		break;
	case XK_Escape:
		lastc = CTRL('[');
		break;
	case XK_KP_Space:
		lastc = ' ';
		break;
	case XK_KP_Equal:
		lastc = '=';
		break;
	case XK_KP_Multiply:
		lastc = '*';
		break;
	case XK_KP_Add:
		lastc = '+';
		break;
	case XK_KP_Separator:
		lastc = ',';
		break;
	case XK_KP_Subtract:
		lastc = '-';
		break;
	case XK_KP_Decimal:
		lastc = '.';
		break;
	case XK_KP_Divide:
		lastc = '/';
		break;
	case XK_KP_0:
		lastc = '0';
		break;
	case XK_KP_1:
		lastc = '1';
		break;
	case XK_KP_2:
		lastc = '2';
		break;
	case XK_KP_3:
		lastc = '3';
		break;
	case XK_KP_4:
		lastc = '4';
		break;
	case XK_KP_5:
		lastc = '5';
		break;
	case XK_KP_6:
		lastc = '6';
		break;
	case XK_KP_7:
		lastc = '7';
		break;
	case XK_KP_8:
		lastc = '8';
		break;
	case XK_KP_9:
		lastc = '9';
		break;

	default:
		if (lastc > 255)
			return;
	}
#if 0				/* fishy */
	if (count)
		drop_selection();
#endif
	if (kevent.state & CONTROL_MASK)
		lastc = CTRL(lastc);
	if (kevent.state & ALT_MASK)
		lastc = ALT(lastc);
	do_cmd(lastc);
}

static void extend_left(Widget w, XEvent * event, String * params,
			Cardinal * n)
{
	start_selection();
	execute("(backward_cell)");
	set_selection();
}

static void extend_right(Widget w, XEvent * event, String * params,
			 Cardinal * n)
{
	start_selection();
	execute("(forward_cell)");
	set_selection();
}

static void extend_up(Widget w, XEvent * event, String * params,
		      Cardinal * n)
{
	start_selection();
	execute("(previous_line)");
	set_selection();
}

static void extend_down(Widget w, XEvent * event, String * params,
			Cardinal * n)
{
	start_selection();
	execute("(next_line)");
	set_selection();
}

static void backward_cell(Widget w, XEvent * event, String * params,
			  Cardinal * n)
{
	drop_selection();
	execute("(backward_cell)");
}

static void forward_cell(Widget w, XEvent * event, String * params,
			 Cardinal * n)
{
	drop_selection();
	execute("(forward_cell)");
}

static void previous_line(Widget w, XEvent * event, String * params,
			  Cardinal * n)
{
	drop_selection();
	execute("(previous_line)");
}

static void next_line(Widget w, XEvent * event, String * params,
		      Cardinal * n)
{
	drop_selection();
	execute("(next_line)");
}

static void cut_to_clipboard(Widget w, XEvent * event,
			     String * params, Cardinal * n)
{
	if (!we_have_selection) {
		return;
	}
	start_clipboard();
	execute("(delete-block)");
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static void copy_to_clipboard(Widget w, XEvent * event,
			      String * params, Cardinal * n)
{
	if (!we_have_selection) {
		return;
	}
	start_clipboard();
}

static void paste_from_clipboard(Widget w, XEvent * event,
				 String * params, Cardinal * n)
{
	XtGetSelectionValue(w, clipboard, target_atom,
			    requestor_callback, event, CurrentTime);
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static LISP lcut_to_clipboard(void)
{
	cut_to_clipboard(w_list->ui->grid, NULL, NULL, NULL);
	return NIL;
}

static LISP lcopy_to_clipboard(void)
{
	copy_to_clipboard(w_list->ui->grid, NULL, NULL, NULL);
	return NIL;
}

static LISP lpaste_from_clipboard(void)
{
	paste_from_clipboard(w_list->ui->grid, NULL, NULL, NULL);
	return NIL;
}

#define SCROLL_INTERVAL 200

static XtIntervalId scroll_timeout = None;

static void scroll_oneline(XtPointer client_data, XtIntervalId * id)
{
	Widget w = (Widget) client_data;
	XEvent event;
	scroll_timeout = None;
	event.xclient.type = ClientMessage;
	event.xclient.format = 8;
	event.xclient.window = XtWindow(w);

	XSendEvent(XtDisplay(w), XtWindow(w), True, NoEventMask, &event);
}

static void grid_grab(Widget w)
{
	int x, y, row, col, startrow, startcol, oldrow, oldcol;
	int owner_events = False;
	unsigned int event_mask = ButtonReleaseMask | PointerMotionMask |
	    EnterWindowMask | LeaveWindowMask;
	int pointer_mode = GrabModeAsync;
	int keyboard_mode = GrabModeAsync;
	static Cursor cursor = None;
	Time time = CurrentTime;
	enum { START = 0, OUTSIDE, STOP } state = START;
	if (cursor == None)
		cursor =
		    XCreateFontCursor(XtDisplay(w),
				      XC_dot /*XC_sizing */ );
	XtGrabPointer(w, owner_events, event_mask, pointer_mode,
		      keyboard_mode, None, cursor, time);
	startrow = oldrow = get_point(w_list).row;
	startcol = oldcol = get_point(w_list).col;
	while (state != STOP) {
		XEvent event_return;
		XtAppNextEvent(app_context, &event_return);
		switch (state) {
		case START:
			if (event_return.type == ButtonRelease) {
				state = STOP;
			} else if (event_return.type == MotionNotify) {
				/* figure out where we are and set selection */
				x = event_return.xmotion.x / zoom;
				y = event_return.xmotion.y / zoom;
				get_coords_cell(w_list,
						get_top(w_list).row,
						get_top(w_list).col, &row,
						&col, x, y);
				set_point_row(w_list, row);
				set_point_col(w_list, col);
				set_block(startrow, startcol, row, col);
				w_list->bsht = w_list->sht;
				show_cur(w_list);
			} else if (event_return.type == LeaveNotify) {
				state = OUTSIDE;
				scroll_timeout =
				    XtAppAddTimeOut
				    (XtWidgetToApplicationContext(w),
				     SCROLL_INTERVAL, scroll_oneline,
				     (XtPointer) w);
			} else {
				XtDispatchEvent(&event_return);
			}
			break;
		case OUTSIDE:
			if (event_return.type == ButtonRelease) {
				state = STOP;
			} else if (event_return.type == EnterNotify) {
				state = START;
			} else if (event_return.type == ClientMessage &&
				   scroll_timeout == None) {
				Window root, child;
				int rx, ry;
				unsigned int keys_buttons;
				Dimension width, height;
				XtVaGetValues(w,
					      XtNwidth, &width,
					      XtNheight, &height,
					      (char *) 0);
				XQueryPointer(XtDisplay(w), XtWindow(w),
					      &root, &child, &rx, &ry, &x,
					      &y, &keys_buttons);
				if (x < 0)
					col--;
				if (x > width)
					col++;
				if (y < 0)
					row--;
				if (y > height)
					row++;
				if (row < 1)
					row = 1;
				if (row > BUFFER_ROWS)
					row = BUFFER_ROWS;
				if (col < 1)
					col = 1;
				if (col > BUFFER_COLS)
					col = BUFFER_COLS;
				set_point_row(w_list, row);
				set_point_col(w_list, col);
				set_block(startrow, startcol, row, col);
				show_cur(w_list);
				scroll_timeout =
				    XtAppAddTimeOut
				    (XtWidgetToApplicationContext(w),
				     SCROLL_INTERVAL, scroll_oneline,
				     (XtPointer) w);
			} else {
				XtDispatchEvent(&event_return);
			}
			break;
		default:
			fprintf(stderr,
				"grid_grab: You shouldn't be here\n");
		}
	}
	if (scroll_timeout != None)
		XtRemoveTimeOut(scroll_timeout);
	scroll_timeout = None;
	XtUngrabPointer(w, CurrentTime);
}

static void autofill_grab(Widget w, int *rx, int *ry)
{
	int owner_events = True;
	unsigned int event_mask = ButtonReleaseMask | PointerMotionMask;
	int pointer_mode = GrabModeAsync;
	int keyboard_mode = GrabModeAsync;
	Window confine_to = XtWindow(w);
	static Cursor cursor = None;
	Time time = CurrentTime;
	int waiting = True;

	*rx = *ry = -1;
	XtGrabPointer(w, owner_events, event_mask,
		      pointer_mode, keyboard_mode, confine_to, cursor,
		      time);
	while (waiting) {
		XEvent event_return;
		XtAppNextEvent(app_context, &event_return);
		if (event_return.type == ButtonRelease) {
			waiting = False;
		} else if (event_return.type == MotionNotify) {
			*rx = event_return.xmotion.x / zoom;
			*ry = event_return.xmotion.y / zoom;
		} else {
			XtDispatchEvent(&event_return);
		}
	}
	XtUngrabPointer(w, CurrentTime);
}

static void GridButtonAction(Widget w,
			     XEvent * event, String * params, Cardinal * n)
{
	int col, row;
	int x, y;

	x = event->xbutton.x / zoom;
	y = event->xbutton.y / zoom;
	hide_cur(w_list);
	activate_window(find_window_by_widget(w));
	get_coords_cell(w_list, get_top(w_list).row, get_top(w_list).col,
			&row, &col, x, y);
	if (*n < 1 || !strcmp(params[0], "point")) {
		/* If we are within 4 pixels from a cell boundary, grab the
		   pointer and move the border to wherever we release it. */
		int x1, y1, h1, w1, hd, wd;
		get_cell_coords(w_list, get_top(w_list).row,
				get_top(w_list).col, row, col, &x1, &y1);
		h1 = y - y1;
		w1 = x - x1;
		hd = cell_height(w_list->buf, w_list->sht, row) - h1;
		wd = cell_width(w_list->buf, w_list->sht, col) - w1;
		if (hd < 5 && wd < 5) {
			int rr, rc, rx, ry;
			autofill_grab(w, &rx, &ry);
			get_coords_cell(w_list,
					get_top(w_list).row,
					get_top(w_list).col, &rr, &rc, rx,
					ry);
			fill_area(w_list->buf, w_list->sht, row, col, rr,
				  rc);
			pr_scr_flag = TRUE;
		} else if (hd < 5) {
			y = rownum_grab(w);
			h1 = y - y1;
			if (h1 > 5 && h1 < 500)
				set_height(w_list->buf, w_list->sht, row,
					   h1);
			pr_scr_flag = TRUE;
		} else if (wd < 5) {
			x = colnum_grab(w);
			w1 = x - x1;
			if (w1 > 5 && w1 < 500)
				set_width(w_list->buf, w_list->sht, col,
					  w1);
			pr_scr_flag = TRUE;
		} else {
			/* Otherwise do everything as before */
			set_point_row(w_list, row);
			set_point_col(w_list, col);
			grid_grab(w);
		}
	} else if (!strcmp(params[0], "mark")) {
		set_mark_row(w_list, row);
		set_mark_col(w_list, col);
	} else if (!strcmp(params[0], "block")) {
		set_block(row, col,
			  get_mark(w_list).row, get_mark(w_list).col);
		pr_scr_flag = TRUE;
	} else if (!strcmp(params[0], "paste")) {
		set_point_row(w_list, row);
		set_point_col(w_list, col);
		XtGetSelectionValue(w, XA_PRIMARY, target_atom,
				    requestor_callback, event,
				    event->xbutton.time);
		pr_scr_flag = TRUE;
	} else if (!strcmp(params[0], "select")) {
		int r1 = block_upper(w_list).row, c1 =
		    block_upper(w_list).col;
		int r2 = block_lower(w_list).row, c2 =
		    block_lower(w_list).col;
		/* is the selection set to something already? */
		if (r1 == -1 || c1 == -1 || r2 == -1 || c2 == -1)
			set_block(row, col,
				  get_point(w_list).row,
				  get_point(w_list).col);
		else {
			/* which corner is closer? */
			if (ABS(r1 - row) < ABS(r2 - row))
				r1 = r2;
			if (ABS(c1 - col) < ABS(c2 - col))
				c1 = c2;
			set_block(r1, c1, row, col);
		}
	}
	show_cur(w_list);
}

static LISP ltooltip_mode(LISP newmode)
{
	int mode = get_c_long(newmode);
	XtVaSetValues(tooltip,
		      XtNtooltipMode, mode,
		      XtNtooltipLabel, label2, (char *) 0);
	return NIL;
}

static void siaghelp_action(Widget w, XEvent * event,
			    String * params, Cardinal * num_params)
{
	char b[1256];

	sprintf(b, "siagrun help file:%s/siag/%s", docdir, params[0]);
}

static void execute_callback(Widget w,
			     XtPointer client_data, XtPointer call_data)
{
	execute((char *) client_data);
}

static void tabs_left(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget tw = (Widget) client_data;
	int n;

	XtVaGetValues(tw, XtNtabbingTop, &n, (char *) 0);
	XtVaSetValues(tw, XtNtabbingTop, n - 1, (char *) 0);
}

static void tabs_right(Widget w, XtPointer client_data,
		       XtPointer call_data)
{
	Widget tw = (Widget) client_data;
	int n;

	XtVaGetValues(tw, XtNtabbingTop, &n, (char *) 0);
	XtVaSetValues(tw, XtNtabbingTop, n + 1, (char *) 0);
}

static void vscroll_jump(Widget w, XtPointer client_data,
			 XtPointer call_data)
{
	float top;
	int gridtop;
	position p;

	hide_cur(w_list);
	w_list = find_window_by_widget(w);
	XtVaGetValues(w, XtNtopOfThumb, &top, (char *) NULL);
	gridtop =
	    top * (w_list->buf->sht[w_list->sht].alloc_lines + 50) + 1;
	if (gridtop < 1)
		gridtop = 1;
	if (gridtop == get_top(w_list).row)
		return;
	/* make sure protection is respected */
	/* is it necessary to move point here? */
	p.row = gridtop;
#if 1
	p.col = get_point(w_list).col;
	set_point(w_list, p);
#endif
	p.col = get_top(w_list).col;
	set_top(w_list, p);
	activate_window(w_list);
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static void vscroll_scroll(Widget w, XtPointer client_data,
			   XtPointer call_data)
{
	int i = (int) call_data;
	Dimension length;

	if (i == 0)
		return;		/* no div by zero */
	activate_window(find_window_by_widget(w));
	XtVaGetValues(w, XtNlength, &length, (char *) NULL);
	if (i < 0) {
		if ((length / -i) > 15)
			execute("(scroll-cell-down)");
		else
			execute("(scroll-down)");
	} else {
		if ((length / i) > 15)
			execute("(scroll-cell-up)");
		else
			execute("(scroll-up)");
	}
}

static void hscroll_jump(Widget w, XtPointer client_data,
			 XtPointer call_data)
{
	float top;
	int gridtop;
	position p;

	hide_cur(w_list);
	w_list = find_window_by_widget(w);
	XtVaGetValues(w, XtNtopOfThumb, &top, (char *) NULL);
	gridtop =
	    top * (w_list->buf->sht[w_list->sht].longest_line + 50) + 1;
	if (gridtop < 1)
		gridtop = 1;
	if (gridtop == get_top(w_list).col)
		return;
	p.col = gridtop;
#if 1
	p.row = get_point(w_list).row;
	set_point(w_list, p);
#endif
	p.row = get_top(w_list).row;
	set_top(w_list, p);
	activate_window(w_list);
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static void hscroll_scroll(Widget w, XtPointer client_data,
			   XtPointer call_data)
{
	int i = (int) call_data;
	Dimension length;

	if (i == 0)
		return;		/* no div by zero */
	activate_window(find_window_by_widget(w));
	XtVaGetValues(w, XtNlength, &length, (char *) NULL);
	if (i < 0) {
		if ((length / -i) > 15)
			execute("(scroll-cell-left)");
		else
			execute("(scroll-left)");
	} else {
		if ((length / i) > 15)
			execute("(scroll-cell-right)");
		else
			execute("(scroll-right)");
	}
}

static struct {
	char *label;
	Widget button, menu;
} *menubar;

static int menucount = 0;

static LISP add_menu(LISP lisp_label)
{
	char *label;
	char button_name[80];

	if (topbox == None)
		return NIL;

	label = MwStrdup(get_c_string(lisp_label));

	sprintf(button_name, "btn%s", label);
	menubar = MwRealloc(menubar, (menucount + 1) * (sizeof *menubar));
	menubar[menucount].label = label;
	menubar[menucount].button = XtVaCreateManagedWidget(button_name,
							    mwMBButtonObjectClass,
							    menubox,
							    XtNmenu_name,
							    label,
							    XtNlabel,
							    _(label),
#if 0
							    XtNgravitation,
							    (strcmp
							     (label,
							      "Help") ?
							     XtCleft :
							     XtCright),
#endif
							    (char *) 0);
	menubar[menucount].menu = XtVaCreatePopupShell(label,
						       mwMenuWidgetClass,
						       menubox,
						       (char *) 0);
	menucount++;

	return NIL;
}

static void remake_ylayout(void)
{
	char b[100];
	sprintf(b, "%s %s %s 30 100%% 30",
		(bars & MENUBAR) ? "30" : "0",
		(bars & TOOLBAR) ? "30" : "0",
		(bars & FORMATBAR) ? "30" : "0");
	XtVaSetValues(topbox, XtNyLayout, b, (char *) 0);
}

static void attach_bar(Widget w, XtPointer client_data,
		       XtPointer call_data)
{
	Widget vw = (Widget) call_data;
	if (vw == frame1)
		bars |= MENUBAR;
	if (vw == frame2)
		bars |= TOOLBAR;
	if (vw == frame3)
		bars |= FORMATBAR;
	remake_ylayout();
}

static void detach_bar(Widget w, XtPointer client_data,
		       XtPointer call_data)
{
	Widget vw = (Widget) call_data;
	if (vw == frame1)
		bars &= ~MENUBAR;
	if (vw == frame2)
		bars &= ~TOOLBAR;
	if (vw == frame3)
		bars &= ~FORMATBAR;
	remake_ylayout();
}


static void init_menu(void)
{
	bars |= MENUBAR;
	frame1 = XtVaCreateManagedWidget("frame1",
					 mwRudegridWidgetClass, topbox,
					 XtNborderWidth, 0,
					 XtNxLayout, "9 100%", (char *) 0);
	MwMakeHandle(frame1, frame1, detach_bar, attach_bar);
	menubox = XtVaCreateManagedWidget("menubox",
					  mwMenuBarWidgetClass, frame1,
					  XtNgridx, 1, (char *) 0);
}

static Widget find_menu_by_name(char *label)
{
	int i;

	if (!MwStrcasecmp("Shortcuts", label))
		return shortcuts;

	for (i = 0; i < menucount; i++) {
		if (!MwStrcasecmp(menubar[i].label, label))
			return menubar[i].menu;
	}
	return NULL;
}

static LISP add_menu_entry(LISP menu, LISP label, LISP function)
{
	Widget entry, menuw;
	char *lbl = MwStrdup(get_c_string(label));

	menuw = find_menu_by_name(get_c_string(menu));
	if (!menuw) {
		return NIL;
	}

	if (!strcmp(lbl, "-")) {	/* line pane */
		entry = XtVaCreateManagedWidget("-",
						mwLineMEObjectClass, menuw,
						(char *) 0);
	} else {
		entry = XtVaCreateManagedWidget(get_c_string(function),
						mwLabelMEObjectClass,
						menuw, XtNlabel, _(lbl),
						(char *) 0);
		XtAddCallback(entry, XtNcallback, execute_callback,
			      MwStrdup(get_c_string(function)));
	}
	return NIL;
}

static struct {
	char *label;
	char *sublabel;
	Widget entry, menu;
} *submenus;

static int submenucount = 0;

static Widget find_submenu_by_name(char *label, char *sublabel)
{
	int i;

	for (i = 0; i < submenucount; i++) {
		if (!MwStrcasecmp(submenus[i].label, label) &&
		    !MwStrcasecmp(submenus[i].sublabel, sublabel))
			return submenus[i].menu;
	}
	return None;
}

static LISP add_submenu(LISP label, LISP sublabel)
{
	Widget menuw;
	char button_name[80];
	char *sublbl = MwStrdup(get_c_string(sublabel));

	menuw = find_menu_by_name(get_c_string(label));

	if (!menuw) {
		return NIL;
	}
	sprintf(button_name, "btn%s", sublbl);
	submenus =
	    MwRealloc(submenus, (submenucount + 1) * (sizeof *submenus));
	submenus[submenucount].label = MwStrdup(get_c_string(label));
	submenus[submenucount].sublabel = sublbl;
	submenus[submenucount].entry =
	    XtVaCreateManagedWidget(button_name,
				    mwSubMEObjectClass, menuw,
				    XtNmenu_name, sublbl,
				    XtNlabel, _(sublbl), (char *) 0);
	submenus[submenucount].menu = XtVaCreatePopupShell(sublbl,
							   mwMenuWidgetClass,
							   menuw,
							   (char *) 0);
	submenucount++;
	return NIL;
}

static LISP add_submenu_entry(LISP menu, LISP submenu,
			      LISP label, LISP function)
{
	Widget entry, menuw;
	char *lbl = MwStrdup(get_c_string(label));

	menuw = find_submenu_by_name(get_c_string(menu),
				     get_c_string(submenu));
	if (!menuw) {
		return NIL;
	}

	if (!strcmp(lbl, "-")) {	/* line pane */
		entry = XtVaCreateManagedWidget("-",
						mwLineMEObjectClass, menuw,
						(char *) 0);
	} else {
		entry = XtVaCreateManagedWidget(get_c_string(function),
						mwLabelMEObjectClass,
						menuw, XtNlabel, _(lbl),
						(char *) 0);
		XtAddCallback(entry, XtNcallback, execute_callback,
			      MwStrdup(get_c_string(function)));
	}
	return NIL;
}

static Widget make_toggle(char *cmd, Widget pw, char *pm, char *t)
{
	Widget w;
	Pixmap pm_return;
	Pixel color;

	XtVaGetValues(pw, XtNbackground, &color, (char *) 0);

	w = XtVaCreateManagedWidget("toolbar_toggle",
				    toggleWidgetClass, pw, (char *) NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);
	XtVaSetValues(w,
		      XtNbitmap, pm_return,
		      XtNforeground, color, (char *) 0);
	XtAddCallback(w, XtNcallback, execute_callback, (XtPointer) cmd);
	MwTooltipAdd(tooltip, w, _(t));
	return w;
}

static Widget make_command(char *cmd, Widget pw, char *pm, char *t)
{
	Widget w;
	Pixmap pm_return;
	Pixel color;

	XtVaGetValues(pw, XtNbackground, &color, (char *) 0);
	w = XtVaCreateManagedWidget("toolbar_command",
				    commandWidgetClass, pw,
				    XtNforeground, color, (char *) NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);
	XtVaSetValues(w, XtNbitmap, pm_return, (char *) 0);

	XtAddCallback(w, XtNcallback, execute_callback, (XtPointer) cmd);
	MwTooltipAdd(tooltip, w, _(t));
	return w;
}

static void make_vsep(Widget pw)
{
	unsigned long bg;
	XtVaGetValues(topbox, XtNbackground, &bg, (char *) 0);
	XtVaCreateManagedWidget("vsep",
				labelWidgetClass, pw,
				XtNborderColor, bg, (char *) 0);
}

/* The toolbar */

static void init_toolbar(void)
{
	bars |= TOOLBAR;
	frame2 = XtVaCreateManagedWidget("toolbar",
					 mwRudegridWidgetClass, topbox,
					 XtNborderWidth, 0,
					 XtNxLayout, "9 100%", (char *) 0);
	MwMakeHandle(frame2, frame2, detach_bar, attach_bar);
	toolbox = XtVaCreateManagedWidget("frame2",
					  mwFrameWidgetClass, frame2,
					  XtNgridx, 1,
					  XtNshadowWidth, 1, (char *) 0);
	toolbox = XtVaCreateManagedWidget("toolbox",
					  boxWidgetClass, toolbox,
					  (char *) 0);
	bars |= FORMATBAR;
	frame3 = XtVaCreateManagedWidget("formatbar",
					 mwRudegridWidgetClass, topbox,
					 XtNborderWidth, 0,
					 XtNxLayout, "9 100%", (char *) 0);
	MwMakeHandle(frame3, frame3, detach_bar, attach_bar);
	formatbox = XtVaCreateManagedWidget("frame3",
					    mwFrameWidgetClass, frame3,
					    XtNgridx, 1,
					    XtNshadowWidth, 1, (char *) 0);
	formatbox = XtVaCreateManagedWidget("formatbox",
					    boxWidgetClass, formatbox,
					    (char *) 0);

	make_command("(new-buffer)", toolbox, "new.xpm",
		     "Start another instance of Siag");

	make_command("(load-buffer)", toolbox, "fld_open.xpm",
		     "Open a Siag document");
	make_command("(save-buffer-as)", toolbox, "save.xpm",
		     "Save the contents of the current buffer");
	make_command("(preview)", toolbox, "preview.xpm",
		     "Preview the contents of the current buffer");
	make_command("(print)", toolbox, "printer.xpm",
		     "Print the contents of the current buffer");
	make_vsep(toolbox);
	make_command("(cut_to_clipboard)", toolbox, "cut.xpm", "Cut");
	make_command("(copy_to_clipboard)", toolbox, "copy.xpm", "Copy");
	make_command("(paste_from_clipboard)", toolbox, "paste.xpm",
		     "Paste");
	make_command("(undo-restore)", toolbox, "undo.xpm", "Undo");
	make_command("(sort-block 0 nil nil nil)", toolbox, "sortaz.xpm",
		     "Sort ascending");
	make_command("(sort-block 0 nil t nil)", toolbox, "sortza.xpm",
		     "Sort descending");
	make_command("(plot-wizard)", toolbox, "plotter.xpm",
		     "Plot the contents of the block using lines");
	make_command("(block-sum)", toolbox, "sigma.xpm",
		     "Add the contents of the block");
	make_vsep(toolbox);
	make_command("(help-contents)", toolbox, "info.xpm",
		     "Display the Siag online documentation");
	make_command("(help-copyright)", toolbox, "copyright.xpm",
		     "Display the Gnu general public license");
}

static void init_toggle(void)
{
	cmdBold = make_toggle("(toggle-format \"bold\")",
			      formatbox, "bold.xpm", "Bold text");
	cmdItalic = make_toggle("(toggle-format \"italic\")",
				formatbox, "italic.xpm", "Italic text");
	cmdUline = make_toggle("(toggle-format \"uline\")",
			       formatbox, "uchar.xpm", "Underlined text");
	make_vsep(formatbox);
	cmdHLeft = make_toggle("(new-format \"hadj\" HADJ_LEFT)",
			       formatbox, "hleft.xpm",
			       "Left adjusted text");
	cmdHCenter =
	    make_toggle("(new-format \"hadj\" HADJ_CENTER)", formatbox,
			"hcenter.xpm", "Centered text");
	cmdHRight =
	    make_toggle("(new-format \"hadj\" HADJ_RIGHT)", formatbox,
			"hright.xpm", "Right adjusted text");
	make_vsep(toolbox);
	cmdBorders = make_command("(block-borders 1)", toolbox,
				  "borders.xpm",
				  "Draw borders around the block");
	cmdGrid =
	    make_command("(block-borders 2)", toolbox, "grid.xpm",
			 "Draw grid lines in the block");
	cmdUline =
	    make_command("(block-borders 3)", toolbox, "uline.xpm",
			 "Underline the block");
	cmdLline =
	    make_command("(block-borders 5)", toolbox, "lline.xpm",
			 "Leftline the block");
	cmdRline =
	    make_command("(block-borders 6)", toolbox, "rline.xpm",
			 "Rightline the block");
	cmdNone =
	    make_command("(block-borders 0)", toolbox, "none.xpm",
			 "Remove grid lines from the block");
}


static char *combo_sizes[] = {
	"8", "9", "10", "11", "12", "14", "16", "18",
	"20", "22", "24", "26", "28", "36", "48", "72"
};

static char **combo_fonts, **combo_colors, **combo_styles;
static int ncombo_fonts, ncombo_colors, ncombo_styles;

static void cb_font(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *) call_data;
	va_execute("(new-format \"family\" \"%s\")", p);
	activate_window(w_list);
}

static void cb_size(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *) call_data;
	va_execute("(new-format \"size\" (* 10 %s))", p);
	activate_window(w_list);
}

static void cb_style(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *) call_data;
	va_execute("(new-format \"style\" \"%s\")", p);
	activate_window(w_list);
}

static void cb_color(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *) call_data;
	va_execute("(new-format \"fg\" \"%s\")", p);
	activate_window(w_list);
}

static void setup_buttons(void)
{
	unsigned long bg;

	if (topbox == None)
		return;

	XtVaGetValues(topbox, XtNbackground, &bg, (char *) 0);
	/* sizes set both for frames and combos, to test maximizing frame */
	combo_fonts = MwFontList(&ncombo_fonts);
	btnFont = XtVaCreateManagedWidget("format_command",
					  mwComboWidgetClass, formatbox,
					  XtNwidth, 160,
					  XtNborderColor, bg,
					  XtNlabel, "Font",
					  XtNcomboTop, topLevel,
					  XtNcomboData, combo_fonts,
					  XtNcomboNData, ncombo_fonts,
					  (char *) NULL);
	XtAddCallback(btnFont, XtNlistCallback, cb_font, NULL);
	XtAddCallback(btnFont, XtNtextCallback, cb_font, NULL);
	MwTooltipAdd(tooltip, btnFont, _("Change the font family"));
	btnSize = XtVaCreateManagedWidget("format_command",
					  mwComboWidgetClass, formatbox,
					  XtNwidth, 40,
					  XtNborderColor, bg,
					  XtNlabel, "Size",
					  XtNcomboTop, topLevel,
					  XtNcomboData, combo_sizes,
					  XtNcomboNData,
					  XtNumber(combo_sizes),
					  (char *) NULL);
	XtAddCallback(btnSize, XtNlistCallback, cb_size, NULL);
	XtAddCallback(btnSize, XtNtextCallback, cb_size, NULL);
	MwTooltipAdd(tooltip, btnSize, _("Change the font size"));
	combo_styles = style_list(&ncombo_styles);
	btnStyle = XtVaCreateManagedWidget("format_command",
					   mwComboWidgetClass, formatbox,
					   XtNwidth, 80,
					   XtNborderColor, bg,
					   XtNlabel, "Style",
					   XtNcomboTop, topLevel,
					   XtNcomboData, combo_styles,
					   XtNcomboNData, ncombo_styles,
					   (char *) NULL);
	XtAddCallback(btnStyle, XtNlistCallback, cb_style, NULL);
	XtAddCallback(btnStyle, XtNtextCallback, cb_style, NULL);
	MwTooltipAdd(tooltip, btnStyle, _("Change the display style"));
	combo_colors = MwColorList(&ncombo_colors);
	btnColor = XtVaCreateManagedWidget("format_command",
					   mwComboWidgetClass, formatbox,
					   XtNwidth, 80,
					   XtNborderColor, bg,
					   XtNlabel, "Color",
					   XtNcomboTop, topLevel,
					   XtNcomboData, combo_colors,
					   XtNcomboNData, ncombo_colors,
					   (char *) NULL);
	XtAddCallback(btnColor, XtNlistCallback, cb_color, NULL);
	XtAddCallback(btnColor, XtNtextCallback, cb_color, NULL);
	MwTooltipAdd(tooltip, btnColor, _("Change the color"));
}


static void place_shortcuts(Widget w, XEvent * event,
			    String * p, Cardinal * n)
{
	XButtonEvent *bev = (XButtonEvent *) event;
	int x, y;

	activate_window(find_window_by_widget(w));
	x = event->xbutton.x;
	y = event->xbutton.y;
	XtVaSetValues(shortcuts,
		      XtNx, bev->x_root, XtNy, bev->y_root, (char *) 0);
}


static void popup_shortcuts(Widget w, XEvent * event,
			    String * p, Cardinal * n)
{
#if 1
	XButtonEvent *bev = (XButtonEvent *) event;

	activate_window(find_window_by_widget(w));
#endif
	if (!XtIsRealized(shortcuts))
		XtRealizeWidget(shortcuts);
#if 1
	XtVaSetValues(shortcuts,
		      XtNx, bev->x_root, XtNy, bev->y_root, (char *) 0);
#endif
	XtPopupSpringLoaded(shortcuts);
}

static XtActionsRec actions[] = {
	{"destroy-notify", DestroyNotifyAction},
	{"key-event", KeyEventAction},
	{"extend_left", extend_left},
	{"extend_right", extend_right},
	{"extend_up", extend_up},
	{"extend_down", extend_down},
	{"backward_cell", backward_cell},
	{"forward_cell", forward_cell},
	{"previous_line", previous_line},
	{"next_line", next_line},
	{"cut_to_clipboard", cut_to_clipboard},
	{"copy_to_clipboard", copy_to_clipboard},
	{"paste_from_clipboard", paste_from_clipboard}, {"grid-button",
							 GridButtonAction},
	    {"colnum-button", ColnumButtonAction},
	{"rownum-button", RownumButtonAction}, {"siaghelp",
						siaghelp_action},
	    {"place-shortcuts", place_shortcuts},
	{"popup-shortcuts", popup_shortcuts}, {"execute", ExecuteAction},
};

static void cb_drag(Widget w, XtPointer client_data, XtPointer call_data)
{
	int x, y;
	Window root, child;
	DropPosition *where = (DropPosition *) call_data;
	printf("cb_drag at (%d,%d)\n", where->x, where->y);
	root = DefaultRootWindow(XtDisplay(w));
	XTranslateCoordinates(XtDisplay(w),
			      root, XtWindow(w_list->ui->grid),
			      where->x, where->y, &x, &y, &child);
	printf("relative to grid: %d,%d\n", x, y);
	printf("grid is %ld, child is %ld\n", XtWindow(w_list->ui->grid),
	       child);
}

static void cb_drop(Widget w, XtPointer client_data, XtPointer call_data)
{
	int prot = (int) call_data;
	int dnd_type;
	Atom xdnd_type;
	char *data, *fn, cmd[2000];

	switch (prot) {
	case DndDrop:
		printf("cb_drop received OffiX drop\n");
		XtVaGetValues(w,
			      XtNdndType, &dnd_type,
			      XtNdropData, &data, (char *) 0);
		switch (dnd_type) {
		case DndRawData:
			printf("Don't know what to do with raw data.\n");
			break;
		case DndFile:
			fn = data;
			sprintf(cmd, "siag %s", fn);
			MwSpawn(cmd);
			break;
		case DndFiles:
			fn = data;
			while (fn[0]) {
				sprintf(cmd, "siag %s", fn);
				MwSpawn(cmd);
				fn += strlen(fn) + 1;
			}
			break;
		case DndText:
			printf("Should insert '%s' at point.\n", data);
			break;
		case DndDir:
			fn = data;
			if (!fork()) {
				close(2);
				chdir(fn);
				execlp("siag", "Siag", (char *) 0);
				exit(1);
			}
			break;
		case DndLink:
			printf("Got a link to '%s'\n", data);
			break;
		case DndExe:
			MwSpawn(data);
			break;
		case DndURL:
			printf("Got a URL to '%s'\n", data);
			break;
		case DndMIME:
			printf("Got mime type '%s'\n", data);
			break;
		default:
			printf("Don't know how to handle type %d\n",
			       dnd_type);
		}
		break;
	case XdndDrop:
		printf("cb_drop received Xdnd drop\n");
		XtVaGetValues(w,
			      XtNxdndType, &xdnd_type,
			      XtNdropData, &data, (char *) 0);
		if (xdnd_type == drop_types[0]) {
			/* text/plain */
			printf("Should insert '%s' at point.\n", data);
		} else if (xdnd_type == drop_types[1]) {
			/* text/uri-list */
			printf("Should load the file '%s'.\n", data);
		} else {
			printf("Unknown drop type '%s'.\n",
			       XGetAtomName(XtDisplay(w), xdnd_type));
		}
		break;
	case MotifDrop:
		printf("cb_drop received Motif drop\n");
		break;
	default:
		printf("cb_drop received unknown drop\n");
		break;
	}
}

static void init_windows1(int *argc, char **argv)
{
	XtSetLanguageProc(NULL, (XtLanguageProc) NULL, NULL);

	topLevel = XtVaOpenApplication(&app_context,	/* application context */
				       "Siag",	/* application class */
				       options,	/* command line options list */
				       XtNumber(options), argc, argv,	/* command line args */
				       fallback_resources,	/* for missing app-defaults file */
				       mwApplicationShellWidgetClass, (char *) NULL);	/* terminate varargs list */

	drop_types = MwMalloc(3 * sizeof *drop_types);
	drop_types[0] =
	    XInternAtom(XtDisplay(topLevel), "text/plain", False);
	drop_types[1] =
	    XInternAtom(XtDisplay(topLevel), "text/uri-list", False);
	drop_types[2] = None;
	XtVaSetValues(topLevel, XtNdropTypes, drop_types, (char *) 0);
	XtAddCallback(topLevel, XtNdragCallback, cb_drag, NULL);
	XtAddCallback(topLevel, XtNdropCallback, cb_drop, NULL);

	MwInitFormat(XtDisplay(topLevel));

	theme_init(XtDisplay(topLevel));

	XSetErrorHandler(MwXErrorHandler);

	XtGetApplicationResources(topLevel, &app_data, resources,
				  XtNumber(resources), NULL, 0);
	plugin = app_data.plugin;
	grid_only = app_data.grid_only;

	XtAppAddActions(app_context, actions, XtNumber(actions));

	shortcuts = XtVaCreatePopupShell("shortcuts",
					 mwMenuWidgetClass, topLevel,
					 (char *) 0);
	XtRegisterGrabAction(popup_shortcuts, True,
			     ButtonPressMask | ButtonReleaseMask,
			     GrabModeAsync, GrabModeAsync);

	if (!app_data.grid_only) {
		topbox = XtVaCreateManagedWidget("topbox",
						 mwRudegridWidgetClass,
						 topLevel, (char *) 0);
	}

	MwHighlightInit(topLevel);
	tooltip =
	    XtVaCreatePopupShell("tooltip",
				 mwTooltipWidgetClass, topLevel,
				 (char *) 0);

	if (!app_data.grid_only) {
		init_menu();

		init_toolbar();
		setup_buttons();
		init_toggle();

		textbox = XtVaCreateManagedWidget("textbox",
						  formWidgetClass, topbox,
						  (char *) 0);
		label1 =
		    XtVaCreateManagedWidget("label1", labelWidgetClass,
					    textbox, XtNtop, XawChainTop,
					    XtNleft, XawChainLeft,
					    XtNright, XawChainLeft,
					    XtNbottom, XawChainTop,
					    XtNwidth, 100, (char *) NULL);
	}

	/* should move this line to somewhere after the
	   conditionals */
	XtAppAddActions(app_context, input_actions,
			XtNumber(input_actions));

	if (!app_data.grid_only) {
		textframe = XtVaCreateManagedWidget("textframe",
						    mwFrameWidgetClass,
						    textbox, XtNtop,
						    XawChainTop, XtNleft,
						    XawChainLeft, XtNright,
						    XawChainRight,
						    XtNbottom,
						    XawChainBottom,
						    XtNfromHoriz, label1,
						    XtNwidth, 496,
						    XtNheight, 24,
						    (char *) 0);
		text1 =
		    XtVaCreateManagedWidget("text1", mwRichtextWidgetClass,
					    textframe,
					    XtNrichtextVisibleCursor,
					    False, XtNwidth, 492,
					    XtNheight, 24, (char *) NULL);

		gridpane = XtVaCreateManagedWidget("gridpane",
						   panedWidgetClass,
						   topbox, XtNallowResize,
						   True, (char *) NULL);

		statusbox = XtVaCreateManagedWidget("statusbox",
						    mwRudegridWidgetClass,
						    topbox, (char *) 0);
		label2 =
		    XtVaCreateManagedWidget("label2", labelWidgetClass,
					    statusbox, (char *) NULL);
		label3 =
		    XtVaCreateManagedWidget("label3", labelWidgetClass,
					    statusbox, (char *) 0);
	}
}

static void replace_tabs(window * w)
{
	int n, top;
	buffer *b;

	if (w->ui->tab == None)
		return;

	XtVaGetValues(w->ui->tab,
		      XtNtabbingCount, &n, XtNtabbingTop, &top,
		      (char *) 0);
	while (n)
		MwTabbingRemove(w->ui->tab, --n);
	for (n = 0; n < w->buf->nsht; n++) {
		MwTabbingInsert(w->ui->tab, w->buf->sht[n].name, n);
	}
	if (top >= w->buf->nsht)
		top = w->buf->nsht - 1;

	/* now add tabs for all other buffers */ b = b_list;
	do {
		if (b != w->buf) {
			char p[1000];
			sprintf(p, "%s:", b->name);
			MwTabbingInsert(w->ui->tab, p, -1);
		}
		b = b->next;
	} while (b != b_list);

	XtVaSetValues(w->ui->tab,
		      XtNtabbingSelected, w->sht, XtNtabbingTop, top,
		      (char *) 0);
}

/* --- This function must also keep track of which plugins to hide
when the sheet is changed. Go through the list and hide all plugins
that are not on the current sheet of the current window.  */
void activate_window(window * w)
{
	char b[256];
	int tab;
	int s;
	buffer *buf;

	if (w_list)
		XtVaSetValues(w_list->ui->grid,
			      XtNtableVisibleCursor, False, (char *) NULL);
	w_list = w;
	s = w->sht;

	replace_tabs(w_list);

	strcpy(b, "Siag: ");
	strncat(b, w->buf->name, 200);
	if (w->ui->tab != None) {
		tab = MwTabbingTextToPos(w->ui->tab, w->buf->sht[s].name);
		if (tab < 0) {
			w_list->sht = s = tab = 0;
		}
		XtVaSetValues(w->ui->tab, XtNtabbingSelected, tab,
			      (char *) 0);
	}
	XtVaSetValues(topLevel, XtNtitle, b, (char *) NULL);
	XtVaSetValues(w->ui->grid, XtNtableVisibleCursor, True,
		      (char *) NULL);

	XtSetKeyboardFocus(topLevel, w->ui->grid);

	/* and now the plugins */ buf = b_list;
	do {
		for (s = 0; s < buf->nsht; s++) {
			int i;
			for (i = 0; i < buf->sht[s].nplugin; i++) {
				if ((w_list->buf != buf || w_list->sht
				     != s)
				    && buf->sht[s].plugin[i].displayed) {
					plugin_hide(buf->sht[s].plugin[i].
						    ph);
					buf->sht[s].plugin[i].displayed =
					    0;
					pr_scr_flag = 1;
				}
			}
		}
		buf = buf->next;
	} while (buf != b_list);	/* there is no need to display any
					   plugins here, that is taken
					   care of elsewhere.
					 */
}

/* --- */
Pixmap draw_snapshot(void)
{
	int x, y;
	unsigned int width, height, border_width, depth;
	Window cell_win, root;
	Pixmap bitmap;
	GC gc;
	unsigned long valuemask = 0;
	XGCValues values;
	Widget grid = w_list->ui->grid;

	cell_win = XtWindow(grid);
	XGetGeometry(XtDisplay(grid),
		     cell_win, &root,
		     &x, &y, &width, &height, &border_width, &depth);
	bitmap = XCreatePixmap(XtDisplay(topLevel), cell_win, width,
			       height, 1);
	gc = XCreateGC(XtDisplay(grid), bitmap, valuemask, &values);
	XCopyPlane(XtDisplay(grid), cell_win, bitmap, gc, 0,
		   0, width, height, 0, 0, 1);
	XFreeGC(XtDisplay(grid), gc);

	return bitmap;
}

/* --- */
window *find_window_by_widget(Widget wdg)
{
	window *w = w_list;
	do {
		if (w->ui->viewport == wdg || w->ui->selectall == wdg ||
		    w->ui->colnum == wdg || w->ui->rownum == wdg ||
		    w->ui->grid == wdg || w->ui->vscroll == wdg ||
		    w->ui->hscroll == wdg || w->ui->tab == wdg ||
		    w->ui->tabl == wdg || w->ui->tabr == wdg)
			return w;
		w = w->next;
	} while (w != w_list);
	return NULL;
}

/* --- */
void free_window(window * w)
{
	window *pw;
	int i;

	for (pw = w_list; pw->next != w && pw->next != pw; pw = pw->next);
	pw->next = w->next;

	if (w_list == w)
		w_list = w_list->next;
	if (w_list == w)
		w_list = NULL;
	if (w->ui->viewport != None)
		XtDestroyWidget(w->ui->viewport);

	if (b_list) {		/* don't do this if there are no buffers left */
		for (i = 0; i < w->buf->sht[w->sht].nplugin; i++) {
			if (w->buf->sht[w->sht].plugin[i].displayed) {
				plugin_hide(w->buf->sht[w->sht].plugin[i].
					    ph);
				w->buf->sht[w->sht].plugin[i].displayed =
				    0;
			}
		}
	}
	MwFree(w->ui);
	MwFree(w);
}

/* --- used for the XtNtablePluginCoords resource */

static void plugin_coordinates(Widget w, XtPointer p, int *x, int *y)
{
	window *win = (window *) p;
	buffer *b = win->buf;
	int n, ph;
	*x = *y = 0;
	ph = plugin_find_by_widget(w);
	if (ph == -1) {
		/* not a plugin; must be editing in place
		 */ if (w_list == NULL)
			*x = *y = 0;
		else
			MwTableZoomedCellToCoords(w_list->ui->grid,
						  get_point(win).row,
						  get_point(win).col, x,
						  y);
		(*x)--;
		(*y)--;

		return;
	}
	n = buffer_plugin2index(b, win->sht, ph);
	if (n == -1)
		return;

	MwTableZoomedCellToCoords(w_list->ui->grid,
				  b->sht[win->sht].plugin[n].row,
				  b->sht[win->sht].plugin[n].col, x, y);
}

static Cursor lr_cursor, ud_cursor, sizing_cursor;

static void colnum_cursor(Widget w, XtPointer p, XEvent * event, Boolean * n)
{
	int col, row, x, y, x1, y1, w1;
	window *win = find_window_by_widget(w);

	x = event->xmotion.x / zoom;
	y = 0;
	get_coords_cell(win,
			get_top(win).row, get_top(win).col,
			&row, &col, x, y);
	get_cell_coords(win, get_top(win).row, get_top(win).col,
			row, col, &x1, &y1);
	w1 = x - x1;
	if (cell_width(win->buf, win->sht, col) - w1 < 5) {
		XDefineCursor(XtDisplay(w), XtWindow(w), lr_cursor);
	} else {
		XDefineCursor(XtDisplay(w), XtWindow(w), None);
	}
}

static void rownum_cursor(Widget w, XtPointer p, XEvent * event, Boolean * n)
{
	int col, row, x, y, x1, y1, h1;
	window *win = find_window_by_widget(w);

	y = event->xmotion.y / zoom;
	x = 0;
	get_coords_cell(win,
			get_top(win).row, get_top(win).col,
			&row, &col, x, y);
	get_cell_coords(win, get_top(win).row, get_top(win).col,
			row, col, &x1, &y1);
	h1 = y - y1;
	if (cell_height(win->buf, win->sht, row) - h1 < 5) {
		XDefineCursor(XtDisplay(w), XtWindow(w), ud_cursor);
	} else {
		XDefineCursor(XtDisplay(w), XtWindow(w), None);
	}
}

static void grid_cursor(Widget w, XtPointer p, XEvent * event, Boolean * n)
{
	int col, row, x, y, x1, y1, w1, h1, wd, hd;
	window *win = find_window_by_widget(w);

	y = event->xmotion.y / zoom;
	x = event->xmotion.x / zoom;
	get_coords_cell(win, get_top(win).row, get_top(win).col,
			&row, &col, x, y);
	get_cell_coords(win, get_top(win).row, get_top(win).col,
			row, col, &x1, &y1);
	h1 = y - y1;
	w1 = x - x1;
	hd = cell_height(win->buf, win->sht, row) - h1;
	wd = cell_width(win->buf, win->sht, col) - w1;
	if (hd < 5 && wd < 5) {
		XDefineCursor(XtDisplay(w), XtWindow(w), sizing_cursor);
	} else if (hd < 5) {
		XDefineCursor(XtDisplay(w), XtWindow(w), ud_cursor);
	} else if (wd < 5) {
		XDefineCursor(XtDisplay(w), XtWindow(w), lr_cursor);
	} else {
		XDefineCursor(XtDisplay(w), XtWindow(w), None);
	}
}

static void select_tab(Widget w, XtPointer client_data, XtPointer call_data)
{
	buffer *b;
	int s = (int) call_data;
	char *name = MwTabbingPosToText(w, s);
	if (name == NULL)
		return;

	w_list = find_window_by_widget(w);
	b = find_sheet_by_name(name, w_list->buf, &s);
	if (b == NULL)
		return;
	hide_cur(w_list);
	w_list->buf = b;
	w_list->sht = s;
	pr_scr_flag = TRUE;
	replace_tabs(w_list);
	activate_window(w_list);
	show_cur(w_list);
}

/* The tab being renamed is always the one already selected, so we don't
need to find_sheet_by_name. That wouldn't work anyway, since the names
don't match.  */
static void rename_tab(Widget w, XtPointer client_data,
					XtPointer call_data)
{
	char *name = MwTabbingPosToText(w, (int) call_data);
	if (name == NULL)
		return;
	hide_cur(w_list);
	buffer_rename_sheet(w_list->buf, w_list->sht, name);
	pr_scr_flag = 1;
	w_list->buf->change = 1;
	replace_tabs(w_list);
	activate_window(w_list);
	show_cur(w_list);
}

static char *rownum_text(buffer * buf, int row, int col)
{
	static char b[80];
	sprintf(b, "%d", row);
	return b;
}


/* --- */
window *new_window(buffer * b, window * prev)
{
	Dimension totalwidth, formheight, w1, h1;
	int d;			/* distance
				   between widgets in form */
	window *w;

	if (w_list && app_data.grid_only)
		return NULL;	/* there can be only one */

	w = (window *) MwMalloc(sizeof(window));

	if (w == NULL)
		return NULL;

	w->ui = (siag_ui *) MwMalloc(sizeof(siag_ui));
	if (w->ui == NULL) {
		MwFree(w);
		return NULL;
	}

	w->buf = b;
	w->sht = w->bsht = 0;

	if (prev == NULL)
		prev = w;
	else
		w->next = prev->next;
	prev->next = w;

	if (app_data.grid_only) {
		w->ui->viewport = w->ui->selectall =
		    w->ui->tab = w->ui->colnum = w->ui->rownum =
		    w->ui->vscroll = w->ui->hscroll = None;
		w->ui->grid = XtVaCreateManagedWidget("grid",
						      mwTableWidgetClass,
						      topLevel,
						      XtNtableData, w,
						      XtNtablePluginCoords,
						      plugin_coordinates,
						      XtNtableRowHeight,
						      tcell_height,
						      XtNtableColWidth,
						      tcell_width,
						      (char *) NULL);
		XtAddEventHandler(w->ui->grid, PointerMotionMask, False,
				  grid_cursor, NULL);
		return w;
	}

	/* the rest is for running with the full set of widgets */

	/* Figure out how big the new form should be.                   */
	/* The total width must be the width of the gridpane.           */
	XtVaGetValues(gridpane, XtNwidth, &totalwidth, (char *) NULL);

	/* The form height is whatever we get, but
	   if it is too small we cannot create the new
	   window.                                     */ formheight = 100;
	w->ui->viewport = XtVaCreateManagedWidget("viewport",
						  mwRudegridWidgetClass,
						  gridpane, XtNwidth,
						  totalwidth, (char *) 0);
	XtVaGetValues(w->ui->viewport, XtNheight, &formheight,
		      XtNdefaultDistance, &d, (char *) NULL);

	w->ui->selectall = XtVaCreateManagedWidget("selectall",
						   commandWidgetClass,
						   w->ui->viewport,
						   (char *) NULL);
	MwTooltipAdd(tooltip, w->ui->selectall, _("Select everything"));
	XtAddCallback(w->ui->selectall, XtNcallback,
		      execute_callback, "(select-all)");

	/* The colnum should take up all the space between the button and
	   vscroll                                                          */
	w1 = totalwidth - MwWidthGet(w->ui->selectall) - 20 - 4 * d;	/*
									   just guessing */
	h1 = MwHeightGet(w->ui->selectall);

	w->ui->colnum = XtVaCreateManagedWidget("colnum",
						mwTableWidgetClass,
						w->ui->viewport,
						XtNtableDefaultHeight, h1,
						XtNtableColWidth,
						tcolnum_width,
#if 1
						XtNtableFormat,
						rowcol_format,
#endif
						XtNtableText,
						colnum_text_wrapper,
						XtNtableMaxRow, 1,
						XtNtableMaxCol,
						BUFFER_COLS, XtNtable3D,
						True, (char *) NULL);
	XtAddEventHandler(w->ui->colnum, PointerMotionMask, False,
			  colnum_cursor, NULL);
	w1 = MwWidthGet(w->ui->selectall);
	h1 = formheight - MwHeightGet(w->ui->selectall) - 20 - 4 * d;

	w->ui->rownum = XtVaCreateManagedWidget("rownum",
						mwTableWidgetClass,
						w->ui->viewport,
						XtNtableDefaultWidth, w1,
						XtNtableRowHeight,
						trownum_height,
#if 1
						XtNtableFormat,
						rowcol_format,
#endif
						XtNtableText, rownum_text,
						XtNtableMaxRow,
						BUFFER_ROWS,
						XtNtableMaxCol, 1,
						XtNtable3D, True,
						(char *) NULL);
	XtAddEventHandler(w->ui->rownum, PointerMotionMask, False,
			  rownum_cursor, NULL);
	w1 = MwWidthGet(w->ui->colnum);
	h1 = MwHeightGet(w->ui->rownum);

	w->ui->grid = XtVaCreateManagedWidget("grid",
					      mwTableWidgetClass,
					      w->ui->viewport,
					      XtNtableData, w,
					      XtNtablePluginCoords,
					      plugin_coordinates,
					      XtNtableRowHeight,
					      tcell_height,
					      XtNtableColWidth,
					      tcell_width, (char *) NULL);
	XtAddEventHandler(w->ui->grid, PointerMotionMask, False,
			  grid_cursor, NULL);
	h1 = MwHeightGet(w->ui->colnum) + MwHeightGet(w->ui->grid) + d;
	w->ui->vscroll = XtVaCreateManagedWidget("vscroll",
						 scrollbarWidgetClass,
						 w->ui->viewport,
						 (char *) NULL);
	w1 = MwWidthGet(w->ui->rownum) + MwWidthGet(w->ui->grid) + d;
	w->ui->hscroll = XtVaCreateManagedWidget("hscroll",
						 scrollbarWidgetClass,
						 w->ui->viewport,
						 (char *) NULL);
	w->ui->tab =
	    XtVaCreateManagedWidget("tab", mwTabbingWidgetClass,
				    w->ui->viewport, (char *) 0);
	XtAddCallback(w->ui->tab, XtNselectCallback, select_tab, NULL);
	XtAddCallback(w->ui->tab, XtNrenameCallback, rename_tab, NULL);
	b = b_list;
	XtVaSetValues(w->ui->tab,
		      XtNtabbingSelected, MwTabbingTextToPos(w->ui->tab,
							     w->buf->
							     sht[0].name),
		      (char *) 0);
	w->ui->tabl =
	    XtVaCreateManagedWidget("tabl", repeaterWidgetClass,
				    w->ui->viewport, (char *) 0);
	XtAddCallback(w->ui->tabl, XtNcallback, tabs_left,
		      (XtPointer) w->ui->tab);
	w->ui->tabr =
	    XtVaCreateManagedWidget("tabr", repeaterWidgetClass,
				    w->ui->viewport, (char *) 0);
	XtAddCallback(w->ui->tabr, XtNcallback, tabs_right,
		      (XtPointer) w->ui->tab);
	XtAddCallback(w->ui->vscroll, XtNjumpProc, vscroll_jump, NULL);
	XtAddCallback(w->ui->vscroll, XtNscrollProc, vscroll_scroll, NULL);

	XtAddCallback(w->ui->hscroll, XtNjumpProc, hscroll_jump, NULL);
	XtAddCallback(w->ui->hscroll, XtNscrollProc, hscroll_scroll, NULL);

	XtVaGetValues(w->ui->viewport,
		      XtNheight, &formheight, (char *) NULL);

	return w;
}

/* --- this does probably not belong here in window.c,
   because it doesn't depend on X
*/

static void save_plugin(char *p)
{
	if (*p++ != ' ' || *p == '\0')
		printf("501 File name missing\n");
	else {
		if (savematrix(p, w_list->buf, NULL)) {
			printf("501 Can't save %s\n", p);
		} else {
			w_list->buf->change = FALSE;
			printf("250 Saved %s\n", p);
		}
	}
}

static void load_plugin(char *p)
{
	if (*p++ != ' ' || *p == '\0')
		printf("501 File name missing\n");
	else {
		if (loadmatrix(p, w_list->buf, NULL)) {
			printf("501 Can't load %s\n", p);
		} else {
			w_list->buf->change = FALSE;
			printf("250 Loaded %s\n", p);
		}
	}
}

static void exec_plugin(char *p)
{
	if (*p++ != ' ' || *p == '\0')
		printf("501 Command missing\n");
	else {
		execute(p);
		printf("250 OK\n");
	}
}

static void help_plugin(char *p)
{
	printf("214 SAVE LOAD EXEC HELP NOOP QUIT PRNT\n");
}

static void noop_plugin(char *p)
{
	printf("250 OK\n");
}

static void win_plugin(char *p)
{
	printf("250 %lx\n", (unsigned long) XtWindow(topLevel));
}

static void quit_plugin(char *p)
{
	printf("221 Over and out\n");
	execute("(quit-program)");
}

static void prnt_plugin(char *p)
{
	Widget w = w_list->ui->grid;
	Display *dpy = XtDisplay(w);
	Pixmap pm = MwTablePixmap(w);
	XFreePixmap(dpy, pm);
	printf("502 Can't print yet\n");
}

static struct {
	char *verb;
	void (*cb) (char *);
} plugin_cmds[] = {
	{
	"SAVE", save_plugin}, {
	"LOAD", load_plugin}, {
	"EXEC", exec_plugin}, {
	"HELP", help_plugin}, {
	"NOOP", noop_plugin}, {
	"WIN", win_plugin}, {
	"QUIT", quit_plugin}, {
	"PRNT", prnt_plugin}, {
	NULL, NULL}
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
		(*plugin_cmds[i].cb) (b + strlen(plugin_cmds[i].verb));
	else
		printf("500 What are you talking about\n");
	fflush(stdout);
}

/* ---
*/
void mainloop(void)
{
	if (app_data.plugin) {
		/* control plugin from stdin */
		XtAppAddInput(XtWidgetToApplicationContext(topLevel),
			      fileno(stdin), (XtPointer) XtInputReadMask,
			      read_plugin_cmd, NULL);
		printf("220 %s\n", version);
		fflush(stdout);
	}

	XtAppMainLoop(app_context);

	exit(0);
}

/* ---
handle spontaneous exit of plugins
*/

static void handle_plugin_exit(int ph)
{
	int s;
	buffer *b = b_list;

	for (s = 0; s < b->nsht; s++) {
		do {
			int n = buffer_plugin2index(b, s, ph);
			if (n != -1) {
				MwFree(b->sht[s].plugin[n].name);
				b->sht[s].nplugin--;
				for (; n < b->sht[s].nplugin; n++)
					b->sht[s].plugin[n] =
					    b->sht[s].plugin[n + 1];
				b->change = pr_scr_flag = TRUE;
			}
			b = b->next;
		} while (b != b_list);
	}
}

static void plugin_exec(char *p)
{
	execute(p);
}


void init_windows(buffer * b, int *argc, char **argv)
{
	char *p;
	Cursor rownum_cursor;
	Cursor colnum_cursor;
	Atom wm_delete_window;	/* Atom sent to destroy a window */
	Display *dpy;

	start_splash();

	init_windows1(argc, argv);
	interp_startup();
	dpy = XtDisplay(topLevel);

	MwEncodeFormat(~0, &fmt0);

	XtRealizeWidget(topLevel);

	plugin_init(topLevel, handle_plugin_exit, plugin_exec);

	lr_cursor = XCreateFontCursor(dpy, XC_sb_h_double_arrow);
	ud_cursor = XCreateFontCursor(dpy, XC_sb_v_double_arrow);
	rownum_cursor = XCreateFontCursor(dpy, XC_sb_right_arrow);
	colnum_cursor = XCreateFontCursor(dpy, XC_sb_down_arrow);
	sizing_cursor = XCreateFontCursor(dpy, XC_sizing);

	activate_window(new_window(b, NULL));

	wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
	XtOverrideTranslations(topLevel,
			       XtParseTranslationTable
			       ("<Message>WM_PROTOCOLS: execute(quit-program)"));

	XSetWMProtocols(dpy, XtWindow(topLevel), &wm_delete_window, 1);

	draw_scrollbars(w_list);
	p = ret_text(w_list->buf, w_list->sht,
		     get_point(w_list).row, get_point(w_list).col);
	if (p == NULL)
		p = "";
	draw_input(p);
	draw_status("");
	draw_cells(w_list);
	draw_colnums(w_list);
	draw_rownums(w_list);

	/* Set up selection */
	target_atom = XInternAtom(dpy, "SIAG_BLOCK", False);
	clipboard = XInternAtom(dpy, "CLIPBOARD", False);

	embed_init(topLevel);

	init_subr_1("add-menu", add_menu);
	init_subr_3("add-menu-entry", add_menu_entry);
	init_subr_2("add-submenu", add_submenu);
	init_subr_4("add-submenu-entry", add_submenu_entry);
	init_subr_1("tooltip-mode", ltooltip_mode);
	init_subr_0("cut_to_clipboard", lcut_to_clipboard);
	init_subr_0("copy_to_clipboard", lcopy_to_clipboard);
	init_subr_0("paste_from_clipboard", lpaste_from_clipboard);

	init_calc_cmds();

	init_form(topLevel);

	MwSetIcon(topLevel, siag_xpm);

	stop_splash();
}

/* ---
Clean up before exit.  All buffers and windows are freed.
*/

void exit_windows(void)
{
	/* free all buffers */
	while (b_list != NULL)
		free_buffer(b_list);
	while (w_list != NULL)
		free_window(w_list);
	/* remove all temp files */
	deletia_mark(0);
	deletia_reap();
}

/* ---
   Prints and refreshes all the windows.
   Sets pr_scr_flag to FALSE.
   970422: also recalculate if needed
   980717: check for unparented plugins
*/

static void pr_scr(void)
{
	window *w;
	buffer *b;
	int s;
	int i;

	draw_status("");
	b = b_list;
	do {
		if (b->recalc) {
			int i;
			b->recalc = 0;
			for (i = 0; i < recalc; i++) {
				for (s = 0; s < b->nsht; s++) {
					calc_matrix(b);
				}
			}
		}
		b = b->next;
	} while (b != b_list);

	w = w_list;
	do {
		draw_cells(w);
		draw_colnums(w);
		draw_rownums(w);
		draw_scrollbars(w);

		/* check for nondisplayed plugins */
		b = w->buf;
		s = w->sht;
		for (i = 0; i < b->sht[s].nplugin; i++) {
			if (!b->sht[s].plugin[i].displayed) {
				plugin_show(b->sht[s].plugin[i].ph,
					    w->ui->grid);
				b->sht[s].plugin[i].displayed = 1;
			}
		}
		replace_tabs(w);
		w = w->next;
	} while (w != w_list);
	pr_scr_flag = FALSE;
	deletia_reap();
}				/* pr_scr */

/* ---
*/
void show_format(void)
{
	static int last_fmt = -1;
	int fmt = ret_format(w_list->buf, w_list->sht,
			     get_point(w_list).row,
			     get_point(w_list).col);
	MwFmt ft;
	char b[100];
	MwDecodeFormat(fmt, ~0, &ft);
	sprintf(b, "%d", ft.size / 10);

	if (fmt != last_fmt) {
		/* menus */
		/* These are Combo controls, and internationalization
		   does not work.
		 */
		MwComboTextChange(btnFont, /*_*/ (ft.family));
		MwComboTextChange(btnSize, b);
		MwComboTextChange(btnStyle, /*_*/ (style2name(ft.style)));
		MwComboTextChange(btnColor, /*_*/ (ft.fg));
	}

	last_fmt = fmt;

	/* toggle buttons */
	MwStateSet(cmdBold, (ft.bold ? 1 : 0), 1, 0);
	MwStateSet(cmdItalic, (ft.italic ? 1 : 0), 1, 0);
	MwStateSet(cmdHLeft, (ft.hadj == MW_HADJ_LEFT ? 1 : 0), 1, 0);
	MwStateSet(cmdHCenter, (ft.hadj == MW_HADJ_CENTER ? 1 : 0), 1, 0);
	MwStateSet(cmdHRight, (ft.hadj == MW_HADJ_RIGHT ? 1 : 0), 1, 0);
}

int cursor_visible = FALSE;

/* ---
   void show_cur(window *w)
   Moves the cursor to reflect the position of point in w.
   If point is not visible, the window is moved so that point is in
   the middle of the screen.
*/

void show_cur(window * w)
{
	char *p;
	int top_row, top_col;

	if (!w)
		return;

	top_row = get_top(w).row;
	top_col = get_top(w).col;

	XtVaSetValues(w->ui->grid,
		      XtNtablePointRow, get_point(w).row,
		      XtNtablePointCol, get_point(w).col,
		      XtNtableVisibleCursor, True, (char *) NULL);

	if (pr_scr_flag) {
		pr_scr();
	} else {
		int r, c;
		/* this may have moved the top, so we must check that */
		XtVaGetValues(w->ui->grid,
			      XtNtableTopRow, &r,
			      XtNtableTopCol, &c, (char *) NULL);
		set_top(w, make_position(r, c));

		if (top_row != get_top(w).row) {
			draw_rownums(w);
			draw_vbar(w);
		}
		if (top_col != get_top(w).col) {
			draw_colnums(w);
			draw_hbar(w);
		}
	}
	p = ret_text(w->buf, w->sht, get_point(w).row, get_point(w).col);
	if (p == NULL)
		p = "";
	draw_input(p);
	show_format();
	cursor_visible = TRUE;
}				/* show_cur */
