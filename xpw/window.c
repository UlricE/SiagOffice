/*
   Pathetic Writer
   Copyright (C) 1997-2003  Ulric Eriksson <ulric@siag.nu>

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

/* ---
   Module name:    window.c

   This module handles windows: creating, destroying, printing on windows.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Repeater.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Paned.h>
#include <X11/xpm.h>

#include <Mowitz/Mowitz.h>

#include "../common/common.h"

#include "../common/bitmaps/pw.xpm"

#include "../xcommon/xcommon.h"
#include "../xcommon/embed.h"

#include "../xcommon/plugin.h"

#include "../pw/pw.h"
#include "xpw.h"
/*#include "drop.h"*/

#include "../siod/siod.h"

#define MENUBAR (1)
#define TOOLBAR (2)
#define FORMATBAR (4)

#define APPNAME "Pw"

String fallback_resources[] = {
#include "../xcommon/xcommon-ad.h"
#include "../xcommon/dialogs-ad.h"
#include "../xcommon/filesel-ad.h"
#include "../xcommon/nws-ad.h"
#include "app-defaults.h"
	NULL
};

Widget topLevel, topbox, frame1, frame2, frame3;
Widget menubox, toolbox, formatbox, textbox, statusbox;
Widget form, gridpane, textframe, text1, label1, label2, label3;

/* The toolbar */
Widget tbOpen, tbSave, tbView, tbPrint, tbHelp;

Widget btnFont, mnuFont, btnSize, mnuSize, btnStyle, mnuStyle,
        btnColor, mnuColor;
Widget cmdBold, cmdItalic, cmdUline, cmdStrike;
Widget cmdHFull, cmdHLeft, cmdHCenter, cmdHRight;
Widget cmdVTop, cmdVBottom;
static Widget shortcuts, tooltip;

float zoom = 1.0;

static int bars = 0;

static int we_have_selection, start_row, start_col;
static int we_have_clipboard;
static char *clip_string;

typedef struct {
        Boolean plugin;
} AppData;

static AppData app_data;
static XtAppContext app_context;

#define XtNplugin "plugin"
#define XtCPlugin "Plugin"

static XtResource resources[] = {
        {
                XtNplugin,
                XtCPlugin,
                XtRBoolean,
                sizeof(Boolean),
                XtOffsetOf(AppData, plugin),
                XtRImmediate,
                (XtPointer)False
        }
};

static XrmOptionDescRec options[] = {
        {"-plugin", "*plugin", XrmoptionNoArg, "True"}
};

window *w_list;

int pr_line_flag;
int display_bops = 0;

Display *display;

Window root;
static Cursor grid_cursor;
static unsigned long blockbg, noblockbg;

Atom wm_delete_window;	/* Atom sent to destroy a window */
Atom target_atom;	/* used for selection */
Atom clipboard;
Atom *drop_types;

/* Selection stuff */

/* ---
   Module name:    selection.c

   This module handles selection: grabbing, releasing, cutting, pasting.

   The selection uses a custom target PW_BLOCK, which is simple
   plaintext. No formatting is preserved.

   All in all, plenty good enough for now.
--- */

Boolean convert_proc(Widget w,
	Atom *selection, Atom *target, Atom *type_return,
	XtPointer *value_return, unsigned long *length_return,
	int *format_return)
{
	window *wl = find_window_by_widget(w);
	unsigned int lr;

	XSelectionRequestEvent *req = XtGetSelectionRequest(w,
		*selection, (XtRequestId) NULL);

	/* handle all required atoms, and the one that we use */
	/* Xt already handles MULTIPLE, no branch necessary */
	if (*target == XA_TARGETS(XtDisplay(w))) {
		Atom *targetP;
		Atom *std_targets;
		unsigned long std_length;
		XmuConvertStandardSelection(w, req->time, selection,
			target, type_return,
			(XPointer *)&std_targets,
			&std_length, format_return);
		*value_return = XtMalloc(sizeof(Atom)*(std_length+1));
		targetP = *(Atom **)value_return;
		*length_return = std_length+1;
		*targetP++ = target_atom;
		memcpy(targetP, std_targets, sizeof(Atom)*std_length);
		XtFree((char *)std_targets);
		*type_return = XA_ATOM;
		*format_return = sizeof(Atom)*8;
		return True;
	} else if (*target == target_atom) {
		/* handle normal selection */
		char *data = pack_area(wl->buf, wl->bsht,
			block_upper(wl).row, block_upper(wl).col,
			block_lower(wl).row, block_lower(wl).col,
			&lr);
		*length_return = lr;
		*value_return = XtMalloc(*length_return);
		memcpy(*value_return, data, *length_return);
		MwFree(data);
		*type_return = target_atom;

		*format_return = 8;
		return True;
	} else if (*target == XA_STRING) {
		/* handle string selections (from outside PW) */
		char *data = pack_area(wl->buf, wl->bsht,
			block_upper(wl).row, block_upper(wl).col,
			block_lower(wl).row, block_lower(wl).col,
			&lr);
		*length_return = strlen(data);
		*value_return = XtMalloc(*length_return+1);
		memcpy(*value_return, data, *length_return+1);
		MwFree(data);

		*type_return = XA_STRING;
		*format_return = 8;
		return True;
	} else {
		if (XmuConvertStandardSelection(w, CurrentTime, selection,
				target, type_return, (XPointer *)value_return,
				length_return, format_return))
			return True;
		else {
#if 0	/* so what? */
			char *p = XGetAtomName(XtDisplay(w), *target);
			XtWarning("PW: unsupported selection type");
			XtWarning(p);
			XFree(p);
#endif
			return False;
		}
	}
}

Boolean convert_clip_proc(Widget w,
	Atom *selection, Atom *target, Atom *type_return,
	XtPointer *value_return, unsigned long *length_return,
	int *format_return)
{
	XSelectionRequestEvent *req = XtGetSelectionRequest(w,
		*selection, (XtRequestId) NULL);

	/* handle all required atoms, and the one that we use */
	/* Xt already handles MULTIPLE, no branch necessary */
	if (*target == XA_TARGETS(XtDisplay(w))) {
		Atom *targetP;
		Atom *std_targets;
		unsigned long std_length;
		XmuConvertStandardSelection(w, req->time, selection,
			target, type_return,
			(XPointer *)&std_targets,
			&std_length, format_return);
		*value_return = XtMalloc(sizeof(Atom)*(std_length+1));
		targetP = *(Atom **)value_return;
		*length_return = std_length+1;
		*targetP++ = target_atom;
		memcpy(targetP, std_targets, sizeof(Atom)*std_length);
		XtFree((char *)std_targets);
		*type_return = XA_ATOM;
		*format_return = sizeof(Atom)*8;
		return True;
	} else if (*target == target_atom) {
		/* handle normal selection */
		char *data = clip_string;
		*length_return = strlen(data);
		*value_return = XtMalloc(*length_return+1);
		strcpy(*value_return, data);
		*type_return = target_atom;

		*format_return = 8;
		return True;
	} else if (*target == XA_STRING) {
		/* handle string selections (from outside PW) */
		char *data = clip_string;
		*length_return = strlen(data);
		*value_return = XtMalloc(*length_return+1);
		strcpy(*value_return, data);

		*type_return = XA_STRING;
		*format_return = 8;
		return True;
	} else {
		if (XmuConvertStandardSelection(w, CurrentTime, selection,
				target, type_return, (XPointer *)value_return,
				length_return, format_return))
			return True;
		else {
#if 0	/* so what? */
			char *p = XGetAtomName(XtDisplay(w), *target);
			XtWarning("PW: unsupported selection type");
			XtWarning(p);
			XFree(p);
#endif
			return False;
		}
	}
}

void lose_ownership_proc(Widget w, Atom *selection)
{
	window *wl = find_window_by_widget(w);
	int s = wl->bsht;
	sheet *st = wl->buf->sht;
	wl->bsht = -1;

	st[s].blku.row = st[s].blku.col = -1;
	st[s].blkl.row = st[s].blkl.col = -1;

	pr_scr_flag = TRUE;	/* Is this enough? */	
	show_cur(wl);
}

void drop_clipboard(void)
{
	if (!we_have_clipboard) return;
	we_have_clipboard = 0;
	XtDisownSelection(w_list->ui->grid, clipboard, CurrentTime);
	MwFree(clip_string);
	clip_string = NULL;
}

void lose_clip_proc(Widget w, Atom *selection)
{
	drop_clipboard();
}

void transfer_done_proc(Widget w, Atom *a, Atom *b)
{
	; /* no need to do anything at all */
}

void string_requestor_callback(Widget w, XtPointer client_data,
        Atom *selection, Atom *type, XtPointer value,
        unsigned long *length, int *format)
{
        if ((value == NULL) && (*length == 0)) {
                XBell(XtDisplay(w), 100);
                XtWarning("PW: no selection or selection timed out");
        } else {
                unpack_string_area(w_list->buf, (char *)value,
			w_list->sht,
                        get_point(w_list).row, get_point(w_list).col);
                w_list->buf->change = TRUE;
                XtFree((char *)value);
                pr_scr_flag = TRUE;
        }
	show_cur(w_list);
}

void requestor_callback(Widget w, XtPointer client_data,
	Atom *selection, Atom *type, XtPointer value,
	unsigned long *length, int *format)
{
	if ((value == NULL) && (*length == 0)) {
        /* if we asked for a PW_BLOCK and got a null response,
           we'll ask again, this time for an XA_STRING */
                XtGetSelectionValue(w, *selection, XA_STRING,
                        string_requestor_callback,
                        NULL, CurrentTime);     /* NULL is bogus event */
	} else {
		unpack_area(w_list->buf, (char *)value,
			w_list->sht,
			get_point(w_list).row, get_point(w_list).col);
		w_list->buf->change = TRUE;

		XtFree((char *)value);
		pr_scr_flag = TRUE;
	}
	show_cur(w_list);
}

static void drop_selection(void)
{
	if (!we_have_selection) return;
	we_have_selection = 0;
	XtDisownSelection(w_list->ui->grid, XA_PRIMARY, CurrentTime);
	w_list->bsht = -1;
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static void start_selection(void)
{
	if (we_have_selection) return;
	if (XtOwnSelection(w_list->ui->grid, XA_PRIMARY, CurrentTime,
			convert_proc, lose_ownership_proc, NULL) == False) {
		XtWarning("PW failed to become selection owner");
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
	if (XtOwnSelection(w_list->ui->grid, clipboard, CurrentTime,
			convert_clip_proc, lose_clip_proc, NULL) == False) {
		XtWarning("PW failed to become clipboard owner");
		return;
	}
	clip_string = pack_string(w_list->buf, w_list->sht,
			p1.row, p1.col, p2.row, p2.col);
	we_have_clipboard = 1;
}

static void set_selection(void)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	int r1, c1, r2, c2;
	position p = get_point(w_list);
	if (start_row < p.row || (start_row == p.row && start_col < p.col)) {
		r1 = start_row;
		c1 = start_col;
		r2 = p.row;
		c2 = p.col;
	} else {
		r1 = p.row;
		c1 = p.col;
		r2 = start_row;
		c2 = start_col;
	}
	if (c2) c2--;	/* at least this is less wrong */

	w_list->bsht = w_list->sht;
	st[s].blku.row = r1;
	st[s].blku.col = c1;
	st[s].blkl.row = r2;
	st[s].blkl.col = c2;
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

/* End of selection stuff */

/* ---
*/
void activate_window(window *w)
{
	char b[256];
	int tab, s;
	buffer *buf;

	w_list = w;
	s = w->sht;

	strcpy(b, "Pathetic Writer: ");
	strncat(b, w->buf->name, 200);
	tab = MwTabbingTextToPos(w->ui->tab, w->buf->sht[s].name);
	if (tab < 0) {
		w_list->sht = s = tab = 0;
	}
	XtVaSetValues(w->ui->tab, XtNtabbingSelected, tab, (char *)0);
	XtVaSetValues(topLevel, XtNtitle, b, (char *)0);
	XtVaSetValues(w->ui->grid,
		XtNrichtextData, w,
		XtNleftMargin, w->buf->left_margin,
		XtNrightMargin, w->buf->right_margin,
		XtNpaperWidth, w->buf->paper_width,
		(char *)0);
	XtVaSetValues(w->ui->ruler,
		XtNtabs, w->buf->sht[s].tabs,
		(char *)0);
	XtSetKeyboardFocus(topLevel, w->ui->grid);
	/* plugins */
	buf = b_list;
	do {
		for (s = 0; s < buf->nsht; s++) {
			int i;
			for (i = 0; i < buf->sht[s].nplugin; i++) {
				if ((w_list->buf != buf || w_list->sht != s)
					&& buf->sht[s].plugin[i].displayed) {
					plugin_hide(buf->sht[s].plugin[i].ph);
					buf->sht[s].plugin[i].displayed = 0;
					pr_scr_flag = 1;
				}
			}
		}
		buf = buf->next;
	} while (buf != b_list);
}

/* ---
*/
void draw_input(Display * display, char *text)
{
	char b[256];
	char b3[256];
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	sprintf(b, "[%d,%d - %d,%d]",
		st[s].blku.row, st[s].blku.col,
		st[s].blkl.row, st[s].blkl.col);
	sprintf(b3, "PW %d/%d %.0f%%",
		st[s].point_pos.row,
		st[s].point_pos.col,
		100*zoom);
	MwLabelSet(label1, b);
	MwLabelSet(label3, b3);
}

/* ---
*/
void draw_status(Display * display, char *text)
{
	char b[200];
	strncpy(b, text, 199);
	b[199] = '\0';
	MwChomp(b);
	MwLabelSet(label2, b);
	XFlush(display);
}

/* ---
   void llpr(char *p)
   Prints the string p on the bottom line of the screen.  If p is empty and
   the last string printed was also empty, the string isn't printed.
*/

void llpr(char *p)
{
	static int isclear = FALSE;

	if (isclear && p[0] == '\0')
		return;
	isclear = (p[0] == '\0');

	draw_status(display, p);
}


/* ---
This function is a dog: it copies from the grid which may well be
   partially obscured during the copy. As a result it doesn't work.
   It should use the richtext_pixmap function instead.
*/

Pixmap draw_snapshot(void)
{
	int x, y;
	unsigned int width, height, border_width, depth;
	Window cell_win, root;
	Pixmap bitmap;
	GC gc;
	unsigned long valuemask = 0;
	XGCValues values;

	cell_win = XtWindow(w_list->ui->grid);
	XGetGeometry(display, cell_win, &root,
		&x, &y, &width, &height, &border_width, &depth);
	bitmap = XCreatePixmap(display, cell_win, width, height, 1);
	gc = XCreateGC(display, bitmap, valuemask, &values);
	XCopyPlane(display, cell_win, bitmap, gc, 0, 0, width, height,
			0, 0, 1);
	XFreeGC(display, gc);

	return bitmap;
}

static int trow_height(XtPointer p, int r)
{
	window *w = (window *)p;
	buffer *b = w->buf;
	int s = w->sht;
	if (r > b->sht[s].used_lines) return 15;
	return b->sht[s].text[r].height;
}

static int trow_hadj(XtPointer p, int r)
{
	window *w = (window *)p;
	return ret_hadj(w->buf, w->sht, r);
}

static int trow_style(XtPointer p, int r)
{
	window *w = (window *)p;
	return ret_style(w->buf, w->sht, r);
}

static int trow_bop(XtPointer p, int r)
{
	window *w = (window *)p;
	buffer *b = w->buf;
	int s = w->sht;
	int n;

	if (1/*display_bops*/) n = ret_bop(b, s, r);
	else n = 0;
	return n;
}

static MwRichchar *trow_text(XtPointer p, int r)
{
	window *w = (window *)p;
	buffer *b = w->buf;
	int s = w->sht;
	if (r > b->sht[s].used_lines) return NULL;
	return b->sht[s].text[r].p;
}

/* ---
*/

void draw_buffer(Display *display, window *w)
{
	int s = w->sht;
	sheet *st = w->buf->sht;
	MwRichtextSetZoom(w->ui->grid, zoom);
	XtVaSetValues(w->ui->grid,
		XtNrichtextData, w,
		XtNrichtextSelectTopRow, st[s].blku.row,
		XtNrichtextSelectTopCol, st[s].blku.col,
		XtNrichtextSelectBottomRow, st[s].blkl.row,
		XtNrichtextSelectBottomCol, st[s].blkl.col,
		XtNrichtextPointRow, st[s].point_pos.row,
		XtNrichtextPointCol, st[s].point_pos.col,
		XtNrichtextPaperWidth, w->buf->paper_width,
		XtNrichtextLeftMargin, w->buf->left_margin,
		XtNrichtextRightMargin, w->buf->right_margin,
		XtNrichtextRedisplay, True,
		(char *)0);
	XtVaSetValues(w->ui->ruler,
		XtNtabs, st[s].tabs,
		(char *)0);
}

/* ---
*/
window *find_window_by_widget(Widget wdg)
{
	window *w = w_list;
	do {
		if (w->ui->viewport == wdg || w->ui->grid == wdg ||
			w->ui->vscroll == wdg || w->ui->hscroll == wdg ||
			w->ui->tab == wdg ||
			w->ui->tabl == wdg || w->ui->tabr)
			return w;
		w = w->next;
	} while (w != w_list);
	return NULL;
}

/* ---
*/
void free_window(window *w)
{
	window *pw;

	for (pw = w_list; pw->next != w && pw->next != pw; pw = pw->next);
	pw->next = w->next;

	if (w_list == w) w_list = w_list->next;
	if (w_list == w) w_list = NULL;
	XtDestroyWidget(w->ui->viewport);

	if (b_list) {
		buffer *b = w->buf;
		int s = w->sht;
		int i;
		for (i = 0; i < b->sht[s].nplugin; i++) {
			if (b->sht[s].plugin[i].displayed) {
				plugin_hide(b->sht[s].plugin[i].ph);
				b->sht[s].plugin[i].displayed = 0;
			}
		}
	}
	MwFree(w->ui);
	MwFree(w);
}

/* ---
used for the XtNtablePluginCoords resource
*/

static void tplugin_coordinates(Widget w, XtPointer p, int *x, int *y)
{
	window *win = (window *)p;
        buffer *b = win->buf;
        int n, ph;
	int wi, he;
        *x = *y = 0;
        ph = plugin_find_by_widget(w);
        if (ph == -1) return;
        n = buffer_plugin2index(b, win->sht, ph);
        if (n == -1) return;
	MwRichtextCharToCoords((MwRichtextWidget)win->ui->grid,
			b->sht[win->sht].plugin[n].row,
			b->sht[win->sht].plugin[n].col, x, y);
	plugin_size_get(ph, &wi, &he);
	switch (ret_hadj(b, win->sht, b->sht[win->sht].plugin[n].row)) {
	case MW_HADJ_CENTER:
		*x -= wi/2;
		break;
	case MW_HADJ_RIGHT:
		*x -= wi;
		break;
	default:
		break;
	}
}

static void replace_tabs(window *w)
{
	int n, top;
	buffer *b;
	XtVaGetValues(w->ui->tab,
		XtNtabbingCount, &n,
		XtNtabbingTop, &top,
		(char *)0);
	while (n) MwTabbingRemove(w->ui->tab, --n);
	for (n = 0; n < w->buf->nsht; n++) {
		MwTabbingInsert(w->ui->tab, w->buf->sht[n].name, n);
	}
	if (top >= w->buf->nsht)
		top = w->buf->nsht-1;

	b = b_list;
	do {
		if (b != w->buf) {
			char p[1000];
			sprintf(p, "%s:", b->name);
			MwTabbingInsert(w->ui->tab, p, -1);
		}
		b = b->next;
	} while (b != b_list);

	XtVaSetValues(w->ui->tab,
		XtNtabbingSelected, w->sht,
		XtNtabbingTop, top,
		(char *)0);
}

static void select_tab(Widget w,  XtPointer client_data, XtPointer call_data)
{
	buffer *b;
	int s = (int)call_data;
	char *name = MwTabbingPosToText(w, s);
	if (name == NULL) return;

	w_list = find_window_by_widget(w);
	b = find_sheet_by_name(name, w_list->buf, &s);
	if (b == NULL) return;

	hide_cur(w_list);

	w_list->buf = b;
	w_list->sht = s;
	pr_scr_flag = TRUE;
	replace_tabs(w_list);
	activate_window(w_list);
	show_cur(w_list);
}

static void rename_tab(Widget w, XtPointer client_data, XtPointer call_data)
{
        char *name = MwTabbingPosToText(w, (int)call_data);
        if (name == NULL) return;
	hide_cur(w_list);
        buffer_rename_sheet(w_list->buf, w_list->sht, name);
        pr_scr_flag = 1;
        w_list->buf->change = 1;
	replace_tabs(w_list);
	activate_window(w_list);
	show_cur(w_list);
}

/* ---
*/
int remove_window(window *w)
{
	
	if (w == w->next) return FALSE;
	free_window(w);
	return TRUE;
}

/* ---
*/
int split_window(window *w)
{
	window *w2 = new_window(w->buf, w);
	int s = w->sht;

	if (w2 == NULL) return FALSE;
	w2->buf->sht[s].point_pos = w->buf->sht[s].point_pos;
	return TRUE;
}

/* stuff snarfed from main.c */

static void set_point_rc(int row, int col)
{
        position p;

        p.row = row;
        p.col = col;
        set_point(w_list, p);
}

static void set_mark_rc(int row, int col)
{
        position p;

        p.row = row;
        p.col = col;
        set_point(w_list, p);
}

static void set_block(int row1, int col1, int row2, int col2)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
        int ur = st[s].blku.row, uc = st[s].blku.col;
        int lr = st[s].blkl.row, lc = st[s].blkl.col;

        st[s].blku.row = row1;
        st[s].blkl.row = row2;
        st[s].blku.col = col1;
        st[s].blkl.col = col2;
	w_list->bsht = s;

        /* Redraw if any change */
        if (ur != st[s].blku.row || uc != st[s].blku.col ||
                lr != st[s].blkl.row || lc != st[s].blkl.col) {
                pr_scr_flag = TRUE;
        }

        /* Become selection owner */
        /* this function should be integrated with the one in cmds.c */
        if (XtOwnSelection(w_list->ui->grid, XA_PRIMARY,
		CurrentTime, convert_proc,
                lose_ownership_proc, transfer_done_proc) == False) {
                XtWarning("PW: failed to become selection owner");
                st[s].blku.row = st[s].blku.col = -1;
                st[s].blkl.row = st[s].blkl.col = -1;
        }
}

static void unselect_block(window *w)
{
	buffer *b = w->buf;
	int s = w->sht;

	if (b->sht[s].blku.row != -1) {
		b->sht[s].blku.row = -1;
		b->sht[s].blku.col = -1;
		b->sht[s].blkl.row = -1;
		b->sht[s].blkl.col = -1;
		XtDisownSelection(w->ui->grid, XA_PRIMARY, CurrentTime);
		pr_scr_flag = TRUE;
	}
}

/* ---
*/
void GridButtonAction(Widget w, XEvent *event, String *params, Cardinal *n)
{
        int col, row;
        int x = event->xbutton.x, y = event->xbutton.y;
	int s;
	sheet *st;

        hide_cur(w_list);
        activate_window(find_window_by_widget(w));
	s = w_list->sht;
	st = w_list->buf->sht;
	MwRichtextCoordsToChar((MwRichtextWidget)w_list->ui->grid,
		&row, &col, x, y);
        if (*n < 1 || !strcmp(params[0], "point")) {
                set_point_rc(row, col);
		unselect_block(w_list);
        } else if (!strcmp(params[0], "mark")) {
                set_mark_rc(row, col);
        } else if (!strcmp(params[0], "block")) {
                set_block(row, col,
                        get_mark(w_list).row, get_mark(w_list).col);
                pr_scr_flag = TRUE;
        } else if (!strcmp(params[0], "paste")) {
                set_point_rc(row, col);
                XtGetSelectionValue(w, XA_PRIMARY, target_atom,
                        requestor_callback, event, event->xbutton.time);
                pr_scr_flag = TRUE;
        } else if (!strcmp(params[0], "select")) {
                int r1 = st[s].blku.row, c1 = st[s].blku.col;
                int r2 = st[s].blkl.row, c2 = st[s].blkl.col;
                /* is the selection set to something already? */
                if (r1 == -1 || c1 == -1 || r2 == -1 || c2 == -1) {
			int r = get_point(w_list).row;
			int c = get_point(w_list).col;
			if (row < r || (row == r && col < c)) {
				set_block(row, col, r, c);
			} else {
				set_block(r, c, row, col);
			}
                } else {
			if (row < r1 || (row == r1 && col <= c1)) {
				/* grow at the start */
				r1 = row, c1 = col;
			} else if (row > r2 || (row == r2 && col >= c2)) {
				/* grow at the end */
				r2 = row, c2 = col;
			} else if (ABS(r1-row) < ABS(r2-row)) {
				/* shrink at the start */
				r1 = row, c1 = col;
			} else {
				/* shrink at the end */
				r2 = row, c2 = col;
			}
                        set_block(r1, c1, r2, c2);
                }
        } else if (!strcmp(params[0], "adjust")) {
		int r = get_point(w_list).row, c = get_point(w_list).col;
		if (row < r || (row == r && col < c)) {
			set_block(row, col, r, c);
		} else {
			set_block(r, c, row, col);
		}
        }
        show_cur(w_list);
}

static void extend_left(Widget w, XEvent *event, String *params, Cardinal *n)
{
	start_selection();
	execute("(backward-char)");
	set_selection();
}

static void extend_right(Widget w, XEvent *event, String *params, Cardinal *n)
{
	start_selection();
	execute("(forward-char)");
	set_selection();
}

static void extend_up(Widget w, XEvent *event, String *params, Cardinal *n)
{
	start_selection();
	execute("(previous-line)");
	set_selection();
}

static void extend_down(Widget w, XEvent *event, String *params, Cardinal *n)
{
	start_selection();
	execute("(next-line)");
	set_selection();
}

static void backward_char(Widget w, XEvent *event, String *params, Cardinal *n)
{
	drop_selection();
	execute("(backward-char)");
}

static void forward_char(Widget w, XEvent *event, String *params, Cardinal *n)
{
	drop_selection();
	execute("(forward-char)");
}

static void next_line(Widget w, XEvent *event, String *params, Cardinal *n)
{
	drop_selection();
	execute("(next-line)");
}

static void previous_line(Widget w, XEvent *event, String *params, Cardinal *n)
{
	drop_selection();
	execute("(previous-line)");
}

static void cut_to_clipboard(Widget w, XEvent *event,
			String *params, Cardinal *n)
{
	if (!we_have_selection) {
		return;
	}
	start_clipboard();
	execute("(delete-block)");
	pr_scr_flag = TRUE;
	show_cur(w_list);
}

static void copy_to_clipboard(Widget w, XEvent *event,
			String *params, Cardinal *n)
{
	if (!we_have_selection) {
		return;
	}
	start_clipboard();
}

static void paste_from_clipboard(Widget w, XEvent *event,
			String *params, Cardinal *n)
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


/*
This action is extremely messy, being copied from the application code
of Pathetic Writer. We need to do it like this instead:
1. Copy the whole paragraph from pw using copy_line and delete_line.
   The result should be a single richtext string with tabs and newlines
   embedded as needed.
2. Convert the point position to an offset within the string.
3. Insert the character.
4. Move point forward one step.
5. Calculate the point position from the offset.
6. Repaint the whole paragraph. As we go along, the single string is
   broken up into lines again.
7. Insert the paragraph using insert_line.

Any of these events will cause the entire window to be repainted:
 - Point ends up outside the window.
 - The total height of the paragraph changes.
*/
static void SelfInsertChar(Widget w, XEvent *event, String *params, Cardinal *n)
{
	int count, bufsiz = 10;
	unsigned char buf[12];
	KeySym keysym;

	hide_cur(w_list);
	count = MwRichtextLookupString(w, event, buf, bufsiz, &keysym);
	if (count) unselect_block(w_list);
	if (keysym >= ' ' && count == 1) {
		int s = w_list->sht;
		sheet *st = w_list->buf->sht;
		buf[count] = '\0';
		ins_text(w_list->buf, s,
			st[s].point_pos, buf, w_list->current_fmt);
		rebreak_line(w_list->buf, s, st[s].point_pos.row);
		st[s].point_pos.col += count;
		/* point position may now be hosed */
		if (st[s].point_pos.col >
				line_length(w_list->buf, s, st[s].point_pos.row)) {
			st[s].point_pos.col -=
				line_length(w_list->buf, s, st[s].point_pos.row);
			st[s].point_pos.col -= 1; /* we deleted a space too */
			st[s].point_pos.row++;
		}
		/* and if that didn't help... */
		if (st[s].point_pos.col >
				line_length(w_list->buf, s, st[s].point_pos.row)) {
			st[s].point_pos.col =
				line_length(w_list->buf, s, st[s].point_pos.row);
		}
		pr_line_flag = TRUE;
	}
	show_cur(w_list);
}

static LISP ltooltip_mode(LISP newmode)
{
	int mode = get_c_long(newmode);
	XtVaSetValues(tooltip,
		XtNtooltipMode, mode,
		XtNtooltipLabel, label2,
		(char *)0);
	return NIL;
}

static void siaghelp_action(Widget w, XEvent *event,
        String *params, Cardinal *num_params)
{
        char b[1256];

        sprintf(b, "siagrun help file://localhost%s/pw/%s", docdir, params[0]);
	MwSpawn(b);
}

static void
execute_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
        char *b = (char *) client_data;

        execute(b);
}

static void tabs_left(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget tw = (Widget)client_data;
	int n;

	XtVaGetValues(tw, XtNtabbingTop, &n, (char *)0);
	XtVaSetValues(tw, XtNtabbingTop, n-1, (char *)0);
}

static void tabs_right(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget tw = (Widget)client_data;
	int n;

	XtVaGetValues(tw, XtNtabbingTop, &n, (char *)0);
	XtVaSetValues(tw, XtNtabbingTop, n+1, (char *)0);
}

static void newtabs_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	buffer *b = w_list->buf;
	int s = w_list->sht;
	char *t = (char *)call_data;
	int r;

	MwFree(b->sht[s].tabs);
	b->sht[s].tabs = MwStrdup(t);
	b->change = 1;
	pr_scr_flag = 1;
	for (r = 1; r < line_last_used(b, s); r++)
		rebreak_line(b, s, r);
	activate_window(w_list);
	show_cur(w_list);
}

/* ---
*/
window *new_window(buffer *b, window *prev)
{
	window *w;
	int s;
	sheet *st;

	w = (window *)MwMalloc(sizeof(window));

	if (w == NULL) return NULL;	/* can't really happen */

	w->ui = (pw_ui *)MwMalloc(sizeof(pw_ui));
	if (w->ui == NULL) {
		MwFree(w);
		return NULL;
	}

	w->buf = b;
	w->sht = 0;
	w->current_fmt = 0;

	if (prev == NULL) prev = w;
	else w->next = prev->next;
	prev->next = w;

	/* The form height is whatever we get, but if it is too small
	we cannot create the new window.				*/
	w->ui->viewport = XtVaCreateManagedWidget("viewport",
		mwRudegridWidgetClass, gridpane,
		(char *)0);

	w->ui->ruler = XtVaCreateManagedWidget("ruler",
		mwTabstopWidgetClass, w->ui->viewport,
		(char *)0);
	XtAddCallback(w->ui->ruler, XtNcallback, newtabs_cb, NULL);

	s = w->sht;
	st = w->buf->sht;
	w->ui->grid = XtVaCreateManagedWidget("grid",
		mwRichtextWidgetClass, w->ui->viewport,
		/* set nonchanging resources here */
		XtNrichtextData, w,
		XtNrichtextRowHeight, trow_height,
		XtNrichtextAdjHoriz, trow_hadj,
		XtNrichtextStyle, trow_style,
		XtNrichtextBop, trow_bop,
		XtNrichtextText, trow_text,
		/* as well as changing ones */
		XtNrichtextSelectTopRow, st[s].blku.row,
		XtNrichtextSelectTopCol, st[s].blku.col,
		XtNrichtextSelectBottomRow, st[s].blkl.row,
		XtNrichtextSelectBottomCol, st[s].blkl.col,
		XtNrichtextPointRow, st[s].point_pos.row,
		XtNrichtextPointCol, st[s].point_pos.col,
		XtNrichtextPluginCoords, tplugin_coordinates,
		XtNrichtextRuler, w->ui->ruler,
		(char *)0);
	XDefineCursor(display, XtWindow(w->ui->grid), grid_cursor);
	w->ui->vscroll = XtVaCreateManagedWidget("vscroll",
		scrollbarWidgetClass, w->ui->viewport,
		(char *)0);
	w->ui->hscroll = XtVaCreateManagedWidget("hscroll",
		scrollbarWidgetClass, w->ui->viewport,
		(char *)0);
	w->ui->tab = XtVaCreateManagedWidget("tab",
		mwTabbingWidgetClass, w->ui->viewport,
		(char *)0);
	XtAddCallback(w->ui->tab, XtNselectCallback, select_tab, NULL);
        XtAddCallback(w->ui->tab, XtNrenameCallback, rename_tab, NULL);
	b = b_list;
	XtVaSetValues(w->ui->tab,
		XtNtabbingSelected, MwTabbingTextToPos(w->ui->tab,
						w->buf->sht[0].name),
		(char *)0);
	w->ui->tabl = XtVaCreateManagedWidget("tabl",
		repeaterWidgetClass, w->ui->viewport,
		(char *)0);
	XtAddCallback(w->ui->tabl, XtNcallback,
			tabs_left, (XtPointer)w->ui->tab);
	w->ui->tabr = XtVaCreateManagedWidget("tabr",
		repeaterWidgetClass, w->ui->viewport,
		(char *)0);
	XtAddCallback(w->ui->tabr, XtNcallback,
			tabs_right, (XtPointer)w->ui->tab);

	/* The jump callback is for dragging the thumb */
	/* The scroll callback is for clicking */
	XtAddCallback(w->ui->vscroll, XtNjumpProc, vscroll_jump, NULL);
	XtAddCallback(w->ui->vscroll, XtNscrollProc, vscroll_scroll, NULL);

	XtAddCallback(w->ui->hscroll, XtNjumpProc, hscroll_jump, NULL);
	XtAddCallback(w->ui->hscroll, XtNscrollProc, hscroll_scroll, NULL);

	return w;
}

/* ---
*/
void vscroll_jump(Widget w, XtPointer client_data, XtPointer call_data)
{
        float top;
	window *win = find_window_by_widget(w);
	int top_row, th;

        activate_window(win);
	th = 20*line_last_used(win->buf, win->sht);
        XtVaGetValues(w, XtNtopOfThumb, &top, (char *)0);
	top_row = top*th;
	XtVaSetValues(w_list->ui->grid,
		XtNrichtextTopRow, top_row,
		(char *)0);
}

/* ---
*/
void vscroll_scroll(Widget w, XtPointer client_data, XtPointer call_data)
{
        int i = (long) call_data;
        Dimension length, height;
	window *win = find_window_by_widget(w);
	int top_row, th;

        activate_window(win);
        XtVaGetValues(w, XtNlength, &length, (char *)0);
	XtVaGetValues(win->ui->grid,
			XtNheight, &height,
			XtNrichtextTopRow, &top_row,
			(char *)0);
	th = 20*line_last_used(win->buf, win->sht);
        if (i < 0) {
                if ((length / -i) > 15) {
			top_row -= 0.1*height;
                } else {
			top_row -= 0.9*height;
                }
        } else {
                if ((length / i) > 15) {
			top_row += 0.1*height;
                } else {
			top_row += 0.9*height;
                }
        }
	if (top_row > th) top_row = th;
	if (top_row < 0) top_row = 0;
	XtVaSetValues(w_list->ui->grid,
		XtNrichtextTopRow, top_row,
		(char *)0);
	XawScrollbarSetThumb(w, (double)top_row/th, 0.0);
}

/* ---
*/
void hscroll_jump(Widget w, XtPointer client_data, XtPointer call_data)
{
        float top;
	window *win = find_window_by_widget(w);
	int top_col, pw;

        activate_window(win);
	pw = win->buf->paper_width;
        XtVaGetValues(w, XtNtopOfThumb, &top, (char *)0);
	top_col = top*pw;
	XtVaSetValues(w_list->ui->grid,
		XtNrichtextTopCol, top_col,
		(char *)0);
}

/* ---
*/
void hscroll_scroll(Widget w, XtPointer client_data, XtPointer call_data)
{
        int i = (long) call_data;
        Dimension length, width;
	window *win = find_window_by_widget(w);
	int top_col, change, pw;

        activate_window(win);
	pw = win->buf->paper_width;
        XtVaGetValues(w, XtNlength, &length, (char *)0);
	XtVaGetValues(win->ui->grid,
			XtNwidth, &width,
			XtNrichtextTopCol, &top_col,
			(char *)0);
        if (i < 0) {
                if ((length / -i) > 15) {
                        change = -0.1*width;
                } else {
                        change = -0.9*width;
                }
        } else {
                if ((length / i) > 15) {
                        change = 0.1*width;
                } else {
                        change = 0.9*width;
                }
        }
	top_col += change;
	if (top_col > pw) top_col = pw;
	if (top_col < 0) top_col = 0;
	XtVaSetValues(w_list->ui->grid,
		XtNrichtextTopCol, top_col,
		(char *)0);
	XawScrollbarSetThumb(w, (double)top_col/pw, 0.0);
}

static struct {
        char *label;
        Widget button, menu;
} *menubar;

static int menucount = 0;

static LISP add_menu(LISP lisp_label)
{
	char *label = MwStrdup(get_c_string(lisp_label));
        char button_name[80];

        sprintf(button_name, "btn%s", label);
	menubar = MwRealloc(menubar, (menucount+1)*(sizeof *menubar));
        menubar[menucount].label = MwStrdup(label);
        menubar[menucount].button = XtVaCreateManagedWidget(button_name,
                mwMBButtonObjectClass, menubox,
		XtNmenu_name, label,
	        XtNlabel, _(label),
		XtNgravitation, (strcmp(label, "Help")?XtCleft:XtCright),
		(char *)0);
        menubar[menucount].menu = XtVaCreatePopupShell(label,
                mwMenuWidgetClass, menubox,
		(char *)0);
        menucount++;

	return NIL;
}

static void remake_ylayout(void)
{
	char b[100];
	sprintf(b, "%s %s %s 100%% 30",
		(bars&MENUBAR) ? "30" : "0",
		(bars&TOOLBAR) ? "30" : "0",
		(bars&FORMATBAR) ? "30" : "0");
	XtVaSetValues(topbox,
		XtNyLayout, b,
		(char *)0);
}

static void attach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget vw = (Widget)call_data;
	if (vw == frame1) bars |= MENUBAR;
	if (vw == frame2) bars |= TOOLBAR;
	if (vw == frame3) bars |= FORMATBAR;
	remake_ylayout();
}

static void detach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget vw = (Widget)call_data;
	if (vw == frame1) bars &= ~MENUBAR;
	if (vw == frame2) bars &= ~TOOLBAR;
	if (vw == frame3) bars &= ~FORMATBAR;
	remake_ylayout();
}


static void init_menu(void)
{
	bars |= MENUBAR;
	frame1 = XtVaCreateManagedWidget("frame1",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(frame1, frame1, detach_bar, attach_bar);
	menubox = XtVaCreateManagedWidget("menubox",
		mwMenuBarWidgetClass, frame1,
		XtNgridx, 1,
		(char *)0);
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
        Widget entry;
        Widget menuw = find_menu_by_name(get_c_string(menu));
	char *lbl = MwStrdup(get_c_string(label));

        if (!menuw) {
                return NIL;
        }

        if (!strcmp(lbl, "-")) {  /* line pane */
                entry = XtVaCreateManagedWidget("-",
                                mwLineMEObjectClass, menuw,
                                (char *)0);
        } else {
                entry = XtVaCreateManagedWidget(get_c_string(function),
                                mwLabelMEObjectClass, menuw,
				XtNlabel, _(lbl),
                                (char *)0);
                XtAddCallback(entry,
                        XtNcallback, execute_callback,
			MwStrdup(get_c_string(function)));
        }
        return NIL;
}

static struct {
	char *label, *sublabel;
	Widget entry, menu;
} *submenus;

static int submenucount = 0;

static Widget find_submenu_by_name(char *label, char *sublabel)
{
        int i;

        for (i = 0; i < submenucount; i++) {
                if (!MwStrcasecmp(submenus[i].label, label)
		    && !MwStrcasecmp(submenus[i].sublabel, sublabel))
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
        submenus = MwRealloc(submenus, (submenucount+1)*(sizeof *submenus));
        submenus[submenucount].label = MwStrdup(get_c_string(label));
        submenus[submenucount].sublabel = sublbl;
        submenus[submenucount].entry =
                XtVaCreateManagedWidget(button_name,
                        mwSubMEObjectClass, menuw,
			XtNmenu_name, sublbl,
                        XtNlabel, _(sublbl),
                        (char *)0);
        submenus[submenucount].menu = XtVaCreatePopupShell(sublbl,
                mwMenuWidgetClass, menuw,
		(char *)0);
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

        if (!strcmp(lbl, "-")) {        /* line pane */
                entry = XtVaCreateManagedWidget("-",
                        mwLineMEObjectClass, menuw,
                        (char *)0);
        } else {
                entry = XtVaCreateManagedWidget(get_c_string(function),
                        mwLabelMEObjectClass, menuw,
                        XtNlabel, _(lbl),
                        (char *)NULL);
                XtAddCallback(entry,
                        XtNcallback, execute_callback,
                        MwStrdup(get_c_string(function)));
        }
        return NIL;
}


static Widget make_toggle(char *cmd, Widget pw, char *pm, char *t)
{
        Widget w;
        Pixmap pm_return;
        Pixel color;

        XtVaGetValues(pw, XtNbackground, &color, (char *)0);

        w = XtVaCreateManagedWidget("toolbar_toggle",
                toggleWidgetClass, pw, (char *)NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);

        XtVaSetValues(w,
                XtNbitmap, pm_return,
		XtNforeground, color,
                (char *)0);
        XtAddCallback(w,
                XtNcallback, execute_callback, (XtPointer)cmd);
	MwTooltipAdd(tooltip, w, _(t));
        return w;
}

static Widget make_command(char *cmd, Widget pw, char *pm, char *t)
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
                XtNcallback, execute_callback, (XtPointer)cmd);
	MwTooltipAdd(tooltip, w, _(t));
        return w;
}

static void make_vsep(Widget pw)
{
	unsigned long bg;
	XtVaGetValues(topbox, XtNbackground, &bg, (char *)0);
	XtVaCreateManagedWidget("vsep",
		labelWidgetClass, pw,
		XtNborderColor, bg,
		(char *)0);
}

/* The toolbar */
static void init_toolbar(void)
{
	frame2 = XtVaCreateManagedWidget("toolbar",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(frame2, frame2, detach_bar, attach_bar);
	bars |= TOOLBAR;
	toolbox = XtVaCreateManagedWidget("frame2",
		mwFrameWidgetClass, frame2,
		XtNgridx, 1,
		XtNshadowWidth, 1,
		(char *)0);
	toolbox = XtVaCreateManagedWidget("toolbox",
		boxWidgetClass, toolbox,
		(char *)0);

	frame3 = XtVaCreateManagedWidget("formatbar",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(frame3, frame3, detach_bar, attach_bar);
	bars |= FORMATBAR;
	formatbox = XtVaCreateManagedWidget("frame3",
		mwFrameWidgetClass, frame3,
		XtNgridx, 1,
		XtNshadowWidth, 1,
		(char *)0);
	formatbox = XtVaCreateManagedWidget("formatbox",
		boxWidgetClass, formatbox,
		(char *)0);

	make_command("(new-buffer)", toolbox, "new.xpm",
		     "Start another instance of Pathetic Writer");
        make_command("(load-buffer)", toolbox, "fld_open.xpm",
		     "Open a Pathetic Writer document");
        make_command("(save-buffer-as)", toolbox, "save.xpm",
		     "Save the contents of the current buffer");
        make_command("(preview)", toolbox, "preview.xpm",
		     "Preview the contents of the current buffer");
        make_command("(print)", toolbox, "printer.xpm",
		     "Print the contents of the current buffer");
	make_vsep(toolbox);
	make_command("(cut_to_clipboard)", toolbox, "cut.xpm",
		     "Cut");
	make_command("(copy_to_clipboard)", toolbox, "copy.xpm",
		     "Copy");
	make_command("(paste_from_clipboard)", toolbox, "paste.xpm",
		     "Paste");
	make_command("(spell-buffer)", toolbox, "spell.xpm",
		     "Spelling checker");
	make_vsep(toolbox);
        make_command("(help-contents)", toolbox, "info.xpm",
		     "Display the PW online documentation");
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
	cmdStrike = make_toggle("(toggle-format \"strike\")",
		formatbox, "strike.xpm", "Strikethrough");
	make_vsep(toolbox);
        cmdHLeft = make_toggle("(change-hadjust HADJ_LEFT)",
                toolbox, "hleft.xpm", "Left adjusted text");
        cmdHCenter = make_toggle("(change-hadjust HADJ_CENTER)",
                toolbox, "hcenter.xpm", "Centered text");
        cmdHRight = make_toggle("(change-hadjust HADJ_RIGHT)",
                toolbox, "hright.xpm", "Right adjusted text");
	cmdHFull = make_toggle("(change-hadjust HADJ_FULL)",
		toolbox, "hfull.xpm", "Full adjusted text");
	make_vsep(formatbox);
        cmdVTop = make_toggle("(toggle-vadj VADJ_TOP)",
                formatbox, "vtop.xpm", "Superscript");
        cmdVBottom = make_toggle("(toggle-vadj VADJ_BOTTOM)",
                formatbox, "vbottom.xpm", "Subscript");
}

static char *combo_sizes[] = {
	"8", "9", "10", "11", "12", "14", "16", "18",
	"20", "22", "24", "26", "28", "36", "48", "72"
};

static char *combo_styles[] = {
	"Default", "Header 1", "Header 2", "Header 3",
	"Header 4", "Header 5", "Header 6", "Address",
	"Title", "Abstract", "Preformatted", "User 1",
	"User 2", "User 3", "User 4", "User 5"
};
static char **combo_fonts, **combo_colors;
int ncombo_fonts, ncombo_styles, ncombo_colors;

static void cb_font(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"family\" \"%s\")", p);
	activate_window(w_list);
}

static void cb_size(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"size\" (* 10 %s))", p);
	activate_window(w_list);
}

static void cb_style(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(change-style \"%s\")", p);
	activate_window(w_list);
}

static void cb_color(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"fg\" \"%s\")", p);
	activate_window(w_list);
}

/* ---
*/
void font_menu(Widget w, char *label, char *cmd)
{
        Widget entry = XtVaCreateManagedWidget(cmd,
                        mwLabelMEObjectClass, w,
                        (char *)0);
        XtAddCallback(entry,
                XtNcallback, execute_callback, cmd);
	MwLabelSet(entry, label);
}

/* ---
*/
void setup_buttons(void)
{
	unsigned long bg;

	if (topbox == None) return;

	XtVaGetValues(topbox,
		XtNbackground, &bg,
		(char *)0);
	combo_fonts = MwFontList(&ncombo_fonts);
        btnFont = XtVaCreateManagedWidget("format_command",
                mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNlabel, "Font",
		XtNcomboTop, topLevel,
		XtNcomboData, combo_fonts,
		XtNcomboNData, ncombo_fonts,
		XtNwidth, 160,
                (char *)0);
	XtAddCallback(btnFont, XtNlistCallback, cb_font, NULL);
	XtAddCallback(btnFont, XtNtextCallback, cb_font, NULL);
	MwTooltipAdd(tooltip, btnFont, _("Change the font family"));

        btnSize = XtVaCreateManagedWidget("format_command",
                mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNlabel, "Size",
		XtNcomboTop, topLevel,
		XtNcomboData, combo_sizes,
		XtNcomboNData, XtNumber(combo_sizes),
		XtNwidth, 40,
                (char *)0);
	XtAddCallback(btnSize, XtNlistCallback, cb_size, NULL);
	XtAddCallback(btnSize, XtNtextCallback, cb_size, NULL);
	MwTooltipAdd(tooltip, btnSize, _("Change the font size"));

        btnStyle = XtVaCreateManagedWidget("format_command",
                mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNlabel, "Style",
		XtNcomboTop, topLevel,
		XtNcomboData, combo_styles,
		XtNcomboNData, XtNumber(combo_styles),
		XtNwidth, 100,
                (char *)0);
	XtAddCallback(btnStyle, XtNlistCallback, cb_style, NULL);
	XtAddCallback(btnStyle, XtNtextCallback, cb_style, NULL);
	MwTooltipAdd(tooltip, btnStyle, _("Change the display style"));

	combo_colors = MwColorList(&ncombo_colors);
        btnColor = XtVaCreateManagedWidget("format_command",
                mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNlabel, "Color",
		XtNcomboTop, topLevel,
		XtNcomboData, combo_colors,
		XtNcomboNData, ncombo_colors,
		XtNwidth, 80,
                (char *)0);
	XtAddCallback(btnColor, XtNlistCallback, cb_color, NULL);
	XtAddCallback(btnColor, XtNtextCallback, cb_color, NULL);
	MwTooltipAdd(tooltip, btnColor, _("Change the color"));
}


static void place_shortcuts(Widget w, XEvent *event,
                String *p, Cardinal *n)
{
        XButtonEvent *bev = (XButtonEvent *)event;
        int col, row;
        int x, y;
	int s;
	sheet *st;

	s = w_list->sht;
	st = w_list->buf->sht;
        x = event->xbutton.x;
        y = event->xbutton.y;
	MwRichtextCoordsToChar((MwRichtextWidget)w_list->ui->grid,
			 &row, &col, x, y);
        XtVaSetValues(shortcuts,
                XtNx, bev->x_root,
                XtNy, bev->y_root,
                (char *)0);
}

static void popup_shortcuts(Widget w, XEvent *event,
		String *p, Cardinal *n)
{
	XButtonEvent *bev = (XButtonEvent *)event;

	activate_window(find_window_by_widget(w));
	if (!XtIsRealized(shortcuts)) XtRealizeWidget(shortcuts);
	XtVaSetValues(shortcuts,
		XtNx, bev->x_root,
		XtNy, bev->y_root,
		(char *)0);
	XtPopupSpringLoaded(shortcuts);
}

static XtActionsRec actions[] =
{
	{"extend_left", extend_left},
	{"extend_right", extend_right},
	{"extend_up", extend_up},
	{"extend_down", extend_down},
	{"backward_char", backward_char},
	{"forward_char", forward_char},
	{"previous_line", previous_line},
	{"next_line", next_line},
	{"cut_to_clipboard", cut_to_clipboard},
	{"copy_to_clipboard", copy_to_clipboard},
	{"paste_from_clipboard", paste_from_clipboard},
        {"grid-button", GridButtonAction},
	{"self-insert-char", SelfInsertChar},
        {"siaghelp", siaghelp_action},
        {"place-shortcuts", place_shortcuts},
	{"popup-shortcuts", popup_shortcuts},
};

static void cb_drag(Widget w, XtPointer client_data, XtPointer call_data)
{                                                                        
        DropPosition *where = (DropPosition *)call_data;
        printf("cb_drag at (%d,%d)\n", where->x, where->y);
}                                                          
 
static void cb_drop(Widget w, XtPointer client_data, XtPointer call_data)
{                                                                        
        int prot = (int)call_data;
        int dnd_type;             
        Atom xdnd_type;
        char *data;    
                   
        switch (prot) {
        case DndDrop:  
                printf("cb_drop received OffiX drop\n");
                XtVaGetValues(w,                        
                        XtNdndType, &dnd_type,
                        XtNdropData, &data,   
                        (char *)0);        
                printf("type = %d, data = '%s'\n", dnd_type, data);
                break;
        case XdndDrop:
                printf("cb_drop received Xdnd drop\n");
                XtVaGetValues(w,
                        XtNxdndType, &xdnd_type,
                        XtNdropData, &data,
                        (char *)0);
                printf("type = '%s', data = '%s'\n",
                        XGetAtomName(XtDisplay(w), xdnd_type), data);
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

        topLevel = XtVaOpenApplication(
                    &app_context,       /* application context */
                    "Pw",               /* application class */
                    options,            /* command line options list */
		    XtNumber(options),
                    argc, argv,        /* command line args */
                    fallback_resources, /* for missing app-defaults file */
		    mwApplicationShellWidgetClass,
                    (char *)0);         /* terminate varargs list */

	drop_types = MwMalloc(3*sizeof *drop_types);
	drop_types[0] = XInternAtom(XtDisplay(topLevel), "text/plain", False);
	drop_types[1] = XInternAtom(XtDisplay(topLevel), "text/uri-list", False);
	drop_types[2] = None;
	XtVaSetValues(topLevel,
		XtNdropTypes, drop_types,
		(char *)0);
	XtAddCallback(topLevel, XtNdragCallback, cb_drag, NULL);
	XtAddCallback(topLevel, XtNdropCallback, cb_drop, NULL);

	MwInitFormat(XtDisplay(topLevel));

	theme_init(XtDisplay(topLevel));

	XSetErrorHandler(MwXErrorHandler);

	XtGetApplicationResources(topLevel, &app_data, resources,
			XtNumber(resources), NULL, 0);
	plugin = app_data.plugin;

        XtAppAddActions(app_context, actions, XtNumber(actions));

	shortcuts = XtVaCreatePopupShell("shortcuts",
                mwMenuWidgetClass, topLevel, (char *)0);
	XtRegisterGrabAction(popup_shortcuts,
		True, ButtonPressMask | ButtonReleaseMask,
		GrabModeAsync, GrabModeAsync);

        topbox = XtCreateManagedWidget("topbox",
                mwRudegridWidgetClass, topLevel, NULL, 0);

        MwHighlightInit(topLevel);
	tooltip = XtVaCreatePopupShell("tooltip",
		mwTooltipWidgetClass, topLevel,
		(char *)0);

        init_menu();
        init_toolbar();
        setup_buttons();
        init_toggle();

        gridpane = XtVaCreateManagedWidget("gridpane",
                panedWidgetClass, topbox,
                XtNallowResize, True,
                (char *)0);

	statusbox = XtVaCreateManagedWidget("statusbox",
		mwRudegridWidgetClass, topbox,
		(char *)0);
        label1 = XtVaCreateManagedWidget("label1",
                labelWidgetClass, statusbox,
                (char *)0);
        label2 = XtVaCreateManagedWidget("label2",
                labelWidgetClass, statusbox,
		(char *)0);
	label3 = XtVaCreateManagedWidget("label3",
		labelWidgetClass, statusbox,
		(char *)0);
}

/* ---
this does probably not belong here in window.c,
   because it doesn't depend on X
*/

static void save_plugin(char *p)
{
        if (*p++ != ' ' || *p == '\0') printf("501 File name missing\n");
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
        if (*p++ != ' ' || *p == '\0') printf("501 File name missing\n");
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
        if (*p++ != ' ' || *p == '\0') printf("501 Command missing\n");
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
	Window win = XtWindow(topLevel);
	printf("250 %lx\n", (unsigned long)win);
}

static void quit_plugin(char *p)
{
        printf("221 Over and out\n");
        execute("(quit-program)");
}

static void prnt_plugin(char *p)
{
        printf("502 Can't print yet\n");
}

static struct {
        char *verb;
        void (*cb)(char *);
} plugin_cmds[] = {
        {"SAVE", save_plugin},
        {"LOAD", load_plugin},
        {"EXEC", exec_plugin},
        {"HELP", help_plugin},
        {"NOOP", noop_plugin},
	{"WIN", win_plugin},
        {"QUIT", quit_plugin},
        {"PRNT", prnt_plugin},
        {NULL, NULL}
};

static void read_plugin_cmd(XtPointer client_data, int *fid, XtInputId *id)
{
        char b[1024], *p;
        int i, n;

        if ((n = read(*fid, b, 1020)) == -1) return;

        b[n] = '\0';
        if ((p = strchr(b, '\n')) == NULL) {
                printf("501 Incomplete command\n");
                return;
        }

        *p = '\0';
        for (i = 0; plugin_cmds[i].verb; i++) {
                if (!strncmp(b, plugin_cmds[i].verb,
                                strlen(plugin_cmds[i].verb)))
                        break;
        }
        if (plugin_cmds[i].verb)
                (*plugin_cmds[i].cb)(b+strlen(plugin_cmds[i].verb));
        else
                printf("500 What are you talking about\n");
        fflush(stdout);
}

/* ---
*/
void mainloop(void)
{
	if (app_data.plugin) {
		XtAppAddInput(XtWidgetToApplicationContext(topLevel),
                        fileno(stdin), (XtPointer)XtInputReadMask,
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
	                                b->sht[s].plugin[n] = b->sht[s].plugin[n+1];
	                        b->change = pr_scr_flag = TRUE;
	                }
	                b = b->next;
	        } while (b != b_list);
	}
}

static MwFmt fmt0 = {"Helvetica", 100, 0, 0, 0, 0, "black", "white", 0, 0, 0, 0};
static MwFmt fmt1 = {"Helvetica", 120, 0, 0, 0, 0, "black", "white", 0, 0, 0, 0};

static void handle_plugin_cmd(char *p)
{
	execute(p);
}

/* ---
   void init_windows(buffer *b)
   Sets up the whole initial window structure and initializes scrupd.
   The window list w_list is set to a list with a single window with the
   buffer b.
*/

void init_windows(buffer *b, int *argc, char **argv)
{
	unsigned long foreground;	/* pixel value */
	unsigned long background;
	Window cell_win;
	XColor screen_color, exact_color;

	start_splash();

	init_windows1(argc, argv);
	interp_startup();

        ok2print = 1;

	do_bp_styles();

	MwEncodeFormat(~0, &fmt0);
	MwEncodeFormat(~0, &fmt1);

	XtRealizeWidget(topLevel);

	plugin_init(topLevel, handle_plugin_exit, handle_plugin_cmd);

	display = XtDisplay(topLevel);

	grid_cursor = XCreateFontCursor(display, XC_xterm);

	activate_window(new_window(b, NULL));

	wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", False);
	XtOverrideTranslations(topLevel,
		XtParseTranslationTable(
			"<Message>WM_PROTOCOLS: execute(quit-program)"));

	XSetWMProtocols(display, XtWindow(topLevel), &wm_delete_window, 1);

	root = DefaultRootWindow(display);

	XAllocNamedColor(display,
			DefaultColormap(display, DefaultScreen(display)),
			"grey", &screen_color, &exact_color);
	blockbg = screen_color.pixel;
	noblockbg = WhitePixelOfScreen(XtScreen(topLevel));

	foreground = BlackPixelOfScreen(XtScreen(topLevel));

	cell_win = XtWindow(w_list->ui->grid);

	XtVaGetValues(w_list->ui->grid,
		XtNbackground, &background, (char *)0);

	draw_status(display, "");

	/* Set up selection */
	target_atom = XInternAtom(display, "PW_BLOCK", False);
	clipboard = XInternAtom(display, "CLIPBOARD", False);

	/* stuff originally in realmain */

        embed_init(topLevel);

        init_subr_1("add-menu", add_menu);
        init_subr_3("add-menu-entry", add_menu_entry);
        init_subr_2("add-submenu", add_submenu);
        init_subr_4("add-submenu-entry", add_submenu_entry);
        init_subr_1("tooltip-mode", ltooltip_mode);
	init_subr_0("cut_to_clipboard", lcut_to_clipboard);
	init_subr_0("copy_to_clipboard", lcopy_to_clipboard);
	init_subr_0("paste_from_clipboard", lpaste_from_clipboard);

        init_form(topLevel, app_context);

        MwSetIcon(topLevel, pw_xpm);

	stop_splash();
}

/* ---
   void exit_windows()
   Cleans up after Calc before exit.  All buffers and windows are freed.
*/

void exit_windows(void)
{
	/* free all buffers */
	while (b_list != NULL)
		free_buffer(b_list);
	while (w_list != NULL)
		free_window(w_list);
	deletia_mark(0);
	deletia_reap();
}


/* ---
   static void pr_scr()
   Prints and refreshes all the windows.
   Sets pr_scr_flag to FALSE.
*/

static void pr_scr(void)
{
	int i;
	window *w;
	buffer *b;
	int s;
	draw_status(display, "");
	w = w_list;
	s = w->sht;
	do {
		/* plugin size fixer-upper */
		for (i = 0; i < w->buf->sht[s].nplugin; i++) {
			int wi, he;
			int r = w->buf->sht[s].plugin[i].row;
			alloc_line(w->buf, s, r);
			plugin_size_get(w->buf->sht[s].plugin[i].ph, &wi, &he);
			w->buf->sht[s].text[r].height = he;
		}
		draw_buffer(display, w);
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
	pr_scr_flag = pr_line_flag = FALSE;
	deletia_reap();
}	/* pr_scr */


/* ---
*/
void get_coords_cell(window *w, int top_row, int top_col,
			int *cur_row, int *cur_col,
			int cur_x, int cur_y)
{
	MwRichtextCoordsToChar((MwRichtextWidget)(w->ui->grid),
		cur_row, cur_col, cur_x, cur_y);
}

/* ---
*/
void show_format(void)
{
	static int last_fmt = -1;
	int f = w_list->current_fmt;
	MwFmt fmt;
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	int sty = ret_style(w_list->buf, s, st[s].point_pos.row);
	static int last_hadj = -1;
	static int last_vadj = -1;
	static int last_sty = -1;
	int hadj, vadj;
	char b[100];

	MwDecodeFormat(f, ~0, &fmt);
	hadj = ret_hadj(w_list->buf, s, st[s].point_pos.row);
	vadj = fmt.vadj;

	/* thou shalt not waste time on things you have already done */
	if (f == last_fmt && hadj == last_hadj && vadj == last_vadj
	    && sty == last_sty && !pr_scr_flag && !pr_line_flag) return;
	last_fmt = f;
	last_hadj = hadj;
	last_vadj = vadj;
	last_sty = sty;

	/* menus */
	/* Can't translate these strings because it interferes
	   with the Combo widgets' operation.
	*/
	MwComboTextChange(btnFont, /*_*/(fmt.family));
	sprintf(b, "%d", fmt.size/10);
	MwComboTextChange(btnSize, b);
	MwComboTextChange(btnStyle, /*_*/(style2name(sty)));
	MwComboTextChange(btnColor, /*_*/(fmt.fg));

	/* toggle buttons */
	MwStateSet(cmdBold, (fmt.bold?1:0), 1, 0);
	MwStateSet(cmdItalic, (fmt.italic?1:0), 1, 0);
	MwStateSet(cmdUline, (fmt.uline?1:0), 1, 0);
	MwStateSet(cmdStrike, (fmt.strike?1:0), 1, 0);
	MwStateSet(cmdHLeft, ((hadj == MW_HADJ_LEFT)?1:0), 1, 0);
	MwStateSet(cmdHCenter, ((hadj == MW_HADJ_CENTER)?1:0), 1, 0);
	MwStateSet(cmdHRight, ((hadj == MW_HADJ_RIGHT)?1:0), 1, 0);
	MwStateSet(cmdHFull, ((hadj == MW_HADJ_FULL)?1:0), 1, 0);
	MwStateSet(cmdVTop, ((vadj == MW_VADJ_TOP)?1:0), 1, 0);
	MwStateSet(cmdVBottom, ((vadj == MW_VADJ_BOTTOM)?1:0), 1, 0);
}

int cursor_visible = FALSE;

/* ---
   void show_cur(window *w)
   Moves the cursor to reflect the position of point in w.
   If point is not visible, the window is moved so that point is in
   the middle of the screen.
*/

void show_cur(window *w)
{
	int s = w->sht;
	sheet *st = w->buf->sht;
	char *p = "";

	XtVaSetValues(w->ui->grid,
		XtNrichtextPointRow, st[s].point_pos.row,
		XtNrichtextPointCol, st[s].point_pos.col,
		XtNrichtextVisibleCursor, True,
		(char *)0);
	if (pr_scr_flag) {
		pr_scr();
	} else {
		if (pr_line_flag) {
			MwRichtextDrawLine(w->ui->grid, st[s].point_pos.row);
			pr_line_flag = FALSE;
		}
	}
	show_format();
	draw_input(display, p);
	cursor_visible = TRUE;
}	/* show_cur */

/* ---
*/
void hide_cur(window *w)
{
	;
}

