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
#include <stdlib.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <Mowitz/Mowitz.h>
#include "../siod/siod.h"
#include "../common/common.h"
#include "../xcommon/embed.h"
#include "../xcommon/xcommon.h"
#include "../siag/calc.h"
#include "xsiag.h"

/*#define DEBUG
*/

/* ---
*/
int select_file(char *path, char *name, char *patterns[], char *fmt, int e)
{
	char extra[1024];
	sprintf(extra, "Home=%s:Examples=%s/siag/examples",
		getenv("HOME"), docdir);
	return MwFileselInput(topLevel, path, name, patterns, fmt, extra, e);
}

/* ---
*/
int alert_box(char *text, char *buttons[], int nbuttons)
{
	return MwAlertBox(topLevel, text, buttons, nbuttons);
}

void about_box(char *text)
{
	MwAboutBox(topLevel, "siag.xpm", text);
}

void about_siag(void)
{
	MwAboutSiag(topLevel);
}

/* ---
*/
int select_from_list(char *text, char *choices[], int nchoices)
{
	return MwListBox(topLevel, text, choices, nchoices);
}

/* ---
*/
void error_box(char *message)
{
	MwErrorBox(topLevel, message);
}

#ifdef HAVE_LIBGUILE
static void execute_guile_action(Widget w, XEvent * event,
                           String * params, Cardinal * num_params)
{
        char b[256];
        int i;

        strcpy(b, "(");
        strncat(b, params[0], 255);
        for (i = 1; i < *num_params; i++) {
                strncat(b, " ", 255);
                strncat(b, params[i], 255);
        }
        strncat(b, ")", 255);

        exec_expr(name2interpreter("guile"), b);
}
#endif	/* GUILE */

#if 0	/* moved to window.c */
static void execute_siod_action(Widget w, XEvent * event,
                           String * params, Cardinal * num_params)
{
        char b[256];
        int i;

        strcpy(b, "(");
        strncat(b, params[0], 255);
        for (i = 1; i < *num_params; i++) {
                strncat(b, " ", 255);
                strncat(b, params[i], 255);
        }
        strncat(b, ")", 255);
        exec_expr(siod_interpreter, b);
}
#endif

#ifdef HAVE_LIBTCL
static void execute_tcl_action(Widget w, XEvent *event,
                           String *params, Cardinal *num_params)
{
        char b[256];
        int i;

        strncpy(b, params[0], 255);
        for (i = 1; i < *num_params; i++) {
                strncat(b, " ", 255);
                strncat(b, params[i], 255);
        }
        exec_expr(name2interpreter("tcl"), b);
}
#endif	/* TCL */

static XtActionsRec actions[] =
{
#ifdef HAVE_LIBGUILE
        {"guile", execute_guile_action},
#endif
#if 0
	{"execute", execute_siod_action},
#endif
#ifdef HAVE_LIBTCL
	{"tcl", execute_tcl_action}
#endif
};

/* a wrapper around XGetGeometry */
/* returns the list (x y width height border_width depth) */
static LISP get_geometry(void)
{
        Window root, cell_win = xwindow_of_window(w_list);
        int x, y;
        unsigned int width, height, border_width, depth;
        LISP result;

        XGetGeometry(XtDisplay(topLevel), cell_win, &root, &x, &y, &width, &height,
                     &border_width, &depth);

        result = cons(flocons(depth), NIL);
        result = cons(flocons(border_width), result);
        result = cons(flocons(height), result);
        result = cons(flocons(width), result);
        result = cons(flocons(y), result);
        result = cons(flocons(x), result);

        return result;
}

static LISP fit_block_width(void)
{
        int r, c, text_width;
        char *p;
	int s;
	MwRichchar *rc;
	buffer *buf = w_list->buf;

        if (block_upper(w_list).row < 1 || block_upper(w_list).col < 1 ||
                block_lower(w_list).row < 1 || block_lower(w_list).col < 1)
                        return NIL;

	s = w_list->sht;
        for (c = block_upper(w_list).col; c <= block_lower(w_list).col; c++) {
                set_width(buf, s, c, 10);
                for (r = block_upper(w_list).row; r <= block_lower(w_list).row; r++) {
			if (r > buf->sht[s].alloc_lines) continue;
                        p = ret_pvalue(NULL, buf, s, r, c, -1);
			rc = MwRcMakerich(p, ret_format(buf, s, r, c));
			text_width = MwRcStrwidth(rc, -1)+10;
                        if (text_width > cell_width(buf, s, c))
                                set_width(buf, s, c, text_width);
			MwFree(rc);
                }
        }
        pr_scr_flag = 1;
        return NIL;
}

static LISP fit_block_height(void)
{
        int r, c, text_height;
        char *p;
	int s;
	MwRichchar *rc;
	buffer *buf = w_list->buf;

        if (block_upper(w_list).row < 1 || block_upper(w_list).col < 1 ||
                block_lower(w_list).row < 1 || block_lower(w_list).col < 1)
                        return NIL;

	s = w_list->sht;
        for (r = block_upper(w_list).row; r <= block_lower(w_list).row; r++) {
                set_height(buf, s, r, 10);
                for (c = block_upper(w_list).col; c <= block_lower(w_list).col; c++) {
			if (c > buf->sht[s].longest_line) continue;
                        p = ret_pvalue(NULL, buf, s, r, c, -1);
			rc = MwRcMakerich(p, ret_format(buf, s, r, c));
			text_height = MwRcStrheight(rc, -1)+10;
                        if (text_height > cell_height(buf, s, r))
                                set_height(buf, s, r, text_height);
			MwFree(rc);
                }
        }
        pr_scr_flag = 1;
        return NIL;
}

static LISP lembed_object(void)
{
        char file[256], *tag;
        char *i;
        unsigned int width, height;
        buffer *buf = buffer_of_window(w_list);
        int row = get_point(w_list).row;
        int col = get_point(w_list).col;
	int s = w_list->sht;
	cval value;
	value.text = NULL;

        if (ret_type(buf, s, row, col) == EMBED) {
                llpr("Can't overwrite embedded object");
                return NIL;
        }

        file[0] = '\0';
        if (!ask_for_str("Object file:", file)) return NIL;

        tag = embed_load(file);
        if (tag == NULL) return NIL;

        i = tag;
        if (!i) return NIL;

        embed_size(tag, &width, &height);
        /* don't change cell width and/or height */

        undo_save(buf, s, row, col, row, col);
        ins_data(buf, siod_interpreter, i, value, EMBED, s, row, col);

        buf->change = TRUE;
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lembed_remove(void)
{
        buffer *buf = buffer_of_window(w_list);
        int row = get_point(w_list).row;
        int col = get_point(w_list).col;
	int s = w_list->sht;
	cval value;
	value.number = 0;

        if (ret_type(buf, s, row, col) == EMBED) {
                undo_save(buf, s, row, col, row, col);
                ins_data(buf, siod_interpreter, NULL, value, EMPTY, s, row, col);
        }
        buf->change = TRUE;
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lembed_open(void)
{
        buffer *buf = buffer_of_window(w_list);
        int row = get_point(w_list).row;
        int col = get_point(w_list).col;
	int s = w_list->sht;

        if (ret_type(buf, s, row, col) == EMBED) {
                embed_open(ret_text(buf, s, row, col));
                embed_load(ret_text(buf, s, row, col));
        }
        buf->change = TRUE;
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lembed_save(void)
{
        char cmd[1024];
        char file[256];
        buffer *buf = buffer_of_window(w_list);
        Pixmap bitmap;

        file[0] = '\0';
        if (!ask_for_str("Object file:", file)) return NIL;

        bitmap = draw_snapshot();
        sprintf(cmd, "siag %s", buf->path);
        embed_save(file, cmd, bitmap);
        XFreePixmap(XtDisplay(topLevel), bitmap);
        return NIL;
}

static LISP copy_block(void)
{
	int s = w_list->bsht;
	int r1 = get_point(w_list).row;
	int c1 = get_point(w_list).col;
	int rows = block_lower(w_list).row-block_upper(w_list).row+1;
	int cols = block_lower(w_list).col-block_upper(w_list).col+1;
	undo_save(w_list->buf, s, r1, c1, r1+rows-1, c1+cols-1);
        /* NULL is a bogus event */
        XtGetSelectionValue(grid_of_window(w_list), XA_PRIMARY, target_atom,
                requestor_callback, NULL, CurrentTime);
        pr_scr_flag = TRUE;
        return NIL;
}

/* ---
FIXME: should set sheet too
*/

static LISP set_block(void)
{
	Atom clipboard = XInternAtom(XtDisplay(topLevel), "CLIPBOARD", False);
	position point = get_point(w_list);
	position mark = get_mark(w_list);
	w_list->bsht = w_list->sht;

        if (point.row < mark.row) {
                set_blku_row(w_list, point.row);
                set_blkl_row(w_list, mark.row);
        } else {
                set_blku_row(w_list, mark.row);
                set_blkl_row(w_list, point.row);
        }
        if (point.col < mark.col) {
                set_blku_col(w_list, point.col);
                set_blkl_col(w_list, mark.col);
        } else {
                set_blku_col(w_list, mark.col);
                set_blkl_col(w_list, point.col);
        }

        /* Become selection owner (CurrentTime is not right, really) */
        if (XtOwnSelection(grid_of_window(w_list), XA_PRIMARY,
                CurrentTime, convert_proc,
                lose_ownership_proc, NULL) == False) {
                XtWarning("Siag: failed to become selection owner\n");
                set_blku_row(w_list, -1); set_blku_col(w_list, -1);
                set_blkl_row(w_list, -1); set_blkl_col(w_list, -1);
        }
        if (XtOwnSelection(grid_of_window(w_list), clipboard,
                CurrentTime, convert_proc,
                lose_ownership_proc, NULL) == False) {
			; /* don't worry, be happy */
        }

        pr_scr_flag = TRUE;
        return NIL;
}

static LISP unset_block(void)
{
	Atom clipboard = XInternAtom(XtDisplay(topLevel), "CLIPBOARD", False);
	position point = get_point(w_list);
	position mark = get_point(w_list);
	w_list->bsht = w_list->sht;

        set_blku_row(w_list, -1); set_blku_col(w_list, -1);
        set_blkl_row(w_list, -1); set_blkl_col(w_list, -1);

	point = get_point(w_list);
	mark = get_point(w_list);

        XtDisownSelection(grid_of_window(w_list), XA_PRIMARY, CurrentTime);
        XtDisownSelection(grid_of_window(w_list), clipboard, CurrentTime);
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lselect_theme(void)
{
	if (select_theme(topLevel)) llpr("Restart to activate theme");
	else llpr("No theme selected");
	return NIL;
}

static LISP ledit_theme(void)
{
	char cmd[1024];
	sprintf(cmd, "cd %s/.siag; xedplus theme &", getenv("HOME"));
	system(cmd);
	return NIL;
}

static LISP ldelete_theme(void)
{
	char b[1024];
	sprintf(b, "%s/.siag/theme", getenv("HOME"));
	remove(b);
	return NIL;
}


/* ---
*/
void interp_startup(void)
{
        XtAppContext app_context = XtWidgetToApplicationContext(topLevel);

        XtAppAddActions(app_context, actions, XtNumber(actions));
	init_subr_0("get-geometry", get_geometry);
	init_subr_0("fit-block-width", fit_block_width);
	init_subr_0("fit-block-height", fit_block_height);
	init_subr_0("embed-object", lembed_object);
	init_subr_0("embed-remove", lembed_remove);
	init_subr_0("embed-open", lembed_open);
	init_subr_0("embed-save", lembed_save);
	init_subr_0("copy-block", copy_block);
	init_subr_0("set-block", set_block);
	init_subr_0("unset-block", unset_block);
	init_subr_0("select_theme", lselect_theme);
	init_subr_0("edit_theme", ledit_theme);
	init_subr_0("delete_theme", ldelete_theme);
}

/* ---
Postscript font handling. Metrics are taken from X
the index is into font_table
--- */

/* ---
this is dead wrong because it doesn't take res_x and res_y into account
*/

int ps_text_width(int index, char *s)
{
	return MwFontWidth(index, s);
}

/* ---
*/
int ps_font_descent(int index)
{
	return MwFontDescent(index);
}

/* ---
*/
int ps_font_height(int font)
{
	return MwFontHeight(font);
}

/* ---
*/
int ps_embed_print(FILE *fp, char *tag, int x_base, int y_base)
{
	return embed_print(fp, tag, x_base, y_base);
}

