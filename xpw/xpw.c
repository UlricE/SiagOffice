/*
   Pathetic Writer
   Copyright (C) 1997-2002  Ulric Eriksson <ulric@siag.nu>

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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Scrollbar.h>
#include <Mowitz/Mowitz.h>
#include "../siod/siod.h"
#include "../pw/pw.h"
#include "../common/common.h"
#include "../xcommon/embed.h"
#include "../xcommon/plugin.h"
#include "../xcommon/xcommon.h"
#include "xpw.h"

/* ---
*/
int ask_for_str_comp(char *prompt, char *buffr, int (*comp)(char *))
{
	return MwDialogInput(topLevel, prompt, buffr);
}

static int nocomp(char *b)
{
	return TRUE;
}

/* ---
*/
int ask_for_str(char *prompt, char *buffr)
{
	return ask_for_str_comp(prompt, buffr, nocomp);
}

/* ---
*/
int select_file(char *path, char *name, char *patterns[], char *fmt, int e)
{
	char extra[1024];
	sprintf(extra, "Home=%s:Examples=%s/pw/examples",
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
        MwAboutBox(topLevel, "pw.xpm", text);
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

/* ---
The interpretation of this is changed so that the return value
is the width of the col:th character in row. All places
where cell_width is called must be changed to char_width.

Precondition: col *must* be a legal position within the line
*/

int char_width(buffer *buf, int s, int row, int col)
{
        MwRichchar *line = buf->sht[s].text[row].p;
        int w;

        if (line == NULL) return 0;     /* do not dump core */

	w = MwRcWidth(line[col]);
        return w;
}

/* ---
width of the line up to but not including col
*/

static int line_segment(float *x_base, MwRichchar *line, int length,
			int extra_space, int no_of_blanks, int tabmode,
			int row, int col)
{
	int i;
	float tw;
	MwRichchar c;

	tw = MwRcStrwidth(line, length);
	switch (tabmode) {
	case 'r':
		*x_base -= tw;
		break;
	case 'c':
		*x_base -= tw/2;
		break;
	default:
		break;
	}
	for (i = 0; i < length; i++) {
		float width;
		if (i >= col) return 1;
		c = line[i];
		if (isspace(c.c)) c.c = ' ';
		if (c.c == ' ' && extra_space > 0 && no_of_blanks > 0) {
			float x = extra_space/no_of_blanks;
			*x_base += x;
			extra_space -= x;
			no_of_blanks--;
		}
		width = MwRcWidth(c);
		*x_base += width;
	}
	return 0;
}

int line_width(buffer *buf, int s, int row, int col)
{
	int lm = buf->left_margin;
	int rm = buf->right_margin;
	int pw = buf->paper_width;
	float tw;
	int nb, n;
	int width;
	float x_base = lm;
	MwRichchar *line, c;
	int ss, nt, hadj, i;
	MwTabstop mt, *tt = MwGetTabs(buf->sht[s].tabs);

        if (row > buf->sht[s].used_lines) return 0;
	line = buf->sht[s].text[row].p;
	if (MwRcStrlen(line) == 0) return 0;
        if (line == NULL) return 0;     /* do not dump core */

        if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
        	char *p;
                if (col == 0) return 0;
        	p = (char *)MwRcMakeplain(line);
                embed_size(p, &width, NULL);
        	MwFree(p);
                return width;
        }
	hadj = ret_hadj(buf, s, row);
	if (hadj == MW_HADJ_CENTER) {
		x_base = lm+(pw-lm-rm)/2;
		line_segment(&x_base, line, MwRcStrlen(line), 0, 0, 'c', row, col);
	} else if (hadj == MW_HADJ_RIGHT) {
		x_base = pw-rm;
		line_segment(&x_base, line, MwRcStrlen(line), 0, 0, 'r', row, col);
	} else {
		mt.x = 0;
		mt.j = 'l';
		ss = nt = 0;
		while (line[nt].c && line[nt].c != '\t') nt++;
		while (line[nt].c == '\t') {
			n = line_segment(&x_base, line, MwRcStrlen(line),
					0, 0, mt.j, row, col);
			if (n) goto Done;
			mt = MwNextTab(tt, x_base-lm);
			x_base = mt.x+lm;
			ss = ++nt;
			while (line[nt].c && line[nt].c != '\t') nt++;
		}
		tw = 0;
		nb = 0;
		if (hadj == MW_HADJ_FULL &&!ret_bop(buf, s, row+1)) {
			for (i = 0; line[i].c; i++) {
				c = line[i];
				if (isspace(c.c)) nb++;
				tw += MwRcWidth(c);
			}
			tw = pw-lm-rm-tw;
		}

		line_segment(&x_base, line+ss, nt-ss, tw, nb, mt.j, row, col);
	}
Done:
	MwFree(tt);
	return x_base-lm;
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
	{"execute", execute_siod_action},
#ifdef HAVE_LIBTCL
	{"tcl", execute_tcl_action}
#endif
};

static LISP set_block(void)
{
        position p = get_point(w_list);
        position m = get_mark(w_list);
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;

        if (p.row < m.row) {
                st[s].blkl.row = p.row;
                st[s].blkl.row = m.row;
        } else {
                st[s].blku.row = m.row;
                st[s].blkl.row = p.row;
        }
        if (p.col < m.col) {
                st[s].blku.col = p.col;
                st[s].blkl.col = m.col;
        } else {
                st[s].blku.col = m.col;
                st[s].blkl.col = p.col;
        }

        if (XtOwnSelection(w_list->ui->grid, XA_PRIMARY,
			CurrentTime, convert_proc,
                lose_ownership_proc, transfer_done_proc) == False) {
                        XtWarning("Siag: failed to become selection owner\n");
                        st[s].blku.row = st[s].blku.col = -1;
                        st[s].blkl.row = st[s].blkl.col = -1;
        }

        pr_scr_flag = TRUE;
        return NIL;
}

static LISP bell(void)
{
        XBell(display, 100);
        return NIL;
}

static LISP lembed_object(void)
{
        char file[256], *tag;
        unsigned int width, height;
        buffer *buf = w_list->buf;
	int s = w_list->sht;
	sheet *st = buf->sht;
        int row = st[s].point_pos.row;

        if (ret_style(buf, s, row) == MW_STY_EMBED) {
                llpr("Can't overwrite embedded object");
                return NIL;
        }

        file[0] = '\0';
        if (!ask_for_str("Object file:", file)) return NIL;

        tag = embed_load(file);
        if (tag == NULL) return NIL;

        embed_size(tag, &width, &height);
        buf->sht[s].text[row].height = height;
        ins_text(buf, s, make_position(row, 0), (unsigned char *)tag,
                        0);
        buf->sht[s].text[row].sty = MW_STY_EMBED;
        w_list->buf->change = TRUE;
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lembed_remove(void)
{
        buffer *buf = w_list->buf;
	int s = w_list->sht;
	sheet *st = buf->sht;
        int row = st[s].point_pos.row;
        if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
                st[s].point_pos.col = 0;
                del_char(buf, s, row, 0);
        }
        w_list->buf->change = TRUE;
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lembed_open(void)
{
        buffer *buf = w_list->buf;
	int s = w_list->sht;
	sheet *st = buf->sht;
        int row = st[s].point_pos.row;
        if (buf->sht[s].text[row].sty == MW_STY_EMBED) {
                char *p = (char *)MwRcMakeplain(buf->sht[s].text[row].p);
                embed_open(p);
                embed_load(p);
                MwFree(p);
        }
        w_list->buf->change = TRUE;
        pr_scr_flag = TRUE;
        return NIL;
}

static LISP lembed_save(void)
{
        char cmd[1024];
        char file[256];
        buffer *buf = w_list->buf;
        Pixmap bitmap;

        file[0] = '\0';
        if (!ask_for_str("Object file:", file)) return NIL;

        bitmap = draw_snapshot();
        sprintf(cmd, "pw %s", buf->path);
        embed_save(file, cmd, bitmap);
        XFreePixmap(display, bitmap);
        return NIL;
}

static LISP lscroll_up(void)
{
	int top_row, th;
	Dimension height;
	XtVaGetValues(w_list->ui->grid,
		XtNheight, &height,
		XtNrichtextTopRow, &top_row,
		(char *)0);
	th = 20*line_last_used(w_list->buf, w_list->sht);

	top_row -= 0.9*height;
	if (top_row > th) top_row = th;
        if (top_row < 0) top_row = 0;
	XtVaSetValues(w_list->ui->grid,
                XtNrichtextTopRow, top_row,
                (char *)0);
	XawScrollbarSetThumb(w_list->ui->vscroll, (double)top_row/th, 0.0);
	return NIL;
}

static LISP lscroll_down(void)
{
	int top_row, th;
	Dimension height;
	XtVaGetValues(w_list->ui->grid,
		XtNheight, &height,
		XtNrichtextTopRow, &top_row,
		(char *)0);
	th = 20*line_last_used(w_list->buf, w_list->sht);

	top_row += 0.9*height;
	if (top_row > th) top_row = th;
        if (top_row < 0) top_row = 0;
	XtVaSetValues(w_list->ui->grid,
                XtNrichtextTopRow, top_row,
                (char *)0);
	XawScrollbarSetThumb(w_list->ui->vscroll, (double)top_row/th, 0.0);
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

static LISP ledit_tabs(void)
{
	char prompt[1024], tabs[1024];
	buffer *buf = w_list->buf;
	int s = w_list->sht;

	sprintf(prompt, "%s:", _("Tabs"));
	strcpy(tabs, buf->sht[s].tabs);

	if (ask_for_str(prompt, tabs)) {
		MwFree(buf->sht[s].tabs);
		buf->sht[s].tabs = MwStrdup(tabs);
	}
	activate_window(w_list);
	return NIL;
}


/* ---
*/
void interp_startup(void)
{
        XtAppContext app_context = XtWidgetToApplicationContext(topLevel);

        XtAppAddActions(app_context, actions, XtNumber(actions));
	init_subr_0("set-block", set_block);
	init_subr_0("bell", bell);
	init_subr_0("embed-object", lembed_object);
	init_subr_0("embed-remove", lembed_remove);
	init_subr_0("embed-open", lembed_open);
	init_subr_0("embed-save", lembed_save);
	init_subr_0("scroll-up", lscroll_up);
	init_subr_0("scroll-down", lscroll_down);
	init_subr_0("edit_tabs", ledit_tabs);
	init_subr_0("select_theme", lselect_theme);
        init_subr_0("edit_theme", ledit_theme);
        init_subr_0("delete_theme", ldelete_theme);
}

/* ---
Postscript font handling. Metrics are taken from afm files in common/fonts
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
int ps_font_height(long font)
{
	return MwFontHeight(font);
}

/* ---
*/
int ps_embed_print(FILE *fp, char *tag, int x_base, int y_base)
{
	return embed_print(fp, tag, x_base, y_base);
}


/* ---
	A dialog for spelling checkers. This should probably be in xcommon.

	The dialog has the following components:

	A label with the word "Original:"
	A text widget (non-editable) with the incorrect word
	A label with the word "Replacement:"
	A text widget (editable) with the replacement word
	A command button with the word "Replace"
	A command button with the word "Accept"
	A command button with the word "Insert"
	A command button with the word "Skip"
	A command button with the word "Cancel"
	A command button with the word "Help"

	The dialog terminates when any of the buttons other than Help
	is pressed. Help pops up Siaghelp with the file pw/spell.html.

	The function accepts these parameters:

	A text buffer with the original word
	A text buffer for the replacement word. This must be large enough.

	The replacement buffer may already contain a suggested word,
	so it must not be cleared by the dialog.

	The function returns the number of the button pressed, with
	1 for Replace and so on.

	Lots of code swiped from forminput.c, which is where this
	really belongs.
--- */

spell_state state;

static Widget vert = None, horiz = None;

#define ANYWHERE 0
#define LEFT 1
#define RIGHT 2

static Widget place(Widget w, Widget vert, Widget horiz, int chain)
{
        if (vert != None)
                XtVaSetValues(w,
                        XtNfromVert, vert, (char *)0);
        if (horiz != None)
                XtVaSetValues(w,
                        XtNfromHoriz, horiz, (char *)0);
        switch (chain) {
        case LEFT:
                XtVaSetValues(w,
                        XtNtop, XawChainTop,
                        XtNbottom, XawChainTop,
                        XtNleft, XawChainLeft,
                        XtNright, XawChainLeft, (char *)0);
                break;
        case RIGHT:
                XtVaSetValues(w,
                        XtNtop, XawChainTop,
                        XtNbottom, XawChainTop,
                        XtNleft, XawChainLeft,  /* this is not a bug */
                        XtNright, XawChainRight, (char *)0);
                break;
        default:        /* nothing to do */
                break;
        }
        return w;
}

static void spell_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	spell_state data = (spell_state)client_data;

	if (data == SPELL_HELP) {
		char b[1256];

		sprintf(b, "siagrun help file:%s/pw/spell.html", docdir);
		MwSpawn(b);
	} else {
		state = data;
	}
}

static Widget new_label(Widget pw, char *name, char *text)
{
	Widget w = XtVaCreateManagedWidget(name,
		labelWidgetClass, pw,
		XtNwidth, 100,
		(char *)0);
	horiz = place(w, vert, horiz, LEFT);
	MwLabelSet(w, text);
	return w;
}

static Widget new_textw(Widget pw, char *name, char *text)
{
	Widget w = XtVaCreateManagedWidget(name,
		asciiTextWidgetClass, pw,
		XtNwidth, 400,
		XtNeditType, XawtextEdit,
		XtNdisplayCaret, False,
		XtNstring, _(text),
		(char *)0);
	horiz = place(w, vert, horiz, RIGHT);
	return w;
}

static Widget new_command(Widget pw, char *name, char *text, int data)
{
	Widget w = XtVaCreateManagedWidget(name,
		commandWidgetClass, pw,
		XtNwidth, 80,
		XtNheight, 20,
		(char *)0);
	horiz = place(w, vert, horiz, LEFT);
	XtAddCallback(w, XtNcallback, spell_cb, (XtPointer)data);
	MwLabelSet(w, text);
	return w;
}

static void new_line(void)
{
	vert = horiz;
	horiz = None;
}

static void wm_del(Widget w)
{
	Atom da;
	if (XtWindow(w)) {
		da = XInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
		XSetWMProtocols(XtDisplay(w), XtWindow(w), &da, 1);
	}
}

extern void center(Widget, Widget);

/* ---
*/
spell_state spell_select(char *original, char *replacement)
{
	Widget spell_shell, spell_form, spell_label1, spell_text1,
		spell_label2, spell_text2,
		spell_replace, spell_accept, spell_insert,
		spell_skip, spell_cancel, spell_help;
	String string;
	state = SPELL_WAIT;

	vert = None, horiz = None;
	spell_shell = XtVaCreateManagedWidget("spell_shell",
		transientShellWidgetClass, topLevel,
		XtNtitle, "Replacement",
		XtNwidth, 520,
		XtNheight, 80,
		(char *)0);
	spell_form = XtVaCreateManagedWidget("spell_form",
		formWidgetClass, spell_shell, (char *)0);
	spell_label1 = new_label(spell_form, "spell_label", "Original:");
	spell_text1 = new_textw(spell_form, "spell_text1", original);
	new_line();
	spell_label2 = new_label(spell_form, "spell_label2", "Replacement:");
	spell_text2 = new_textw(spell_form, "spell_text2", replacement);
	new_line();
	spell_replace = new_command(spell_form, "spell_replace",
		"Replace", SPELL_REPLACE);
	spell_accept = new_command(spell_form, "spell_accept",
		"Accept", SPELL_ACCEPT);
	spell_insert = new_command(spell_form, "spell_insert",
		"Insert", SPELL_INSERT);
	spell_skip = new_command(spell_form, "spell_skip",
		"Skip", SPELL_SKIP);
	spell_cancel = new_command(spell_form, "spell_cancel",
		"Cancel", SPELL_CANCEL);
	spell_help = new_command(spell_form, "spell_help",
		"Help", SPELL_HELP);

	MwCenter(spell_shell);
	XtPopup(spell_shell, XtGrabNonexclusive);
	wm_del(spell_shell);

	XtSetKeyboardFocus(spell_shell, spell_text2);

	while (state == SPELL_WAIT) {
		XEvent event_return;

		XtAppNextEvent(XtWidgetToApplicationContext(spell_shell),
				&event_return);
		XtDispatchEvent(&event_return);
	}
	XtVaGetValues(spell_text2,
		XtNstring, &string, (char *)0);
	strcpy(replacement, string);
	XtPopdown(spell_shell);
	XtDestroyWidget(spell_shell);
	return state;
}

