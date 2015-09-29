/*
   Siag Office
   Copyright (C) 1997-2001  Ulric Eriksson <ulric@siag.nu>

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

/*
 * forminput.c
 */

#include <stdio.h>
#include <string.h>
#include <X11/keysym.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <Mowitz/Mowitz.h>

#include "../siod/siod.h"
#include "../common/common.h"
#include "xcommon.h"

#define siod_interpreter 0
extern void exec_expr(int, char *);	/* from railway.c */
extern void center(Widget, Widget);	/* from dialogs.c */

#define TEXT 1
#define OKBUTTON 2
#define CANCELBUTTON 3
#define MENU 4
#define CHECK 5

typedef struct s_control {
	int type;
	Widget w;
	char *name;
	struct s_control *next, *prev;
} control;

static control *c_list;

static Widget top;
static XtAppContext app;

static Widget form_shell, form_dialog;
static Widget vert = None, horiz = None, lastw = None, menu = None;

static int status;

static void CBform_done(Widget w, XtPointer client_data, XtPointer call_data)
{
	status = DONE;
}

static void CBform_cancel(Widget w, XtPointer client_data, XtPointer call_data)
{
	status = ABORT;
}

static void CBform_cmd(Widget w, XtPointer client_data, XtPointer call_data)
{
	exec_expr(siod_interpreter, (char *)client_data);
}

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
			XtNleft, XawChainLeft,	/* this is not a bug */
			XtNright, XawChainRight, (char *)0);
		break;
	default:	/* nothing to do */
		break;
	}
	return w;
}

static LISP form_begin(void)
{
	vert = None, horiz = None;
	c_list = NULL;
	form_shell = XtVaCreatePopupShell("form_shell",
		transientShellWidgetClass, top,
		XtNtitle, _("Form"),
		(char *)0);
	form_dialog = XtVaCreateManagedWidget("form_dialog",
		formWidgetClass, form_shell, (char *)0);
	return NIL;
}

static LISP form_label(LISP text)
{
	char *label = get_c_string(text);
	Widget w = lastw = XtVaCreateManagedWidget("form_label",
		labelWidgetClass, form_dialog,
		(char *)0);
	MwLabelSet(w, label);
	horiz = place(w, vert, horiz, LEFT);
	return NIL;
}

static control *new_control(int type, char *name)
{
	control *c = (control *)MwMalloc(sizeof(control));
	if (!c_list) {
		c_list = c;
		c->next = c;
		c->prev = c;
	} else {
		c->prev = c_list->prev;
		c->next = c_list;
		c_list->prev->next = c;
		c_list->prev = c;
	}
	c->type = type;
	c->name = MwStrdup(name);
	return c;
}

static LISP form_check(LISP n, LISP text)
{
	char *name = get_c_string(n);
	char *label = get_c_string(text);
	control *c = new_control(CHECK, name);
	c->w = lastw = XtVaCreateManagedWidget("form_check",
		mwCheckWidgetClass, form_dialog,
		XtNstate, False,
		XtNlabel, label,
		XtNcheckStyle, MwCheckWin,
		XtNwidth, 200,
		XtNheight, 20,
		XtNborderWidth, 0,
		(char *)0);
	MwLabelSet(c->w, label);
	horiz = place(c->w, vert, horiz, LEFT);
	return NIL;
}

static LISP form_text(LISP n)
{
	char *name = get_c_string(n);
	control *c = new_control(TEXT, name);
	c->w = lastw = XtVaCreateManagedWidget(name,
		mwTextfieldWidgetClass, form_dialog,
		XtNdisplayCaret, False, (char *)0);

	XtOverrideTranslations(lastw,
		XtParseTranslationTable(
			"<Key>Return:	form-done()\n\
			<Key>Escape:	form-cancel()\n\
			:<Key>Tab:	form-next()\n\
			Ctrl<Key>n:	form-next()\n\
			Ctrl<Key>p:	form-previous()\n\
			<Btn1Down>:	form-select()"));

	horiz = place(c->w, vert, horiz, RIGHT);
	return NIL;
}

static LISP form_menu(LISP n)
{
	char *name = MwStrdup(get_c_string(n));
	control *c = new_control(MENU, name);

	c->w = lastw = XtVaCreateManagedWidget("form_menu",
		mwMenuButtonWidgetClass, form_dialog,
		XtNmenu_name, name,
		XtNlabel, name,
		XtNheight, 20,
		(char *)0);
	menu = XtVaCreatePopupShell(name,
		mwMenuWidgetClass, form_dialog,
		(char *)0);

	horiz = place(c->w, vert, horiz, RIGHT);
	return NIL;
}

static control *find_menubutton_by_entry(Widget w)
{
	control *c = c_list;
	char *name, *mnu;
	Widget mw = XtParent(w);	/* menu widget */
	name = XtName(mw);		/* menu name */
	if (!c) return NULL;
	do {
		if (XtClass(c->w) == mwMenuButtonWidgetClass) {
			mnu = NULL;
			XtVaGetValues(c->w, XtNmenu_name, &mnu, (char *)0);
			if (mnu && !strcmp(mnu, name)) return c;
		}
		c = c->next;
	} while (c != c_list);
	return NULL;
}

static void menu_select(Widget w, XtPointer client_data, XtPointer call_data)
{
	String p = (String)client_data;
	control *c = find_menubutton_by_entry(w);
	if (!c) {
		MwErrorBox(form_dialog, _("No such menu button"));
		return;
	}
	XtVaSetValues(c->w, XtNlabel, p, (char *)0);
}

static LISP form_menuentry(LISP n)
{
	char *name = MwStrdup(get_c_string(n));
	Widget entry = XtVaCreateManagedWidget(name,
		mwLabelMEObjectClass, menu,
		XtNlabel, name,
		(char *)0);
	XtAddCallback(entry, XtNcallback, menu_select, name);
	return NIL;
}

static control *find_widget(Widget w)
{
	control *c = c_list;
	if (!c) return NULL;
	do {
		if (c->w == w) return c;
		c = c->next;
	} while (c != c_list);
	return NULL;
}

static Widget last_textwidget(void)
{
	control *c = c_list->prev;

	if (!c) return None;
	do {
		if (c->type == TEXT) return c->w;
		c = c->prev;
	} while (c != c_list);
	return None;
}

static Widget first_textwidget(void)
{
	control *c = c_list;

	if (!c) return None;
	do {
		if (c->type == TEXT) return c->w;
		c = c->next;
	} while (c != c_list);
	return None;
}

static Widget previous_textwidget(Widget w)
{
	control *c, *c1 = find_widget(w);
	if (c1 == NULL) return None;
	c = c1->prev;

	do {
		if (c->type == TEXT) return c->w;
		c = c->prev;
	} while (c != c1);

	return None;
}

static Widget next_textwidget(Widget w)
{
	control *c, *c1 = find_widget(w);
	if (c1 == NULL) return None;
	c = c1->next;

	do {
		if (c->type == TEXT) return c->w;
		c = c->next;
	} while (c != c1);

	return None;
}

static void done_action(Widget w, XEvent *event, String *params, Cardinal *n)
{
	status = DONE;
}

static void cancel_action(Widget w, XEvent *event, String *params, Cardinal *n)
{
	status = ABORT;
}

static void set_focus(Widget w)
{
	control *c = c_list;

	if (w == None) return;
	do {
		if (c->type == TEXT)
			XtVaSetValues(c->w, XtNdisplayCaret, False, (char *)0);
		c = c->next;
	} while (c != c_list);
	XtSetKeyboardFocus(form_shell, w);
	XtVaSetValues(w, XtNdisplayCaret, True, (char *)0);
}

static void next_action(Widget w, XEvent *event, String *params, Cardinal *n)
{
	Widget w1 = next_textwidget(w);
	if (w1 == None) w1 = first_textwidget();
	set_focus(w1);
}

static void previous_action(Widget w, XEvent *event, String *params, Cardinal *n)
{
	Widget w1 = previous_textwidget(w);
	if (w1 == None) w1 = last_textwidget();
	set_focus(w1);
}

static void focus_action(Widget w, XEvent *event, String *params, Cardinal *n)
{
	set_focus(w);
}

static Widget add_button(Widget pw, char *name, char *label,
        void (*cb)(Widget, XtPointer, XtPointer), XtPointer data)
{
        Widget w = XtVaCreateManagedWidget(name,
                commandWidgetClass, pw,
                (char *)0);
        MwLabelSet(w, label);
        XtVaSetValues(w, XtNwidth, 80, (char *)0);
        if (cb) XtAddCallback(w, XtNcallback, cb, data);
        return w;
}

static LISP form_okbutton(LISP text)
{
	char *name = get_c_string(text);
	Widget w = lastw = add_button(form_dialog, name, name,
		CBform_done, None);
	horiz = place(w, vert, horiz, LEFT);
	return NIL;
}

static LISP form_cancelbutton(LISP text)
{
	char *name = get_c_string(text);
	Widget w = lastw = add_button(form_dialog, name, name,
		CBform_cancel, None);
	horiz = place(w, vert, horiz, LEFT);
	return NIL;
}

static LISP form_command(LISP text, LISP cmd)
{
	char *name = get_c_string(text);
	char *command = get_c_string(cmd);
	Widget w = lastw = add_button(form_dialog, name, name,
		CBform_cmd, command);
	horiz = place(w, vert, horiz, LEFT);
	return NIL;
}

static LISP form_property(LISP name, LISP value)
{
	char *n = get_c_string(name);
	char *tv;
	int lv;

	if (lastw == None) err("Last widget is None", NIL);

	if (FLONUMP(value)) {
		lv = get_c_long(value);
		XtVaSetValues(lastw, n, lv, (char *)0);
	} else {
		tv = get_c_string(value);
		XtVaSetValues(lastw, n, tv, (char *)0);
	}

	return NIL;
}

static LISP form_newline(void)
{
	vert = horiz;
	horiz = None;
	return NIL;
}

static LISP form_end(void)
{
	control *c;
	LISP result;
	Widget w = first_textwidget();

	XtRealizeWidget(form_shell);
	XtUnrealizeWidget(form_shell);
	MwCenter(form_shell);
	XtPopup(form_shell, XtGrabNonexclusive);

	XWarpPointer(XtDisplay(form_shell), None, XtWindow(form_shell),
		0, 0, 0, 0, 0, 0);
	set_focus(w);
	/* and the loop */
	status = WAITING;
	while (status == WAITING) {
		XEvent event_return;

		XtAppNextEvent(app, &event_return);
		XtDispatchEvent(&event_return);
	}
	XtPopdown(form_shell);
	/* collect values and free resources */
	result = NIL;
	c = c_list;
	if (!c) return NIL;
	do {
		String value;
		Boolean state;
		switch (c->type) {
		case TEXT:
			value = MwTextFieldGetString(c->w);
			break;
		case MENU:
			value = MwLabelGet(c->w);
			break;
		case CHECK:
			XtVaGetValues(c->w, XtNstate, &state, (char *)0);
			value = state ? "1" : "0";
			break;
		default:
			value = "";
		}
		if (!value) value = "";
		result = cons(cons(strcons(strlen(c->name), c->name),
				   strcons(strlen(value), value)),
			      result);
		c = c->next;
	} while (c != c_list);
	XtDestroyWidget(form_shell);
	lastw = None;
	if (status == ABORT) return NIL;
	return result;
}

static XtActionsRec actions[] =
{
	{"form-done", done_action},
	{"form-cancel", cancel_action},
	{"form-next", next_action},
	{"form-previous", previous_action},
	{"form-select", focus_action}
};

/* ---
Initialise the SIOD functions used by the form input code
*/

void init_form(Widget topLevel)
{
	XtAppContext app_context = XtWidgetToApplicationContext(topLevel);

	XtAppAddActions(app_context, actions, XtNumber(actions));

	top = topLevel;
	app = app_context;
	init_subr_0("form-begin", form_begin);
	init_subr_1("form-label", form_label);
	init_subr_1("form-text", form_text);
	init_subr_2("form-check", form_check);
	init_subr_1("form-menu", form_menu);
	init_subr_1("form-menuentry", form_menuentry);
	init_subr_1("form-okbutton", form_okbutton);
	init_subr_1("form-cancelbutton", form_cancelbutton);
	init_subr_2("form-command", form_command);
	init_subr_2("form-property", form_property);
	init_subr_0("form-newline", form_newline);
	init_subr_0("form-end", form_end);
}

