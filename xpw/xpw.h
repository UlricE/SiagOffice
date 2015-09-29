/*
   Pathetic Writer
   Copyright (C) 1997-2000  Ulric Eriksson <ulric@siag.nu>

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
 * xpw.h
 */

#include "../pw/user_interface.h"

typedef struct pw_ui {
        Widget viewport,        /* really a form */
                ruler,		/* a canvas */
                grid,           /* cells */
                hscroll,        /* horizontal scrollbar */
                vscroll,        /* vertical scrollbar */
		tab,		/* tabbing widget */
		tabl, tabr;	/* to browse tabs */
} pw_ui;

#define xwindow_of_window(w) (XtWindow((w)->ui->grid))

/* from window.c */
extern Display *display;
extern Window root;
extern window *find_window_by_widget(Widget);
extern void draw_input(Display *, char *);
extern void draw_status(Display *, char *);
extern void draw_cell(Display *, Window, buffer *, int, int, int, int, window *);
extern void draw_buffer(Display *, window *);
extern Pixmap draw_snapshot(void);
extern void draw_input(Display *, char *);

/* from selection.c */
extern Atom target_atom;
extern Boolean convert_proc(Widget, Atom *, Atom *, Atom *,
                        XtPointer *, unsigned long *, int *);
extern void lose_ownership_proc(Widget, Atom *);
extern void requestor_callback(Widget, XtPointer, Atom *, Atom *,
                        XtPointer, unsigned long *, int *);
extern void transfer_done_proc(Widget, Atom *, Atom *);

/* from input.c */
extern void DialogDoneAction(Widget, XEvent *, String *, Cardinal *);
extern void DialogCancelAction(Widget, XEvent *, String *, Cardinal *);

/* from forminput.c */
extern void init_form(Widget, XtAppContext);

/* from main.c */
extern Widget topLevel, topbox, box, form, label1, label2, gridpane;
extern Widget btnFont, btnSize, btnStyle, btnColor;
extern Widget cmdBold, cmdItalic, cmdUline, cmdHLeft, cmdHCenter, cmdHRight;
extern Widget cmdVTop, cmdVBottom;
extern void vscroll_jump (Widget, XtPointer, XtPointer);
extern void vscroll_scroll (Widget, XtPointer, XtPointer);
extern void hscroll_jump (Widget, XtPointer, XtPointer);
extern void hscroll_scroll (Widget, XtPointer, XtPointer);

/* from fontsel.c */
extern void font_init(Widget);

/* from plugin.c */
extern int plugin_find_by_widget(Widget);

extern void interp_startup(void);

