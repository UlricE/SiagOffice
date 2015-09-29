/*
   Siag, Scheme In A Grid
   Copyright (C) 1996, 1997  Ulric Eriksson <ulric@siag.nu>

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

/*
 * xsiag.h
 */

#include "../siag/user_interface.h"

typedef struct siag_ui {
	Widget viewport,	/* really a form */
		selectall,	/* the small button */
		colnum,		/* column numbers */
		rownum,		/* row numbers */
		grid,		/* cells */
		hscroll,	/* horizontal scrollbar */
		vscroll,	/* vertical scrollbar */
		tab,		/* tabbing widget */
		tabl, tabr;	/* to browse tabs */
} siag_ui;

#define xwindow_of_window(w) (XtWindow((w)->ui->grid))
#define grid_of_window(w) ((w)->ui->grid)

/* from window.c */
extern window *find_window_by_widget(Widget);
extern Pixmap draw_snapshot(void);
extern Widget topLevel;
extern int lastc;

/* from selection.c */
extern Atom target_atom;
extern Boolean convert_proc(Widget,
        Atom *, Atom *, Atom *, XtPointer *, unsigned long *, int *);
extern void lose_ownership_proc(Widget, Atom *);
extern void requestor_callback(Widget, XtPointer, Atom *, Atom *,
                                XtPointer, unsigned long *, int *);

/* from forminput.c */
extern void init_form(Widget);

/* from fontsel.c */
extern void font_init(Widget);

/* from plugin.c */
extern int plugin_find_by_widget(Widget);

extern void interp_startup(void);

