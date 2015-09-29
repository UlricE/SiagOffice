/*
   Egon Animator
   Copyright (C) 1997-2006  Ulric Eriksson <ulric@siag.nu>

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

typedef struct {
        Boolean plugin;
        Boolean play;
} AppData;

/* Each list has a frame, a title, a viewport and a list */
typedef struct egon_ui {
	Widget viewport,       /* really a rudegrid */
                objf, objt, objv, objl,     /* cast */
                tickf, tickt, tickv, tickl,   /* script */
                propf, propt, propv, propl;   /* properties */
	Widget ani_shell, stage;
	Widget tabbox, tab, tabl, tabr;		/* tabbing stuff */
} egon_ui;


extern Atom target_atom;

/* from selection.c */
extern int pack_selection(buffer *, char *, int, int, int, int);
extern Boolean convert_proc(Widget, Atom *, Atom *, Atom *,
			XtPointer *, unsigned long *, int *);
extern void lose_ownership_proc(Widget, Atom *);
extern void transfer_done_proc(Widget, Atom *, Atom *);
extern int unpack_selection(buffer *, char *, int, int);
extern void requestor_callback(Widget, XtPointer, Atom *, Atom *,
			XtPointer, unsigned long *, int *);

/* from input.c */
extern void DialogDoneAction(Widget, XEvent *, String *, Cardinal *);
extern void DialogCancelAction(Widget, XEvent *, String *, Cardinal *);
extern void init_input(void);
extern int ask_for_str_comp(char *, char *, int (*)(char *));
extern int ask_for_str(char *, char *);

/* from forminput.c */
extern void init_form(Widget);

extern void interp_startup(void);

/* from main.c */
extern Widget topLevel;
extern void stage_init(window *);

