/*
   Egon Animator
   Copyright (C) 1997  Ulric Eriksson <ulric@siag.nu>

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
   Module name:    selection.c

   This module handles selection: grabbing, releasing, cutting, pasting.

   The selection uses a custom target PW_BLOCK, which is simple
   plaintext. No formatting is preserved.

   All in all, plenty good enough for now.
--- */

#include <stdio.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>
#include <X11/xpm.h>

#include "../common/common.h"
#include <Mowitz/Mowitz.h>
#include "../egon/egon.h"

Atom target_atom;	/* used for selection */

/* ---
*/
int pack_selection(buffer *buf, char *data, int r1, int c1, int r2, int c2)
{
	return 0;
}

/* ---
*/
Boolean convert_proc(Widget w,
	Atom *selection, Atom *target, Atom *type_return,
	XtPointer *value_return, unsigned long *length_return,
	int *format_return)
{
	return False;
}

/* ---
*/
void lose_ownership_proc(Widget w, Atom *selection)
{
	pr_scr_flag = TRUE;	/* Is this enough? */
}

/* ---
*/
void transfer_done_proc(Widget w, Atom *a, Atom *b)
{
	; /* no need to do anything at all */
}

/* ---
*/
int unpack_selection(buffer *buf, char *data, int row, int col)
{
	return 0;
}

/* ---
*/
void requestor_callback(Widget w, XtPointer client_data,
	Atom *selection, Atom *type, XtPointer value,
	unsigned long *length, int *format)
{
	if ((value == NULL) && (*length == 0)) {
		XBell(XtDisplay(w), 100);
		XtWarning("Egon: no selection or selection timed out\n");
	}
	else {
		w_list->buf->change = TRUE;

		XtFree((char *)value);
		pr_scr_flag = TRUE;
	}
}

