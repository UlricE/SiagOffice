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

/* user_interface.h

   These are all the functions that the user interface must implement
   and export. Different user interface are free to implement them
   differently, even as stub functions, but they must all be there.

   This file is included from pw.h and from xpw.h.
*/

extern int select_file(char *, char *, char **, char *, int);
extern int alert_box(char *, char **, int);
extern void about_box(char *);
extern void about_siag(void);
extern int select_from_list(char *, char **, int);
extern void error_box(char *);
extern float zoom;

/* Postscript */
extern char *ps_fontname(int);
extern int ps_text_width(int, char *);
extern int ps_font_descent(int);
extern int ps_font_height(long);
extern int ps_embed_print(FILE *, char *, int, int);
extern int ps_font_size(int);

/* from input.c (now window.h) */
extern int ask_for_str_comp(char *, char *, int (*)(char *));
extern int ask_for_str(char *, char *);
extern int macro_flag;
extern textbuf kbd_macro;

extern spell_state spell_select(char *, char *);

