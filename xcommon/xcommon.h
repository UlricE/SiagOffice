/*
    Copyright (C) 1996-2001  Ulric Eriksson <ulric@siag.nu>

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
 
extern void start_splash(void);
extern void stop_splash(void);
extern Pixmap load_pixmap(Display *, Pixel, char *);
extern void free_colors(void);
extern int xerror_handler(Display *, XErrorEvent *);
extern void theme_init(Display *);
extern int select_theme(Widget);

