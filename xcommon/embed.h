/*
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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston,
    MA 02111-1307, USA.
*/

/* embed.h */

#define EMBED_OK 0
#define EMBED_ERR 1

extern int embed_init(Widget);
extern char *embed_load(char *);
extern int embed_unload(char *);
extern int embed_open(char *);
extern int embed_save(char *, char *, Pixmap);
extern int embed_print(FILE *, char *, int, int);
extern int embed_size(char *, unsigned int *, unsigned int *);
extern int embed_draw(Drawable, int, int, char *);

