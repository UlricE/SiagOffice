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

/* from plugin.c */
extern char **plugin_patterns;
extern int plugin_write(int, char *);
extern int plugin_read(int, char *);
extern int plugin_start(char *);
extern int plugin_stop(int);
extern int plugin_load(int, char *);
extern int plugin_save(int, char *);
extern char *plugin_help(int);
extern int plugin_hide(int);
extern int plugin_print(int, FILE *, int, int, int);
extern int plugin_register(char *, char *, char *);
extern int plugin_size_get(int, int *, int *);
extern int plugin_size_set(int, int, int);

