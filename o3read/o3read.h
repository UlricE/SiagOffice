/*
   Copyright (C) 2002  Ulric Eriksson <ulric@siag.nu>

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

typedef struct hnode {
	char *tag;		/* tag name */
	char *text;		/* text */
	struct hnode *parent;	/* parent */
	struct hnode *next;	/* next sibling */
	struct hnode *child;	/* first child */
} hnode;

extern void dump_html(hnode *h, int i);
extern void free_html(hnode *h);
extern hnode *parse_html(int (*getc_cb)(void *), void *);
extern char *get_value(char *, char *, char *);

extern void *crealloc(void *, size_t);
extern void *cmalloc(size_t);
extern char *cstrdup(const char *);
extern int cstrncasecmp(const char *, const char *, size_t);
