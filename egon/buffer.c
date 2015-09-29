/*
   Egon Animator
   Copyright (C) 1997-2002  Ulric Eriksson <ulric@siag.nu>

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
 * buffer.c
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>

#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

/* ---
   Module name: buffer.c

   This module handles creating and deleting buffers.  It also contains
   a function for finding a certain buffer, given its name.
--- */

char *paper_name = "A4";
int paper_height = 842;
int paper_width = 595;
int left_margin = 72, right_margin = 72, top_margin = 72, bottom_margin = 72;
int header_margin = 36, footer_margin = 36;
char *header = "&n";	/* buffer name */
char *footer = "&p";	/* page number */
int header_on_first = 1;
int first_page_number = 1;
int orientation = LANDSCAPE;
int tab_distance = 36;

buffer *b_list;

static int unique_name(char *p)
{
	buffer *b = b_list;

	if (b) {
		do {
			b = b->next;
			if (!strcmp(p, b->name)) return 0;
		} while (b != b_list);
	}
	return 1;
}

/* ---
Create a unique name from a file name.
*/

char *buffer_name(char *fn)
{
	static char name[256];
	int i;

	char *p = strrchr(fn, '/');
	if (p != NULL) p++;
	if (p == NULL || *p == '\0') p = fn;
	if (unique_name(p)) {
		strcpy(name, p);
		return name;
	}
	for (i = 2; i < 100; i++) {
		sprintf(name, "%s<%d>", p, i);
		if (unique_name(name))
			return name;
	}
	return name;	/* unlikely and no problem */
}

sheet new_sheet(void)
{
	sheet s;
	char *p = "Empty Sheet";
	s.name = MwStrdup(p);
	s.cast = NULL;
	s.delta = 100;
	s.duration = 1000;
	s.bg = NULL;
	s.bgrad = MwStrdup("100 100 128 0 1 black blue");
	s.now = 0;
	s.plugin = NULL;
	s.nplugin = 0;
	return s;
}

static int sheet_name_taken(buffer *b, char *n)
{
	int i;

	for (i = 0; i < b->nsht; i++)
		if (!strcmp(b->sht[i].name, n)) return 1;
	return 0;
}

int buffer_add_sheet(buffer *b, int n)
{
	int i;
	char name[1000];

	if (n < 0 || n > b->nsht) return b->nsht;

	b->nsht++;
	b->sht = (sheet *)MwRealloc(b->sht, b->nsht*sizeof(sheet));
	for (i = b->nsht-1; i > n; i--) {
		b->sht[i] = b->sht[i-1];
	}
	b->sht[n] = new_sheet();
	MwFree(b->sht[n].name);

	for (i = b->nsht; i < 1000; i++) {
		sprintf(name, _("Sheet %d"), i);
		if (!sheet_name_taken(b, name)) break;
	}
	b->sht[n].name = MwStrdup(name);
	return b->nsht;
}

int buffer_remove_sheet(buffer *b, int n)
{
	int i;

	if (n < 0 || n >= b->nsht || b->nsht <= 1) return b->nsht;
	b->nsht--;
	for (i = n; i < b->nsht; i++) {
		b->sht[i] = b->sht[i+1];
	}
	return b->nsht;
}

void buffer_rename_sheet(buffer *b, int n, char *name)
{
	int i;
	char nn[1024];

	if (n < 0 || n > b->nsht) return;
	if (!strcmp(name, b->sht[n].name)) return;
	strcpy(nn, name);
	i = 1;
	while (sheet_name_taken(b, nn)) {
		sprintf(nn, "%s<%d>", name, i++);
	}
	MwFree(b->sht[n].name);
	b->sht[n].name = MwStrdup(nn);
}

/* ---
   Creates a new buffer with name as name.  It is the responsability of
   the caller to make sure that the name is unique.  The new buffer is
   inserted in the buffer list.
*/

buffer *new_buffer(char *name, char *path)
{
	buffer *b;

	b = (buffer *)MwMalloc(sizeof(buffer));

	if (b_list == NULL)
		b_list = b;	/* first buffer ever */
	else
		b->next = b_list->next;		/* add to list */
	b_list->next = b;

	b->sht = NULL;
	b->nsht = 0;
	buffer_add_sheet(b, 0);
	strncpy(b->name, name, 1000);
	strncpy(b->path, path, 1000);
	b->width = 600;
	b->height = 400;
	b->change = FALSE;
	b->state = MW_ANI_STOP;
	b->paper_name = paper_name;
	b->paper_width = paper_width;
	b->paper_height = paper_height;
	b->top_margin = top_margin;
	b->bottom_margin = bottom_margin;
	b->left_margin = left_margin;
	b->right_margin = right_margin;
	b->header_margin = header_margin;
	b->footer_margin = footer_margin;
	b->header = header;
	b->footer = footer;
	b->header_on_first = header_on_first;
	b->first_page_number = first_page_number;
	b->orientation = orientation;
	return b;
}

/* ---
   buffer *free_buffer(buffer *b)
   Removes b from the buffer list and frees the memory.
   If b_list points to b, it is set to point to the next buffer.
   If the buffer list is empty, b_list is set to NULL.
   Returns the next buffer in the buffer list, or NULL if it is empty.
*/

buffer *free_buffer(buffer *b)
{
	buffer *pb, *next;
	char p[1024];

	next = b->next;
	if (next == b)
		next = NULL;

	/* unlink from buffer list */
	for (pb = b_list;
	     pb->next != b && pb->next != pb;
	     pb = pb->next);
	pb->next = b->next;

	/* make sure b_list does not point to a deleted buffer */
	if (b_list == b)
		b_list = b_list->next;

	/* no buffers in the list => b_list = NULL */
	if (b_list == b)
		b_list = NULL;

	plugin_basedir(p, b->name);
	rmdir(p);

	MwFree((char *) b);

	return next;
}

static int plugin_name_taken(char *name)
{
        buffer *b = b_list;
	int s = 0;
        int i;

        do {
		for (s = 0; s < b->nsht; s++) {
	                for (i = 0; i < b->sht[s].nplugin; i++) {
	                        if (!strcmp(b->sht[s].plugin[i].name, name))
					return 1;
			}
		}
                b = b->next;
        } while (b != b_list);
        return 0;
}

/* ---
make a unique plugin name
*/

void plugin_unique_name(char *from, char *to)
{
        int i;

        strcpy(to, from);
        for (i = 2; i < 100 && plugin_name_taken(to); i++)
                sprintf(to, "%d_%s", i, from);
}


/* ---
   Returns the buffer with the name name, or NULL if it can't be found.
*/

buffer *find_buffer_by_name(char *name)
{
	buffer *b;
	int s;

	if ((b = b_list) == NULL)
		return NULL;

	do {
		if (!strcmp(name, b->name))
			return b;
		for (s = 0; s < b->nsht; s++) {
			if (!strcmp(name, b->sht[s].name)) return b;
		}
		b = b->next;
	} while (b != b_list);

	return NULL;
}

buffer *find_sheet_by_name(char *name, buffer *defb, int *s)
{
	buffer *b;
	char bname[1000], sname[1000];
	char *p;

	if (b_list == NULL) return NULL;

	p = strchr(name, ':');
	if (!p) {
		b = defb;
		strncpy(sname, name, 999);
		sname[999] = '\0';
	} else {
		size_t n = p-name;
		if (n == 0) {
			b = defb;
		} else {
			if (n > 999) n = 999;
			strncpy(bname, name, n);
			bname[n] = '\0';
			b = find_buffer_by_name(bname);
		}
		strncpy(sname, p+1, 999);
		sname[999] = '\0';
	}
	if (!b) return NULL;

	if (sname[0] == '\0') {
		*s = 0;
		return b;
	}
	for (*s = 0; *s < b->nsht; (*s)++) {
		if (!strcmp(sname, b->sht[*s].name)) return b;
	}
	return NULL;
}


/* ---
Get the plugin's index in this buffer.
*/

int buffer_plugin2index(buffer *buf, int ph)
{
        int n;
	int sht = w_list->sht;

        if (!buf) return -1;

        for (n = 0; n < buf->sht[sht].nplugin; n++)
                if (ph == buf->sht[sht].plugin[n].ph) return n;

        return -1;
}

