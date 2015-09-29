/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2002  Ulric Eriksson <ulric@siag.nu>

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

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>
#include "../common/plugin.h"
#include "calc.h"

/* ---
   Module name: buffer.c

   This module handles creating and deleting buffers.  It also contains
   a function for finding a certain buffer, given its name.
--- */

buffer *b_list;

/* ---
Global defaults
--- */
char *paper_name = "A4";
int paper_height = 842;
int paper_width = 595;
int top_margin = 72;
int left_margin = 72;
int right_margin = 72;
int bottom_margin = 72;
int header_margin = 36;
int footer_margin = 36;
int orientation = PORTRAIT;
int respect_protection = 0;		/* don't respect protection */
char *header = "&n";
char *footer = "&p";
int header_on_first = 1;
int first_page_number = 1;

static int unique_name(char *p)
{
	buffer *b = b_list;

	do {
		b = b->next;
		if (!strcmp(p, b->name)) return 0;
	} while (b != b_list);
	return 1;
}

/* ---
*/
char *buffer_name(char *fn)
{
	static char name[256];
	int i;

	char *p = strrchr(fn, '/');
	if (p != NULL) p++;
	if (p == NULL || *p == '\0') p = fn;
	if (unique_name(p)) {
		strncpy(name, p, 255);
		return name;
	}
	for (i = 2; i < 100; i++) {
		sprintf(name, "%s<%d>", p, i);
		if (unique_name(name))
			return name;
	}
	return name;	/* unlikely and no problem */
}


/* ---
Add string property to a buffer.
   If value is NULL, remove property.
   Returns value if successful, otherwise NULL.
*/

char *put_property(buffer *b, char *key, char *value)
{
	property_list *p, *q;

	if ((!b) || (!key)) return NULL;

	/* remove old value, if there is one */
	for (p = b->p_list; p; p = p->next) {
		if (!strcmp(key, p->key)) break;
	}
	if (p) {
		if (p == b->p_list) {
			b->p_list = p->next;
		} else {
			for (q = b->p_list; q->next != p; q = q->next);
			q->next = p->next;
		}
		MwFree(p->key);
		MwFree(p->value);
		MwFree(p);
	}
	/* add new value, if there is one */
	if (value) {
		p = (property_list *)MwMalloc(sizeof(property_list));
		p->key = MwStrdup(key);
		p->value = MwStrdup(value);
		p->next = b->p_list;
		b->p_list = p;
		return p->value;
	}
	return NULL;
}

/* ---
*/
char *get_property(buffer *b, char *key)
{
	property_list *p;

	if ((!b) || (!key)) return NULL;

	for (p = b->p_list; p; p = p->next) {
		if (!strcmp(p->key, key)) return p->value;
	}
	return NULL;
}

/* ---
*/
sheet new_sheet(void)
{
	sheet s;
	char *p = "Empty Sheet";
	s.name = MwStrdup(p);
	s.alloc_lines = 0;
	s.longest_line = 0;
	s.alloc_cols = NULL;
	s.used_lines = 0;
	s.used_cols = 0;
	s.height = NULL;
	s.width = NULL;
	s.mark_pos = P_MIN;
	s.point_pos = s.top = s.prot = s.mark_pos;
	s.blku = make_position(2, 2);
	s.blkl = make_position(1, 1);
	s.matrix = NULL;
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

/* ---
*/
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
	/* this breaks when a single document has more than 1000 sheets,
	   and even then it will still work, sort of.
	*/
	for (i = b->nsht; i < 1000; i++) {
		sprintf(name, _("Sheet %d"), i);
		if (!sheet_name_taken(b, name)) break;
	}
	MwFree(b->sht[n].name);
	b->sht[n].name = MwStrdup(name);
	return b->nsht;
}

/* ---
*/
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

/* ---
*/
void buffer_rename_sheet(buffer *b, int n, char *name)
{
	int i;
	char nn[1024];

	if (n < 0 || n > b->nsht) return;		/* out of bounds */
	if (!strcmp(name, b->sht[n].name)) return;	/* no change */
	strcpy(nn, name);
	i = 1;
	while (sheet_name_taken(b, nn)) {
		sprintf(nn, "%s<%d>", name, i++);
	}
	MwFree(b->sht[n].name);
	b->sht[n].name = MwStrdup(nn);
}

#define NSHT 1

/* ---
   buffer *new_buffer(char *name, char *path)
   Creates a new buffer with name as name.  It is the responsability of
   the caller to make sure that the name is unique.  The new buffer is
   inserted in the buffer list.
*/

buffer *new_buffer(char *name, char *path)
{
	buffer *b;
	int s;

	b = (buffer *)MwMalloc(sizeof(buffer));

	if (b_list == NULL)
		b_list = b;	/* first buffer ever */
	else
		b->next = b_list->next;		/* add to list */
	b_list->next = b;

	strncpy(b->name, name, 1000);
	strncpy(b->path, path, 1000);
	b->change = FALSE;
	b->recalc = FALSE;
	b->sh = 20;
	b->sw = 80;
	b->sf = 0;
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
	b->respect_protection = respect_protection;
	b->a1_refs = a1_refs;
	b->p_list = NULL;
	b->nsht = 0;
	b->sht = NULL;
	for (s = 0; s < NSHT; s++) {
		buffer_add_sheet(b, s);
	}
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
	int i;
	buffer *pb, *next;
	int s;
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

	for (s = 0; s < b->nsht; s++) {
		/* stop any plugins */
		for (i = 0; i < b->sht[s].nplugin; i++) {
			plugin_stop(b->sht[s].plugin[i].ph);
			b->sht[s].plugin[i].displayed = 0;
		}
	}

	plugin_basedir(p, b->name);
	rmdir(p);

	MwFree((char *) b);

	return next;
}

static int plugin_name_taken(char *name)
{
	buffer *b = b_list;
	int i;
	int s;

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
   buffer *find_buffer_by_name(char *name)
   Returns the buffer with the name name, or NULL if it can't be found.
 note: this allows buffer names with ":sheet" appended.
*/

buffer *find_buffer_by_name(char *name)
{
	buffer *b;
	int s;

	if ((b = b_list) == NULL)
		return NULL;

	do {
		/* start with the buffer name */
		if (!strcmp(name, b->name)) {
			return b;
		}
		/* then each sheet */
		for (s = 0; s < b->nsht; s++) {
			if (!strcmp(name, b->sht[s].name))
				return b;
		}
		b = b->next;
	} while (b != b_list);

	return NULL;
}

/* ---
Format of name:

bname:sname	returns buffer called bname, sheet called sname
:sname		returns buffer defb, sheet called sname
sname		returns buffer defb, sheet called sname
bname:		returns buffer called bname, sheet 0
:		returns buffer defb, sheet 0
*/

buffer *find_sheet_by_name(char *name, buffer *defb, int *s)
{
	buffer *b;
	char bname[1000], sname[1000];
	char *p;

	if (b_list == NULL)
		return NULL;

	p = strchr(name, ':');
	if (!p) {	/* no buffer name */
		b = defb;
		strncpy(sname, name, 999);
		sname[999] = '\0';
	} else {
		size_t n = p-name;
		if (n == 0) {	/* no buffer name */
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

	if (sname[0] == '\0') {	/* no sheet name */
		*s = 0;
		return b;
	}

	/* each sheet */
	for (*s = 0; *s < b->nsht; (*s)++) {
		if (!strcmp(sname, b->sht[*s].name))
			return b;
	}

	return NULL;
}

/* ---
*/
int cell_width(buffer *b, int s, int col)
{
	if (b == NULL) return 80;
	if (s >= b->nsht ||
	    b->sht[s].width == NULL ||
	    col > b->sht[s].used_cols)
		return b->sw;
	return b->sht[s].width[col];
}

/* ---
*/
int cell_height(buffer *b, int s, int row)
{
	if (b == NULL) return 20;
	if (s >= b->nsht ||
	    b->sht[s].height == NULL ||
	    row > b->sht[s].used_lines)
		return b->sh;
	return b->sht[s].height[row];
}

/* ---
*/
void set_width(buffer *b, int s, int col, int width)
{
	int i;

	if (b->sht[s].width == NULL) {
		b->sht[s].width = (int *)MwMalloc((col+1) * sizeof(int));
		for (i = 0; i < col; i++)
			b->sht[s].width[i] = b->sw;
		b->sht[s].used_cols = col;
	} else if (col > b->sht[s].used_cols) {
		b->sht[s].width = (int *)MwRealloc((void *)b->sht[s].width, (col+1)*sizeof(int));
		for (i = b->sht[s].used_cols+1; i < col; i++)
			b->sht[s].width[i] = b->sw;
		b->sht[s].used_cols = col;
	}
	b->sht[s].width[col] = width;
}

/* ---
*/
void set_height(buffer *b, int s, int row, int height)
{
	int i;

	if (b->sht[s].height == NULL) {
		b->sht[s].height = (int *)MwMalloc((row+1)*sizeof(int));
		for (i = 0; i < row; i++)
			b->sht[s].height[i] = b->sh;
		b->sht[s].used_lines = row;
	} else if (row > b->sht[s].used_lines) {
		b->sht[s].height = (int *)MwRealloc((void *)b->sht[s].height, (row+1)*sizeof(int));
		for (i = b->sht[s].used_lines+1; i < row; i++)
			b->sht[s].height[i] = b->sh;
		b->sht[s].used_lines = row;
	}
	b->sht[s].height[row] = height;
}

/* ---
*/
void buffer_global_coords(buffer *buf, int s, int row, int col,
        int *x, int *y)
{
        int i;

        *x = *y = 0;

        for (i = 1; i < col; i++)
                *x += cell_width(buf, s, i);

        for (i = 1; i < row; i++)
                *y += cell_height(buf, s, i);
}

/* ---
*/
int buffer_plugin2index(buffer *buf, int s, int ph)
{
	int n;

	if (!buf) return -1;

	for (n = 0; n < buf->sht[s].nplugin; n++)
		if (ph == buf->sht[s].plugin[n].ph) return n;

	return -1;
}

/* ---
*/
void buffer_cleanup(buffer *buf)
{
	;
}

