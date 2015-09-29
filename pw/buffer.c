/*
   Pathetic Writer
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

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../common/plugin.h"
#include "pw.h"

/* ---
   Module name: buffer.c

   This module handles creating and deleting buffers.  It also contains
   a function for finding a certain buffer, given its name.
--- */

buffer *b_list;

/* ---
Global defaults.
--- */
char *paper_name = "A4";
int paper_height = 842;
int paper_width = 595;
int left_margin = 72, right_margin = 72, top_margin = 72, bottom_margin = 72;
int header_margin = 36, footer_margin = 36;
char *header = "&n";		/* buffer name */
char *footer = "&p";		/* page number */
int header_on_first = 0;
int first_page_number = 1;
int orientation = PORTRAIT;
int tab_distance = 36;

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

/* ---
*/
sheet new_sheet(void)
{
	sheet s;
	char *p = "New Section";
	s.name = MwStrdup(p);
	s.alloc_lines = 0;
	s.text = NULL;
	s.blku.row = -1;
	s.blku.col = -1;
	s.blkl = s.blku;
	s.mark_pos.row = 1;
	s.mark_pos.col = 0;
	s.point_pos = s.mark_pos;
	s.alloc_lines = 0;
	s.text = NULL;
	s.used_lines = 0;
	s.plugin = NULL;
	s.nplugin = 0;
	s.tabs = MwStrdup("l36 l72 l108 l144 l180 l216 "
			 "l252 l288 l324 l360 l396 l432");
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
	MwFree(b->sht[n].name);
	for (i = b->nsht; i < 1000; i++) {
		sprintf(name, _("Section %d"), i);
		if (!sheet_name_taken(b, name)) break;
	}
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

	b = (buffer *) MwMalloc(sizeof(buffer));

	if (b_list == NULL)
		b_list = b;	/* first buffer ever */
	else
		b->next = b_list->next;		/* add to list */
	b_list->next = b;

	strncpy(b->name, name, 1000);
	strncpy(b->path, path, 1000);
	b->nsht = 0;
	b->sht = NULL;
	for (s = 0; s < NSHT; s++) {
		buffer_add_sheet(b, s);
	}
	b->change = FALSE;

	/* Set all of these to their application wide defaults */
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
	b->height_interest = 1;

	return b;
}

/* ---
   buffer *free_buffer(buffer *b)
   Removes b from the buffer list and frees the memory.
   If b_list points to b, it is set to point to the next buffer.
   If the buffer list is empty, b_list is set to NULL.
   Returns the next buffer in the buffer list, or NULL if it is empty.
*/

buffer *free_buffer(buffer * b)
{
	buffer *pb, *next;
	int i;
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

	/* delete the plugin directory, if there is one */
	plugin_basedir(p, b->name);
	rmdir(p);

	MwFree(b);

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
*/

buffer *find_buffer_by_name(char *name)
{
	buffer *b;
	int s;

	if ((b = b_list) == NULL)
		return NULL;

	do {
		if (!strcmp(name, b->name)) {
			return b;
		}
		for (s = 0; s < b->nsht; s++) {
			if (!strcmp(name, b->sht[s].name))
				return b;
		}
		b = b->next;
	} while (b != b_list);

	return NULL;
}

/* ---
*/
buffer *find_sheet_by_name(char *name, buffer *defb, int *s)
{
	buffer *b;
	char bname[1000], sname[1000];
	char *p;

	if (b_list == NULL)
		return NULL;

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
		if (!strcmp(sname, b->sht[*s].name))
			return b;
	}
	return NULL;
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
Calculate what height the line should have and modify the line.
*/

int check_line_height(buffer *b, int s, int row)
{
	int oh, nh;
	MwRichchar *p;

	oh = line_height(b, s, row);
	if (row < 1 || row > b->sht[s].alloc_lines) {
		nh = style_height(0);
	} else {
		nh = 0;
		p = b->sht[s].text[row].p;
		if (p && p[0].c) {
			nh = MwRcStrheight(p, -1);
		}
		if (nh == 0)
			nh = style_height(b->sht[s].text[row].sty);
	}
	if (oh != nh) {
		b->sht[s].text[row].height = nh;
		b->change = pr_scr_flag = 1;
	}
	return nh;
}

/* ---
Returns the height of a line.
*/

int line_height(buffer *buf, int s, int row)
{
	int ph = 0;
	if (buf == NULL || s >= buf->nsht) return style_height(MW_STY_DEFAULT)+ph;
	if (row < 1 || row > buf->sht[s].used_lines)
		return style_height(MW_STY_DEFAULT)+ph;
	return buf->sht[s].text[row].height+ph;
}

/* ---
total number of characters in the line
*/

int line_length(buffer *buf, int s, int row)
{
	if (row < 1 || row > buf->sht[s].used_lines) return 0;
	if (buf->sht[s].text[row].sty == MW_STY_EMBED) return 1;

	return MwRcStrlen(buf->sht[s].text[row].p);
}

/* ---
*/
int cell_width(buffer *b, int s, int col)
{
	return 10;	/* AHH! FIXME! */
}

/* ---
*/
int cell_height(buffer *b, int s, int row)
{
	return line_height(b, s, row);
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
void buffer_cleanup(buffer *buf)
{
	;
}

