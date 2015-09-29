/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-1999  Ulric Eriksson <ulric@siag.nu>

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
    position.c

      This module contains functions for determining positions within
      buffers.
--- */

#include <stdio.h>
#include <ctype.h>

#include "../siag/calc.h"

/* ---
*/
position make_position(int make_r, int make_c)
{
	position p;
	p.row = make_r;
	p.col = make_c;
	return (p);
}

position add_position(position p1, position p2)
{
	return make_position(p1.row + p2.row, p1.col + p2.col);
}

position sub_position(position p1, position p2)
{
	return make_position(p1.row - p2.row, p1.col - p2.col);
}

int same_position(position p1, position p2)
{
	return p1.row == p2.row && p1.col == p2.col;
}

/* ---
kludge to make sure point and top are outside the protected area
*/

void prot_fixup(window * w)
{
	int s = w->sht;
	sheet *st = w->buf->sht;
	if (st[s].top.row < st[s].prot.row)
		st[s].top.row = st[s].prot.row;
	if (st[s].top.col < st[s].prot.col)
		st[s].top.col = st[s].prot.col;
	if (st[s].point_pos.row < st[s].prot.row)
		st[s].point_pos.row = st[s].prot.row;
	if (st[s].point_pos.col < st[s].prot.col)
		st[s].point_pos.col = st[s].prot.col;
}

/* ---
*/
position set_point(window * w, position p)
{
	if (!w) return p;
		
	w->buf->sht[w->sht].point_pos = p;
	prot_fixup(w);
	return w->buf->sht[w->sht].point_pos;
}

/* ---
*/
position get_point(window * w)
{
	if (w) return w->buf->sht[w->sht].point_pos;
	return P_MIN;
}

/* ---
*/
position set_mark(window * w, position p)
{
	w->buf->sht[w->sht].mark_pos = p;
	prot_fixup(w);
	return w->buf->sht[w->sht].mark_pos;
}

/* ---
*/
position get_mark(window *w)
{
	return w->buf->sht[w->sht].mark_pos;
}

/* ---
*/
position set_top(window *w, position p)
{
	w->buf->sht[w->sht].top = p;
	prot_fixup(w);
	return w->buf->sht[w->sht].top;
}

/* ---
*/
position get_top(window * w)
{
	return w->buf->sht[w->sht].top;
}

/* ---
*/
int inblock(window *w, position p)
{
	return ((p.row >= block_upper(w).row) && (p.row <= block_lower(w).row) &&
	        (p.col >= block_upper(w).col) && (p.col <= block_lower(w).col));
} /* inblock */

/* ---
*/
void set_blku_row(window *w, int r)
{
	buffer *b = w->buf;
	int s = w->bsht;

	if (r >= 1) b->sht[s].blku.row = r;
}

/* ---
*/
void set_blkl_row(window *w, int r)
{
	buffer *b = w->buf;
	int s = w->bsht;

	if (r >= 1) b->sht[s].blkl.row = r;
}

/* ---
*/
void set_blku_col(window *w, int r)
{
	buffer *b = w->buf;
	int s = w->bsht;

	if (r >= 1) b->sht[s].blku.col = r;
}

/* ---
*/
void set_blkl_col(window *w, int r)
{
	buffer *b = w->buf;
	int s = w->bsht;

	if (r >= 1) b->sht[s].blkl.col = r;
}

