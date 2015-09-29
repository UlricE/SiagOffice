/*
   Pathetic Writer
   Copyright (C) 1997, 1998  Ulric Eriksson <ulric@siag.nu>

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

This module contains functions for determining positions within buffers.
--- */

#include <stdio.h>
#include <ctype.h>

#include "pw.h"

/* ---
*/
position make_position(int row, int col)
{
	position p;

	p.row = row;
	p.col = col;
	return p;
}

/* ---
*/
static void sane_position(position *p)
{
	if (p->row < 1) p->row = 1;
	if (p->col < 0) p->row = 0;
}

/* ---
*/
position set_point(window * w, position p)
{
	int s = w->sht;
	sheet *st = w->buf->sht;
	sane_position(&p);
	w->current_fmt = ret_format(w->buf, s, p.row, p.col);
	return st[s].point_pos = p;
}

/* ---
*/
position get_point(window * w)
{
	position p;

	if (w) {
		int s = w->sht;
		sheet *st = w->buf->sht;
		return st[s].point_pos;
	}
	p.row = p.col = 1;
	return p;
}

/* ---
*/
position find_beginning_of_buffer(window * w)
{
	position p;
	p.row = 1;
	p.col = 0;
	return p;
}

/* ---
*/
position find_end_of_buffer(window * w)
{
	position p;
	p.row = line_last_used(w->buf, w->sht);
	p.col = line_length(w->buf, w->sht, p.row);
	return p;
}

/* ---
*/
int at_end_of_line(window * w, position p)
{
	return p.col >= line_length(w->buf, w->sht, p.row);
}

/* ---
*/
int at_beginning_of_line(window * w, position p)
{
	return p.col <= 1;
}

/* ---
*/
position find_beginning_of_line(window * w, position p)
{
	p.col = 1;
	return p;
}

/* ---
*/
position find_end_of_line(window * w, position p)
{
	p.col = line_length(w->buf, w->sht, p.row);
	return p;
}

/* ---
*/
position line_forward(window * w, position p)
{
	if (p.row < max_lines)
		p.row++;
	return p;
}

/* ---
*/
position line_backward(window *w, position p)
{
	if (p.row > 1)
		p.row--;
	return p;
}

/* ---
*/
int at_end_of_buffer(window * w, position p)
{
	return p.row >= max_lines && at_end_of_line(w, p);
}

/* ---
*/
position cell_forward(window * w, position p)
{
	if (p.col < max_columns)
		p.col++;
	return p;
}

/* ---
*/
position cell_backward(window * w, position p)
{
	if (p.col > 1)
		p.col--;
	return p;
}

/* ---
*/
position set_mark(window *w, position p)
{
	int s = w->sht;
	sheet *st = w->buf->sht;
	return st[s].mark_pos = p;
}

/* ---
*/
position get_mark(window *w)
{
	int s = w->sht;
	sheet *st = w->buf->sht;
	return st[s].mark_pos;
}


