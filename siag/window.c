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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*#define DEBUG
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "../siod/siod.h"

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>

#include "../siag/calc.h"

char *rownum_text(buffer *buf, int row, int col)
{
	static char b[80];
	sprintf(b, "%d", row);
	return b;
}

/* ---
This particular completion function doesn't complete at all, it just
   returns TRUE, making TAB equivalent to RET and LFD.
*/

int nocomp(char *b)
{
	return TRUE;
}

/* ---
Calls ask_for_str_comp with nocomp as completion function.
95-06-29: changed "buffer" to "buffr" to please gcc
*/

int ask_for_str(char *prompt, char *buffr)
{
	return ask_for_str_comp(prompt, buffr, nocomp);
}

/* ---
Removes w from the window list and frees the memory.  The window below
w (or above if w was the lowest window on the screen) is expanded to
make use of the space that was previously occupied by w.
The last window on the screen can't be removed with remove_window.
Returns TRUE for success, FALSE for failure.
*/
int remove_window(window *w)
{
	
	if (w == w->next) return FALSE;
	free_window(w);
	return TRUE;
}

/* ---
Splits w in two halves of the same size, if w is big enough.
Both windows point to the buffer that was previously in w.
Return TRUE for success, FALSE for failure.
*/
int split_window(window *w)
{
	window *w2 = new_window(w->buf, w);

	if (w2 == NULL) return FALSE;
	set_point(w2, get_point(w));
	set_top(w2, get_top(w));
	set_prot(w2, get_prot(w));
	return TRUE;
}

/*
How to make Siag always draw the top row and/or the left column.

This is desirable to keep headers on the screen when the sheet
is scrolled. To make this possible, I need to add a field in
the window structure to specify the first unprotected cell.
Everything above and to the left of this cell (called w->prot)
should always be drawed, regardless of what top is.

While this sounds easy, it profoundly changes the mapping from
cell coordinates to screen coordinates and vice versa. For instance,
answering the question "is the cursor on the screen" is no longer
as easy.

Make top be the first unprotected column. Point can never leave
the unprotected area. Leave mark and other positions for now.
*/

/* ---
970427: the following four functions facilitate navigating in
	the cell area with protection implemented
*/

int cell_next_row(window *w, int row)
{
	if (row+1 == get_prot(w).row) return get_top(w).row;
	return row+1;
}

int cell_next_col(window *w, int col)
{
	if (col+1 == get_prot(w).col) return get_top(w).col;
	return col+1;
}

int cell_prev_row(window *w, int row)
{
	if (row == get_top(w).row) return get_prot(w).row-1;
	return row-1;
}

int cell_prev_col(window *w, int col)
{
	if (col == get_top(w).col) return get_prot(w).col-1;
	return col-1;
}

/* ---
From (row, col), calculate (x, y) coordinates. This is a little tricker
	now. Rather than starting the calculation in (0, 0) we must
	take the protected cells into account.
*/

void get_cell_coords(window *w, int top_row, int top_col,
		     int cell_row, int cell_col,
		     int *cell_x, int *cell_y)
{
	int i;

	*cell_y = 0;
	for (i = 1; i < get_prot(w).row; i++)
		*cell_y += zoom*cell_height(w->buf, w->sht, i);

	while (cell_row < top_row) {
		cell_row = cell_next_row(w, cell_row);
		*cell_y -= zoom*cell_height(w->buf, w->sht, cell_row);
	}
	while (cell_row > top_row) {
		cell_row = cell_prev_row(w, cell_row);
		*cell_y += zoom*cell_height(w->buf, w->sht, cell_row);
	}
	*cell_x = 0;
	for (i = 1; i < get_prot(w).col; i++)
		*cell_x += zoom*cell_width(w->buf, w->sht, i);

	while (cell_col < top_col) {
		cell_col = cell_next_col(w, cell_col);
		*cell_x -= zoom*cell_width(w->buf, w->sht, cell_col);
	}
	while (cell_col > top_col) {
		cell_col = cell_prev_col(w, cell_col);
		*cell_x += zoom*cell_width(w->buf, w->sht, cell_col);
	}
	*cell_x /= zoom;
	*cell_y /= zoom;
}

/* ---
*/
void get_coords_cell(window *w, int top_row, int top_col,
		     int *cur_row, int *cur_col,
		     int cur_x, int cur_y)
{
	int prot_x = 0, prot_y = 0, i;

	for (i = 1; i < get_prot(w).col; i++)
		cur_x -= cell_width(w->buf, w->sht, i);
	for (i = 1; i < get_prot(w).row; i++)
		cur_y -= cell_height(w->buf, w->sht, i);

	*cur_row = top_row;
	*cur_col = top_col;
	while (cur_y < prot_y && *cur_row > 1) {
		cur_y += cell_height(w->buf, w->sht, *cur_row);
		(*cur_row) = cell_prev_row(w, *cur_row);
	}
	while (cur_y > cell_height(w->buf, w->sht, *cur_row) && *cur_row < BUFFER_ROWS) {
		cur_y -= cell_height(w->buf, w->sht, *cur_row);
		(*cur_row) = cell_next_row(w, *cur_row);
	}
	while (cur_x < prot_x && *cur_col > 1) {
		cur_x += cell_width(w->buf, w->sht, *cur_col);
		(*cur_col) = cell_prev_col(w, *cur_col);
	}
	while (cur_x > cell_width(w->buf, w->sht, *cur_col) && *cur_col < BUFFER_COLS) {
		cur_x -= cell_width(w->buf, w->sht, *cur_col);
		(*cur_col) = cell_next_col(w, *cur_col);
	}
}

/* ---
*/
void hide_cur(window *w)
{
	;
}

