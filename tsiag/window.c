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

/*
   Module name:    window.c

   This module handles windows: creating, destroying, printing on windows.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <curses.h>
#include <unistd.h>
#include <ctype.h>

#include <termios.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#include "../config.h"

#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>
#include "../common/common.h"

#include "../siag/calc.h"
#include "../siod/siod.h"

#include "tsiag.h"

#ifdef HAVE_LIBGUILE
#include <guile/gh.h>
#endif

#ifdef HAVE_LIBTCL
#include <tcl.h>
#endif

#define KB_MAX 10000
#define IQ_MAX 10000

/* Some systems don't have these, so set some reasonable alternatives */
#ifdef ACS_ULCORNER
#define S1 ACS_S1
#define S9 ACS_S9
#define VLINE ACS_VLINE
#define HLINE ACS_HLINE
#define ULCORNER ACS_ULCORNER
#define URCORNER ACS_URCORNER
#define LLCORNER ACS_LLCORNER
#define LRCORNER ACS_LRCORNER
#define RARROW ACS_RARROW
#define LARROW ACS_LARROW
#define UARROW ACS_UARROW
#define DARROW ACS_DARROW
#else /* eek! but these don't look as pretty! */
#define S1 '~'
#define S9 '_'
#define VLINE '|'
#define HLINE '-'
#define ULCORNER '+'
#define URCORNER '+'
#define LLCORNER '+'
#define LRCORNER '+'
#define RARROW '>'
#define LARROW '<'
#define UARROW '^'
#define DARROW 'v'
#endif

struct winsize size;

struct {
	char *buf;
	int index, size;
} input_queue;

MwFmt fmt0 = {"Helvetica", 100, 0, 0, 0, 0, "black", "white", 0, 0, 0, 0};
MwFmt fmt1 = {"Helvetica", 120, 0, 0, 0, 0, "black", "grey", 0, MW_VADJ_CENTER, MW_HADJ_CENTER, 0};

int columns;    /* number of cells per line */
int width;      /* default cell width */
int left_margin;
/*int right_margin = 5;*/

float zoom = 1.0;	/* unused */

position old;

int rowcol_fmt_index;

unsigned int rowcol_format()
{
	return rowcol_fmt_index;        /* set in init_windows */
}

void pr_region(window *w, position screen_start, position screen_size, position data_start)
{
	int i, j;
	char s[1024];

	/*
	 * duplicating these for loops isn't great, but it makes
	 * everything look nice
	 */
	if (grid_lines) {
		for (i = 0; i < screen_size.row; i++) {
			for (j = 0; j < screen_size.col; j++) {
				ret_pvalue(s, buffer_of_window(w), w->sht, data_start.row + i, data_start.col + j, -1);
				if (s[0] == '\0') {
					wmove(stdscr, 1 + screen_start.row + i, left_margin + (screen_start.col + j) * width);
					waddch(stdscr, S9);
				}
			}
		}
	}

	for (j = 0; j < screen_size.col; j++) {
		for (i = 0; i < screen_size.row; i++) {
			MwFmt fmt;
			MwDecodeFormat(ret_format(buffer_of_window(w), w->sht, data_start.row + i, data_start.col + j), ~0, &fmt);

			if (fmt.borders & MW_BORDER_RIGHT) {
				wmove(stdscr, 1 + screen_start.row + i, left_margin + (screen_start.col + j + 1) * width - 1);
				waddch(stdscr, VLINE);
			}
			if (fmt.borders & MW_BORDER_LEFT) {
				wmove(stdscr, 1 + screen_start.row + i, left_margin + (screen_start.col + j) * width - 1);
				waddch(stdscr, VLINE);
			}
			if (fmt.borders & MW_BORDER_TOP) {
				int k;
				wmove(stdscr, 1 + screen_start.row + i, 1 + left_margin + (screen_start.col + j) * width);
				for (k = 0; k < width - (fmt.borders & MW_BORDER_RIGHT ? 2 : 1); k++)
					waddch(stdscr, S1);
			}
			if (fmt.borders & MW_BORDER_BOTTOM) {
				int k;
				wmove(stdscr, 1 + screen_start.row + i, 1 + left_margin + (screen_start.col + j) * width);
				for (k = 0; k < width - (fmt.borders & MW_BORDER_RIGHT ? 2 : 1); k++)
					waddch(stdscr, S9);
			}

			ret_pvalue(s, buffer_of_window(w), w->sht, data_start.row + i, data_start.col + j, -1);
			if (s[0] != '\0') {
				wmove(stdscr, 1 + screen_start.row + i, left_margin + (screen_start.col + j) * width);
				if (inblock(w, add_position(data_start, make_position(i, j))))
					wstandout(stdscr);
				else
					wstandend(stdscr);
#ifdef A_NORMAL
				if (fmt.bold)
					wattron(stdscr, A_BOLD);
				else
					wattroff(stdscr, A_BOLD);
				if (fmt.italic)
					wattron(stdscr, A_DIM);
				else
					wattroff(stdscr, A_DIM);
#endif
				waddnstr(stdscr, s, size.ws_col - (left_margin + (screen_start.col + j) * width));
				wstandend(stdscr);
			}
		}
	}
}

void pr_prot(window *w)
{
	pr_region(w, make_position(0, 0), make_position(size.ws_row - 3, get_prot(w).col - P_MIN.col), P_MIN);
	pr_region(w, make_position(0, 0), make_position(get_prot(w).row - P_MIN.row, columns), P_MIN);
}

void pr_grid(window *w)
{
	pr_region(w, sub_position(get_prot(w), P_MIN), make_position(size.ws_row - 3 - get_prot(w).row + P_MIN.row, columns - get_prot(w).col + P_MIN.col), get_top(w));
}

void pr_labels(window *w)
{
	int i;

	wstandout(stdscr);
	for (i = 0; i < size.ws_row - 3; i++) {
		wmove(stdscr, 1 + i, 0);
		wprintw(stdscr, "%-*d", left_margin - 1, get_top(w).row - get_prot(w).row + i + P_MIN.row);
	}
	for (i = 0; i < columns; i++) {
		wmove(stdscr, 0, left_margin + width * i);
		wprintw(stdscr, "%-*s", width - 1,
			colnum_text(w->buf, get_top(w).col - get_prot(w).col + i + P_MIN.col));
	}
	wstandend(stdscr);
}

/* Print and refresh all the windows. Sets pr_scr_flag to FALSE. */
void pr_scr(window *w)
{
	buffer *b = b_list;

	MW_TRACE((f, "pr_scr(): b->recalc=%d, recalc=%d", b->recalc, recalc));

	do {
		if (b->recalc) {
			int i, s;
			b->recalc = 0;
			for (i = 0; i < recalc; i++) {
				for (s = 0; s < b->nsht; s++) {
					calc_matrix(b);
				}
			}
		}
		b = b->next;
	} while (b != b_list);

	werase(stdscr);
  
  	w = w_list;
	do {

		pr_labels(w);
		pr_prot(w);
		pr_grid(w);

	} while ((w = next_window(w)) != w_list);

	pr_scr_flag = FALSE;
} /* pr_scr */

/*X
Print prompt on the bottom line and let the user enter a line into buffer.
The following commands exist:
a .. ~		Insert a character
<tab>		Call comp(buffer)
C-t		Interchange the characters around cursor
C-b		Move one character backward
C-f		Move one character forward
C-a		Move to the beginning of the line
C-e		Move to the end of the line
C-d		Delete the character at the cursor
C-h		Delete the character before the cursor
C-k		Delete the rest of the line
Escape		Delete the whole line
C-g		Print "Quit" and return FALSE
Enter	 	Return TRUE
X*/
int ask_for_str_comp(char *prompt, char *buffr, int (*comp)(char *))
{
	int l = strlen(buffr);
	int modified_flag = FALSE;
	textbuf t;

	MW_TRACE((f, "ask_for_str_comp: %s | %s", prompt, buffr));

	/* this is a temporary fix until xsiag can do macros */
	t.size=1;
	t.maxsize=1;
	t.text=buffr;
	add_str_to_input_queue(t);

	buffr[0] = '\0'; l = 0;

	for (;;) {
		int c;

		wmove(stdscr, size.ws_row - 1, 0);
		waddstr(stdscr, prompt);
		wclrtoeol(stdscr);

		/* Show the buffer */
		if (l + strlen(prompt) < size.ws_col) {
			waddnstr(stdscr, buffr, size.ws_col - strlen(prompt));
		} else {
			waddstr(stdscr, &buffr[l - size.ws_col + strlen(prompt) + 1]);
		}

		/* Then place the cursor */
		if (l + strlen(prompt) < size.ws_col) {
			wmove(stdscr, size.ws_row - 1, strlen(prompt) + l);
		} else {
			wmove(stdscr, size.ws_row - 1, size.ws_col - 1);
		}

		wrefresh(stdscr);

		switch (c = get_char_from_input_queue()) {
		case '\t': /* jump to next cell */
			c = CTRL('f');
			t.text=(char *)&c;
			add_str_to_input_queue(t);
			if ((*comp)(buffr)) return modified_flag;
			break;
		case CTRL('f'): /* Move one character forward, or jump to next cell */
			if (buffr[l]) { l++; break; }
			t.text=(char *)&c;
			add_str_to_input_queue(t);
			if ((*comp)(buffr)) return modified_flag;
			break;
		case CTRL('b'): /* Move one character backward, or jump to previous cell */
			if (l > 0) { l--; break; }
		case CTRL('p'): case CTRL('n'): /* jump to adjacent cell */
			t.text=(char *)&c;
			add_str_to_input_queue(t);
		case '\n':
			if ((*comp)(buffr)) return modified_flag;
			break;
		case CTRL('a'): /* Move to the beginning of the line */
			l = 0; break;
		case CTRL('e'): /* Move to the end of the line */
			l = strlen(buffr); break;
		case CTRL('k'): /* Delete the rest of the line */
			buffr[l] = '\0';
			modified_flag = TRUE;
			break;
		case CTRL('d'): /* Delete the character at cursor */
			if (buffr[l]) {
				/* strcpy(buffr+l, buffr+l+1); */
				memmove(buffr+l,buffr+l+1,strlen(buffr+l+1)+1);
			}
			modified_flag = TRUE;
			break;
		case CTRL('h'): /* Delete the character before cursor */
			if (l > 0) {
				l--;
				/* strcpy(buffr+l, buffr+l+1); */
				memmove(buffr+l,buffr+l+1,strlen(buffr+l+1)+1);
			}
			modified_flag = TRUE;
			break;
		case CTRL('g'): /* Return and don't save changes */
			llpr("Changes abandoned"); return FALSE;
			if ((*comp)(buffr)) return modified_flag;
			l = strlen(buffr);
			break;
		case CTRL('t'): /* Interchange the characters around cursor */
			/* umm... I don't see why this is useful. */
			/* I'm leaving it here, though :) */
			if (strlen(buffr) >= 2) {
				if (buffr[l]) l++;
				if (l == 1) {
					c = buffr[0];
					buffr[0] = buffr[1];
					buffr[1] = c;
				} else {
					c = buffr[l-1];
					buffr[l-1] = buffr[l-2];
					buffr[l-2] = c;
				}
			}
			modified_flag = TRUE;
			break;
		case ESC:
			buffr[0] = '\0';
			modified_flag = TRUE;
			break;
		default: /* Normal key press */
			if (c >= ' ' && c <= '~' ) {
				int i;
				for (i = strlen(buffr) + 1; i > l; i--)
					buffr[i] = buffr[i - 1];
				buffr[l++] = c;
			}
			modified_flag = TRUE;
			break;
		}
	}
	/*NOTREACHED*/
}

/* EXTERNAL */
int edit_cell(char *p, char *b)
{
	MW_TRACE((f, "edit_cell"));

	wmove(stdscr, 1 - P_MIN.row + get_point(w_list).row - get_top(w_list).row + get_prot(w_list).row, left_margin + width * (get_point(w_list).col - get_top(w_list).col - P_MIN.col + get_prot(w_list).col) - 1);
	waddch(stdscr, RARROW);

	/* Make show_cur() redraw the contents */
	old = sub_position(P_MIN, make_position(1, 1));

	return ask_for_str(p, b);
}

/* Adds c to the keyboard macro.
Macros longer than KB_MAX charactesr are not allowed. */
void record_macro(int c)
{
	MW_TRACE((f, "record_macro"));

	if (kbd_macro.size >= KB_MAX) {
		llpr("Macro too long; definition terminated");
		kbd_macro.size = 0;
		macro_flag = FALSE;
		return;
	}

	kbd_macro.text[kbd_macro.size++] = c;
}

/*X
Writes the buffer to the input queue.  The size of the buffer is limited
to IQ_MAX characters to prevent recursive macros from crashing the program
(the macros crash, of course, but that is less of a disaster).
TRUE is returned if the string could be added, FALSE otherwise.
X*/
int add_str_to_input_queue(textbuf buf)
{
	int i, j;

	if (input_queue.size + buf.size >= IQ_MAX) return FALSE;

	i = 0;
	j = input_queue.index + input_queue.size;

	while (i < buf.size) {
		/* this isn't very fast, but it should be fast */
		/* enough for our needs */

		j %= IQ_MAX;	/* wrap to beginning of text */
		input_queue.buf[j++] = buf.text[i++];
		input_queue.size++;
	}

	return TRUE;
}

/* This where we fix input, so that things like arrow keys work. */
int fix_input(int c)
{
	switch (c) {
#ifdef KEY_MIN
	case KEY_BEG: case KEY_FIND:
	case KEY_HOME:			return CTRL('a');
	case KEY_LEFT:			return CTRL('b');
	case KEY_END: case KEY_SELECT:	return CTRL('e');
	case KEY_RIGHT:			return CTRL('f');
	case KEY_EOL:			return CTRL('k');
	case KEY_DC:			return CTRL('d');
	case KEY_DOWN:			return CTRL('n');
	case KEY_UP:			return CTRL('p');
	case KEY_PPAGE:			return ALT('v');
	case KEY_NPAGE:			return CTRL('v');
	case KEY_SUSPEND:		return CTRL('z');
	case KEY_BACKSPACE: 		return CTRL('h');
	case KEY_CANCEL:		return CTRL('g');
	case KEY_ENTER: 		return '\n';
#endif
	case '\b':			return CTRL('h');
	case '\r':			return CTRL('\n');
	default:			return c;
	}
	/*NOTREACHED*/
}

/*X
Reads and returns one input character.
If there are characters in the input queue, these are used first.
If macro_flag is TRUE, the character is recorded.
X*/
int get_char_from_input_queue()
{
	int c;
  
	if (input_queue.size > 0) {
		c = input_queue.buf[input_queue.index++];
		input_queue.index %= IQ_MAX;
		input_queue.size--;
	} else {
		c = fix_input(wgetch(stdscr));
	}
	if (macro_flag) record_macro(c);

	MW_TRACE((f, "\tgcfiq: %d %c", c, c));

	return c;
}

void mainloop()
{
	MW_TRACE((f, "Now entering the main loop!"));

	old = get_point(w_list);
	pr_scr_flag = TRUE;

	for(;;) {
		show_cur(w_list);
		old = get_point(w_list);
		do_cmd(get_char_from_input_queue());
	}
	/*NOTREACHED*/
}

/* Prints the string p on the bottom line of the screen. */
void llpr(char *p) /* important */
{
	MW_TRACE((f, "this is llpr speaking (%s)", p));

	wmove(stdscr, size.ws_row - 1, 0);
	waddnstr(stdscr, p, size.ws_col);
	wclrtoeol(stdscr);
	wrefresh(stdscr);
}

/* Remove w from the window list and free the memory it used.
If w was the active window, w_list is moved forward to the next window.
If w was the last window, w_list is set to NULL. */
void free_window(window *w)
{
	window *pw;
  
	MW_TRACE((f, "free_window"));

	/* unlink from window list */
	for (pw = w_list; next_window(pw) != w && next_window(pw) != pw; pw = next_window(pw));

	next_window(pw) = next_window(w);
  
	/* make sure w_lst does not point to a deleted window */
	if (w_list == w) w_list = next_window(w_list);

	/* no windows in the list => w_list = NULL */
	if (w_list == w) w_list = NULL;

	MwFree(w);
}

/*X
The new window points to buffer b.  Point and top positions are set
to the first position in b.
The new window is inserted in the window list after prev, or after itself
if prev is NULL.
The new window is returned, or NULL, if it couldn't be created.
X*/
window *new_window(buffer *b, window *prev)
{
	window *w;

	MW_TRACE((f, "new_window"));

	if((w = (window *)MwMalloc(sizeof(window))) == NULL) return NULL;

	set_window_buffer(w, b);
	w->sht = w->bsht = 0;

	if (prev == NULL) prev = w;
	else next_window(w) = next_window(prev);

	next_window(prev) = w; /* macros are fun! */

	return w;
}

/* the goal is to not do this */
void die(char *s)
{
	MW_TRACE((f, "Die: %s", s));
	exit_windows();			/* Reset the terminal, */
	fprintf(stderr, "%s\n", s);	/* tell'm what happened, */
	exit(EXIT_FAILURE);		/* and split! */
}

/* Internal */
void resize_ncurses()
{
	MW_TRACE((f, "resize_ncurses"));
#ifdef HAVE_RESIZETERM
	if (ioctl(fileno(stdout), TIOCGWINSZ, &size) != 0 &&
	    resizeterm(size.ws_row, size.ws_col) != OK) {
#endif
		MW_TRACE((f, "resizing: Not so cool if this happens"));
		size.ws_row = LINES;
		size.ws_col = COLS;
#ifdef HAVE_RESIZETERM
	}
#endif

	/* The code doesn't handle small screens very well, so for now we
	   just bail out right away.  These dimentions hopefully aren't
	   too limiting */
	if (size.ws_col < 30 || size.ws_row < 10) 
		die("The screen is too small");

	width = 8;
	left_margin = 8;
	columns = (size.ws_col - left_margin) / width;

	pr_scr_flag = TRUE;
	show_cur(w_list);
} /* resize_ncurses */

void new_ncurses()
{
	MW_TRACE((f,"new_ncurses"));

	if ((stdscr = newwin(0, 0, 0, 0)) == NULL)
		die("Error creating window");

#ifdef A_NORMAL
	wattrset(stdscr, A_NORMAL);
#endif
#ifdef HAVE_IMMEDOK
	immedok(stdscr, FALSE);
#endif
	scrollok(stdscr, FALSE);
	leaveok(stdscr, FALSE);
#ifdef HAVE_KEYPAD
	keypad(stdscr, TRUE);
#endif
	resize_ncurses();
}

/* Much nicer then attempting to spawn a shell :) */
LISP suspend_function()
{
	MW_TRACE((f,"suspend_function"));
#ifdef SIGTSTP
	delwin(stdscr);
	raise(SIGTSTP);
	new_ncurses();
#else
	llpr("No SIGTSTP available");
#endif
	return NIL;
}

#ifdef SIGWINCH
/* What to do when someone resizes the xterm (for example) */
void sig_winch_handler()
{
	MW_TRACE((f,"sig_winch_handler"));
	wclear(stdscr);
	resize_ncurses();
}
#endif

#ifdef SIGTERM
/* What to do when someone does a 'killall tsiag' (for example) */
/* Eventually, we'll want to save data somewhere where it can be recovered */
void sig_term_handler()
{
	die("Recieved SIGTERM");
}
#endif

/* Needed for scroll-up/down/left/right */
LISP get_geometry(void)
{
	LISP result;

	MW_TRACE((f, "get_geometry"));

	/* Say the magic words... */
	result = cons(flocons(0), NIL);		/* depth */
	result = cons(flocons(0), result);	/* border_width */
	result = cons(flocons((size.ws_row - 3) * 20), result); /* height */
	result = cons(flocons((size.ws_col - 3) * 10), result); /* width */
	result = cons(flocons(0), result);	/* y */
	result = cons(flocons(0), result);	/* x */

	return result;
}

/* Sets up the whole initial window structure and initializes scrupd.
The window list w_list is set to a list with a single window with the
buffer b. */
void init_windows(buffer *b, int *argc, char **argv)
{
	MW_TRACE((f,"init_windows"));

	initscr();    /* this curses function must be called first */

	raw(); noecho(); nonl(); /* Make input behave */

	/* very important */
	interp_startup();
	init_subr_0("suspend-function", suspend_function);
	init_subr_0("get-geometry", get_geometry);

#ifdef SIGWINCH
	signal(SIGWINCH, sig_winch_handler);
#endif
#ifdef SIGTERM
	signal(SIGTERM, sig_term_handler);
#endif

	w_list = new_window(b, (window *)0);

	delwin(stdscr);
	new_ncurses();

	init_calc_cmds();

	MwEncodeFormat(~0, &fmt0);
	rowcol_fmt_index = MwEncodeFormat(~0, &fmt1);

	kbd_macro.maxsize = KB_MAX;
	kbd_macro.text = (char *)MwCalloc(KB_MAX, sizeof(char));
  
	input_queue.buf = (char *)MwCalloc(IQ_MAX, sizeof(char));
	input_queue.index = 0;
	input_queue.size = 0;
}

/* Cleans up after Calc before exit.  All buffers and windows are freed. */
void exit_windows()
{
	MW_TRACE((f, "exit_windows"));

	/* free all buffers */
	while (b_list != NULL) free_buffer(b_list);
  
	/* free all windows */
	while (w_list != NULL) free_window(w_list);

	endwin();
}

/* This is significantly different then the xsiag draw_input */
/* Also, it's duplicating some of what siag/cmds.c does */
/* but it shure do look pretty */
void draw_input(char *b, int n)
{
	int s = w_list->sht;
	position p = get_point(w_list);

	switch (ret_type(buffer_of_window(w_list), s, p.row, p.col)) {
	case EMBED:
		sprintf(b, "Embedded Object:");
		strncat(b, ret_text(buffer_of_window(w_list), s, p.row, p.col), n - 16);
		return;
	case LABEL: 
		sprintf(b, "Label:");
		strncat(b, ret_text(buffer_of_window(w_list), s, p.row, p.col), n - 6);
		return;
	case ERROR: case EXPRESSION: case STRING:
		sprintf(b, "%s expression:", interpreter2name(ret_interpreter(buffer_of_window(w_list), s, p.row, p.col)));
		strncat(b, ret_text(buffer_of_window(w_list), s, p.row, p.col), n - 14);
		return;
	case EMPTY:
		b[0] = '\0';
		return;
	}
	sprintf(b, "Unkown object type");
	return;
}

/* Move the cursor to reflect the position of point in w.
If point is not visible, the window is moved so that point is in
the middle of the screen. */
void show_cur(window *w)
{
	char *b;
	int i;

	b = (char *)MwMalloc(size.ws_col); /* Is there a better way to do this? */

	MW_TRACE((f,"\tshow_cur(): enter: point=(%d,%d) top=(%d,%d)",
	         get_point(w).row, get_point(w).col,
	         get_top(w).row, get_top(w).col));

	if (!w) return;

	if (get_point(w).row < get_top(w).row) {
		set_top_row(w, get_point(w).row);
		pr_scr_flag = TRUE;
	}
	if (get_point(w).col < get_top(w).col) {
		set_top_col(w, get_point(w).col);
		pr_scr_flag = TRUE;
	}

	/* TODO: these look pretty gross */
	if (get_point(w).row > get_top(w).row + size.ws_row - 3 - (get_prot(w).row - P_MIN.row) - 1) {
		set_top_row(w, get_point(w).row - (size.ws_row - 3 - (get_prot(w).row - P_MIN.row) - 1));
		pr_scr_flag = TRUE;
	}
	if (get_point(w).col > get_top(w).col + columns - (get_prot(w).col - P_MIN.col) - 1) {
		set_top_col(w, get_point(w).col - (columns - (get_prot(w).col - P_MIN.col) - 1));
		pr_scr_flag = TRUE;
	}

	MW_TRACE((f,"\tshow_cur() point=(%d,%d) top=(%d,%d) pr_scr_flag=(%d)",
	         get_point(w).row, get_point(w).col,
	         get_top(w).row, get_top(w).col, pr_scr_flag));

	if (pr_scr_flag) pr_scr(w);

	/* Print contents of cell */
	if(!same_position(get_point(w_list), old)) {
		draw_input(b, size.ws_col);
		wmove(stdscr, size.ws_row - 1, 0);
		waddnstr(stdscr, b, size.ws_col);
		wclrtoeol(stdscr);
	}

	wstandout(stdscr);

	/* the line */
	wmove(stdscr, size.ws_row - 2, 0);
	for (i = 0; i < size.ws_col; i++)
		waddch(stdscr, HLINE);

	/* filename and modified flag */
	wmove(stdscr, size.ws_row - 2, 0);
	wprintw(stdscr, "[%s]%s%s",
	        buffer_of_window(w)->name,
	        buffer_of_window(w)->change ? "[M]" : "",
	        macro_flag ? "[Record Macro]" : "");

	/* point coordinates and protection flag */
	sprintf(b, "%s[%d, %d]", 
	        same_position(get_prot(w), P_MIN) ? "" : "[P]",
	        get_point(w).row, get_point(w).col);
	wmove(stdscr, size.ws_row - 2, size.ws_col - strlen(b));
	waddstr(stdscr, b);

	wstandend(stdscr);

	/* finally, the cursor */
	wmove(stdscr, 1 - P_MIN.row + get_point(w).row - get_top(w).row + get_prot(w).row, left_margin + width * (get_point(w).col - get_top(w).col - P_MIN.col + get_prot(w).col));

	wrefresh(stdscr);
} /* show_cur */

/* This function is used */
void activate_window(window *w)
{
	MW_TRACE((f,"activate_window"));
	w_list = w;
}

int select_from_list(char *text, char **choices, int nchoices)
{
	int i;
	int offset = 0;

	MW_TRACE((f,"select_from_list(%s,%s...,%d)",text,*choices,nchoices));

	pr_scr_flag = TRUE;

	for(;;) {
		werase(stdscr);

		wmove(stdscr, 0, 0);
		wstandout(stdscr);
		waddnstr(stdscr, text, size.ws_col);
		for (i = strlen(text); i < size.ws_col; i++) waddch(stdscr, ' ');
		wstandend(stdscr);
	
		if (offset > 0 ) {
			wmove(stdscr, 2, 5);
			waddch(stdscr, UARROW);
		}
	
		if (10 + offset <= nchoices) {
			wmove(stdscr, 13, 5);
			waddch(stdscr, DARROW);
		}

		for (i = 0; i < 10 && i + offset < nchoices; i++) {
			wmove(stdscr, i + 3, 2);
			wprintw(stdscr, "%d: %s", i, choices[i + offset]);
		}
	
		wmove(stdscr, size.ws_row - 1, 0);
		wrefresh(stdscr);

		switch (i = get_char_from_input_queue()) {
		case ESC: return -1;
		case CTRL('n'): case CTRL('f'): case ' ': case CTRL('v'): case '-':
			if(offset + 10 < nchoices) offset += 10;
			break;
		case CTRL('p'): case CTRL('b'): case ALT('v'): case '+':
			if(offset - 10 >= 0) offset -= 10;
			break;
		default:
			if (i >= '0' && i < 10 + '0') { 
				wstandout(stdscr);
				wmove(stdscr, i - '0' + 3, 2);
				wprintw(stdscr, "%d: %s", i - '0', choices[i - '0' + offset]);
				wstandend(stdscr);
				return i - '0' + offset;
			}
		}
	}
	/*NOTREACHED*/
}

