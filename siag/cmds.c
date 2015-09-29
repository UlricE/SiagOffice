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

/* ---
      cmds.c

      This rather bulky module contains all the functions that implement
      commands.  It also handles initialization of the interface to those
      functions.
--- */

#define DEBUG

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>

#include "../siod/siod.h"
#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>
#include "../common/common.h"
#include "../common/plugin.h"
#include "calc.h"

typedef struct {
	char *name;
	 LISP(*function) (void);
} s_fn_table0;

typedef struct {
	char *name;
	 LISP(*function) (LISP);
} s_fn_table1;

typedef struct {
	char *name;
	 LISP(*function) (LISP, LISP);
} s_fn_table2;

/* ---
*/
static LISP auto_recalc(LISP n)
{
	if (NFLONUMP(n))
		err("wta(1st) to auto-recalc", n);
	recalc = get_c_long(n);
	return NIL;
}

static LISP lcmalloc_stats(void)
{
	MwMallocStats();
	return NIL;
}

/* ---
*/
static LISP lspawn(LISP command)
{
	double fl_pid;

	fl_pid = MwSpawn(get_c_string(command));
	return flocons(fl_pid);
}

/* ---
*/
static LISP ldeletia_add(LISP p, LISP fn)
{
	deletia_add(get_c_long(p), get_c_string(fn));
	return NIL;
}

/* Windows and buffers */

/* ---
   static void load_buffer()

   Load a buffer from file.
*/

static LISP load_buffer(void)
{
	static char path[1024], name[1024];
	char fn[1024];
	char fmt[80];
	buffer *b;
	static int need_init = 1;
	char *startup;

	if (need_init) {
		getcwd(path, 1024);
		need_init = 0;
	}
	strncpy(name, "", 1020);
	fn[0] = '\0';
	if (select_file(path, name, loader_patterns, fmt, 0)) {
		sprintf(fn, "%s/%s", path, name);

		b = new_buffer(buffer_name(fn), fn);
		llpr("Loading");

		set_window_buffer(w_list, b);
		w_list->sht = 0;
		if (loadmatrix(fn, b, fmt))
			llpr("New file");
	/* this will fail if load returns 1 because the string pool is full */

	/* execute startup code, if any */
		if ((startup = get_property(b, "startup"))) {
			FILE *fp;
			char fn[256];
			char cmd[256];
			char *buttons[] = {"Yes", "No"};
			int n = alert_box("Load auto startup code?", buttons, 2);
			if (n != 1) {
				sprintf(fn, "%s/siag%ld",
					siag_tmpdir, (long)getpid());
				sprintf(cmd, "(load \"%s\")", fn);
				fp = fopen(fn, "w");
				fwrite(startup, strlen(startup), 1, fp);
				fputc('\n', fp);
				fclose(fp);
				execute(cmd);
				remove(fn);
			}
		}

		b->change = FALSE;
		calc_matrix(b);
		pr_scr_flag = TRUE;
	}
	activate_window(w_list);
	return NIL;
}

/* ---
Load using an external program

   1. Ask for the program to use
   2. Run the program and save output to file in siag_tmpdir
   3. Load the file using NULL format (i.e. ask for type)
*/

static LISP load_external(void)
{
	static int loaders = 0;
	static char *loadname[20], *loadprog[20];
	static int need_init = 1;
	char program[256], param[256], fn[80], cmd[256];
	buffer *b;
	int i;

	if (need_init) {
		FILE *fp;
		char fnl[1024];
		char *p, *q, b[256];

		sprintf(fnl, "%s/siag/external.load", datadir);
		if ((fp = fopen(fnl, "r")) == NULL) {
			llpr("Can't open loader file");
			return NIL;
		}
		while (fgets(b, 250, fp) != NULL && loaders < 20) {
			if ((p = strtok(b, ":")) && (q = strtok(NULL, "\n"))) {
				loadname[loaders] = MwStrdup(p);
				loadprog[loaders] = MwStrdup(q);
				loaders++;
			}
		}
		fclose(fp);
		need_init = 0;
	}
	program[0] = param[0] = '\0';
	i = select_from_list("External Program:", loadname, loaders);

	if (i >= 0 && ask_for_str("Parameters:", param)) {
		sprintf(fn, "%s/siag%ld", siag_tmpdir, (long)getpid());
		sprintf(cmd, "%s %s > %s", loadprog[i], param, fn);
		if (system(cmd)) {
			llpr("External program failed");
			return NIL;
		}

		b = new_buffer(buffer_name(fn), fn);
		llpr("Loading");

		if (loadmatrix(fn, b, NULL))
			llpr("New file");
	/* this will fail if load returns 1 because the string pool is full */

		calc_matrix(b);
		b->change = FALSE;
		set_window_buffer(w_list, b);
		w_list->sht = 0;
		pr_scr_flag = TRUE;
	}
	activate_window(w_list);
	return NIL;
}

/* ---
*/
static LISP delete_window(void)
{
	if (!remove_window(w_list))
		llpr("Attempt to delete sole ordinary window");
	else
		pr_scr_flag = TRUE;
	activate_window(w_list);
	return NIL;
}

/* ---
*/
static LISP delete_other_windows(void)
{
	while (remove_window(next_window(w_list)));
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP split_window_vertically(void)
{
	if (!split_window(w_list))
		llpr("This window is too small to split");
	else
		pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP other_window(void)
{
	activate_window(next_window(w_list));
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
   static void save_buffer()

   Save the buffer in the currently active window to file.
*/

static LISP save_buffer(void)
{
	buffer *buf = buffer_of_window(w_list);
	char fn[100];
	llpr("Saving");

	sprintf(fn, _("Saved %s"), buf->name);
	if (savematrix(buf->path,
			buf,
			guess_file_format(buf->path)))
		error_box("Couldn't save");
	else {
		buf->change = FALSE;
		llpr(fn);
	}
	return NIL;
}

/* ---
*/
static LISP lsavematrix(LISP path, LISP bname, LISP format)
{
	char *p, *fmt;
	buffer *b;

	if (NULLP(bname)) b = buffer_of_window(w_list);
	else b = find_buffer_by_name(get_c_string(bname));
	if (!b) {
		llpr("No such buffer");
		return NIL;
	}

	p = get_c_string(path);
	if (NULLP(format)) fmt = guess_file_format(p);
	else fmt = get_c_string(format);

	if (savematrix(p, b, fmt)) {
	  llpr("File saved"); /* what is this one for? */
		return NIL;
	} else {
		return a_true_value();
	}
}

static int confirm_overwrite = 0;

/* ---
*/
static LISP lconfirm_overwrite(LISP x)
{
	confirm_overwrite = get_c_long(x);
	return NIL;
}

/* ---
   static void save_buffer_as()

   Save the buffer in the currently active window to a named file.
*/

static LISP save_buffer_as(void)
{
	static char path[1024], name[1024];
	char fn[1024];
	char fmt[80];
	char *p;
	static int need_init = 1;
	struct stat sb;

	if (need_init) {
		getcwd(path, 1024);
		need_init = 0;
	}
	p = strrchr(buffer_of_window(w_list)->path, '/');
	if (p) strncpy(name, p+1, 1020);
	else strncpy(name, buffer_of_window(w_list)->path, 1020);
	fn[0] = '\0';
	if (select_file(path, name, saver_patterns, fmt, 1)) {
		sprintf(fn, "%s/%s", path, name);
		if (confirm_overwrite && !stat(fn, &sb)) {
			/* file exists */
			char q[1024];
			char *btn[] = {"Yes", "No"};
			int n;
			sprintf(q, _("Overwrite existing %s?"), fn);
			n = alert_box(q, btn, 2);
			if (n == 1) return NIL;
		}
		llpr("Saving");
		if (savematrix(fn, buffer_of_window(w_list), fmt))
			error_box("Couldn't save");
		else {
		        char temp[100];
			buffer_of_window(w_list)->change = FALSE;
			sprintf(temp, _("Saved as %s"), name);
			llpr(temp);
			
			strncpy(buffer_of_window(w_list)->path, fn, 1020);
		}
	/*	pr_scr_flag = TRUE;*/
	}
	return NIL;
}

/* ---
Save using an external program

   1. Ask for the program to use
   2. Save to a file in siag_tmpdir using NULL format (i.e. ask for type)
   3. Run the program and read the file as input
*/

static LISP save_external(void)
{
	static int savers = 0;
	static char *savename[20], *saveprog[20];
	static int need_init = 1;
	char program[256], param[256], fn[80], cmd[256];
	int i;

	if (need_init) {
		FILE *fp;
		char fnl[1024];
		char *p, *q, b[256];

		sprintf(fnl, "%s/siag/external.save", datadir);
		if ((fp = fopen(fnl, "r")) == NULL) {
			error_box("Can't open saver file");
			return NIL;
		}
		while (fgets(b, 250, fp) != NULL && savers < 20) {
			if ((p = strtok(b, ":")) && (q = strtok(NULL, "\n"))) {
				savename[savers] = MwStrdup(p);
				saveprog[savers] = MwStrdup(q);
				savers++;
			}
		}
		fclose(fp);
		need_init = 0;
	}

	program[0] = param[0] = '\0';
	i = select_from_list("External Program:", savename, savers);

	if (i >= 0 && ask_for_str("Parameters:", param)) {
		llpr("Saving");

		sprintf(fn, "%s/siag%ld", siag_tmpdir, (long)getpid());
		if (savematrix(fn, buffer_of_window(w_list), NULL)) {
			error_box("Couldn't save");
			return NIL;
		}
		sprintf(cmd, "%s %s < %s", program, param, fn);
		if (system(cmd)) {
			error_box("External program failed");
			return NIL;
		}
	}
	llpr("File saved via external program"); /* something more useful?*/
	return NIL;
}

#if USE_COMPLETION
/* ---
   static void complete_name(char *name)
   This function takes a partially completed buffer name
   and returns the first buffer name that matches it.
*/

static int complete_name(char *name)
{
	buffer *b;
	int len;

	b = w_list->buf;	/* start with the next buffer */
	do {
		b = b->next;
		if ((len = strlen(name)) == 0 
				|| !strncmp(b->name, name, len)) {
			strncpy(name, b->name, 1020);
			return FALSE;
		}
	} while (b != w_list->buf);
	return FALSE;
}
#endif

/* ---
*/
static LISP switch_to_buffer(void)
{
	buffer *b;
	char *blist[100];
	int nblist = 0, n;
	int i;
	int s;

	b = buffer_of_window(w_list);
	s = w_list->sht;
	for (i = 0; i < b->sht[s].nplugin; i++) {
		if (b->sht[s].plugin[i].displayed) {
			plugin_hide(b->sht[s].plugin[i].ph);
			b->sht[s].plugin[i].displayed = 0;
		}
	}
	do {
		b = b->next;
		blist[nblist++] = b->name;
	} while (b != buffer_of_window(w_list));
	if ((n = select_from_list("Change Buffer:", blist, nblist)) >= 0) {
		set_window_buffer(w_list, find_buffer_by_name(blist[n]));
		w_list->sht = 0;
	}
	pr_scr_flag = TRUE;
	activate_window(w_list);
	return NIL;
}

static char *quit_buttons[] = {"Yes", "No", "Cancel"};

/* ---
*/
static LISP kill_buffer(LISP x)
{
	buffer *b, *next_b;
	window * w;
	char *blist[100];
	int nblist = 0, n;
	char prompt[256];

	b = buffer_of_window(w_list);
	do {
		b = b->next;
		blist[nblist++] = b->name;
	} while (b != buffer_of_window(w_list));
	sprintf(prompt, "%s:", _("Close Buffer"));
	if ((n = select_from_list(prompt, blist, nblist)) >= 0) {
		if ((b = find_buffer_by_name(blist[n])) != NULL) {
			if (b != b->next) {
				if (b->change && (x != NIL)) {
					sprintf(prompt,
						_("Save %s?"), b->name);
					switch (alert_box(prompt,
							quit_buttons, 3)) {
					case 0:
						savematrix(b->path, b,
							guess_file_format(b->path));
						break;
					case 2:
						return NIL;
					default:
						break;
					}
				}
				next_b = free_buffer(b);
				w = w_list;
				do {
					if (buffer_of_window(w) == b) {
						set_window_buffer(w, next_b);
						w->sht = 0;
					}
					w = next_window(w);
				} while (w != NULL && w != w_list);
				pr_scr_flag = TRUE;
			}
			else llpr("Can't close only buffer");
		}
	}
	activate_window(w_list);
	return NIL;
}

/* ---
*/
static void copy_from_start(buffer *b, int s,
		position blku, position blkl, int nr, int nk, int smart)
{
	int i, j;
	char *old, *new;

	undo_save(b, s, blku.row, blku.col, blkl.row, blkl.col);
	for (i = 0; i < nr; i++) {
		for (j = 0; j < nk; j++) {
			if (inblock(w_list,
				    add_position(get_point(w_list),
				    make_position(i, j)))
			    && ins_data(b,
					ret_interpreter(b, s, blku.row+i,
							blku.col+j),
				        ret_text(b, s, blku.row+i, blku.col+j),
				        ret_val(b, s, blku.row+i, blku.col+j),
				        ret_type(b, s, blku.row+i, blku.col+j),
				        s,
				        get_point(w_list).row+i,
				        get_point(w_list).col+j)) {
				buffer_of_window(w_list)->change = TRUE;
				if (smart
				    && (ret_type(b, s, get_point(w_list).row+i,
					 	 get_point(w_list).col+j)==EXPRESSION)) {
					cval value;
					old = ret_text(b, s,
						get_point(w_list).row+i,
						get_point(w_list).col+j);
					new = update_references(b, ret_interpreter(b, s, get_point(w_list).row+i,
						get_point(w_list).col+j),
						old, 1, 1, BUFFER_ROWS, BUFFER_COLS,
						get_point(w_list).row - blku.row,
						get_point(w_list).col - blku.col);
					if (old != new) {
						value = ret_val(b, s, get_point(w_list).row+i,
							get_point(w_list).col+j);
						ins_data(b,
							 ret_interpreter(b, s,
								get_point(w_list).row+i,
								get_point(w_list).col+j),
							 new, value,
							 ret_type(b, s,
								get_point(w_list).row+i,
								get_point(w_list).col+j),
						 	 s,
							 get_point(w_list).row+i,
							 get_point(w_list).col+j);
					}
				}
			}
		}
	}
}

/* ---
Name changed from copy_block to fill_block
Also only fills within block
*/

static LISP fill_block(void)
{
	int nr, nk;
	position blku, blkl;
	int s;

	blku = block_upper(w_list);
	blkl = block_lower(w_list);
	nr = blkl.row - blku.row + 1;
	nk = blkl.col - blku.col + 1;
	s = w_list->sht;

	copy_from_start(buffer_of_window(w_list), s, blku, blkl, nr, nk, 0);
	calc_matrix(buffer_of_window(w_list));
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP smart_fill_block(void)
{
       int nr, nk;
       position blku, blkl;
	int s;

       blku = block_upper(w_list);
       blkl = block_lower(w_list);
       nr = blkl.row - blku.row + 1;
       nk = blkl.col - blku.col + 1;
	s = w_list->sht;

       copy_from_start(buffer_of_window(w_list), s, blku, blkl, nr, nk,1);
       calc_matrix(buffer_of_window(w_list));
       pr_scr_flag = TRUE;
       return NIL;
}

/* ---
*/
static LISP block_borders(LISP style)
{
	buffer *buf = w_list->buf;
	int sty, r, c, oldfmt, newfmt;
	MwFmt fmt;
	int s;

	if (NFLONUMP(style))
		err("wta(1st) to block-borders", style);
	sty = get_c_long(style);
	if  (block_upper(w_list).row < 1 || block_upper(w_list).col < 1)
		return NIL;

	s = w_list->bsht;
	for (r = block_upper(w_list).row; r <= block_lower(w_list).row; r++) {
		cval value;
		value.number = 0.0;
		for (c = block_upper(w_list).col; c <= block_lower(w_list).col; c++) {
			if (ret_type(buf, s, r, c) == EMPTY) {
				ins_data(buf, siod_interpreter,
					" ", value, LABEL, s, r, c);
			}
			oldfmt = ret_format(buf, s, r, c);
			MwDecodeFormat(oldfmt, ~0, &fmt);
			switch (sty) {
			case 1:		/* borders */
				if (r == block_upper(w_list).row)
					fmt.borders |= MW_BORDER_TOP;
				if (r == block_lower(w_list).row)
					fmt.borders |= MW_BORDER_BOTTOM;
				if (c == block_upper(w_list).col)
					fmt.borders |= MW_BORDER_LEFT;
				if (c == block_lower(w_list).col)
					fmt.borders |= MW_BORDER_RIGHT;
				break;
			case 2:		/* grid */
				fmt.borders |= MW_BORDER_MASK;
				break;
			case 3:		/* underline */
				fmt.borders ^= MW_BORDER_BOTTOM;
				break;
			case 4:		/* border top */
				fmt.borders ^= MW_BORDER_TOP;
				break;
			case 5:		/* border left */
				fmt.borders ^= MW_BORDER_LEFT;
				break;
			case 6:		/* border right */
				fmt.borders ^= MW_BORDER_RIGHT;
				break;
			default:	/* none */
				fmt.borders &= ~MW_BORDER_MASK;
			}
			newfmt = MwEncodeFormat(~0, &fmt);
			ins_format(buffer_of_window(w_list), s, r, c, newfmt);
		}
	}
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP execute_extended_command(void)
{
	char b[256];

	b[0] = '\0';
	if (ask_for_str("Command:", b))
		execute(b);
	return NIL;
}

/* ---
*/
static LISP execute_interpreter_command(LISP intpr)
{
	char prompt[80];
	char b[256];
	char *intname = get_c_string(intpr);
	int intp = name2interpreter(intname);
	if (intp < 0) return NIL;
	sprintf(prompt, _("%s command:"), intname);
	b[0] = '\0';
	if (ask_for_str(prompt, b))
		exec_expr(intp, b);
	return NIL;
}

/* ---
*/
static LISP lalertbox(LISP prompt, LISP buttons)
{
	char *btext[10], *p = get_c_string(prompt);
	int bno = 0, n;

	while (bno < 10 && NNULLP(buttons)) {
		char *c = get_c_string(car(buttons));
		if (c) btext[bno] = MwStrdup(c);
		else btext[bno] = MwStrdup("button");
		buttons = cdr(buttons);
		bno++;
	}
	if (p == NULL) p = "prompt";
	n = alert_box(p, btext, bno);
	while (bno) MwFree(btext[--bno]);
	return flocons(n);
}

static LISP laboutbox(void)
{
	char msg[1024];
	sprintf(msg, "%s\n\n%s\n\nUlric Eriksson, ulric@siag.nu\n\n%s\n",
		version,
		_("A spreadsheet for X"),
		_("Part of Siag Office"));
	about_box(msg);
	return NIL;
}

static LISP laboutsiag(void)
{
	about_siag();
	return NIL;
}

/* ---
*/
static LISP quit_siag(void)
{
	char prompt[256], cmd[1024];
	buffer *b = b_list;
	do {
		if (b->change /*&& !plugin*/) {
			sprintf(prompt, _("Save %s?"), b->name);
			switch (alert_box(prompt, quit_buttons, 3)) {
			case 0:
				savematrix(b->path,
						b,
						guess_file_format(b->path));
				break;
			case 2:
				return NIL;
			default:
				break;
			}
		}
		b = b->next;
	} while (b != b_list);
	/* remove all windows, buffers and plugins */
	exit_windows();
	/* remove the temp directory for this process */
	sprintf(cmd, "rm -rf %s/%ld", siag_basedir, (long)getpid());
	system(cmd);
	exit(0);
	return NIL;
}

/* ---
*/
static LISP go_to(void)
{
	char b[256];
	int tr = 0, tc = 0;

	b[0] = '\0';
	if (ask_for_str("Go to:", b))
		sscanf(b, "%d %d", &tr, &tc);

	if ((tr > 0) && (tr <= BUFFER_ROWS))
		set_point_row(w_list, tr);
	if ((tc > 0) && (tc <= BUFFER_COLS))
		set_point_col(w_list, tc);
	return NIL;
}

/* ---
*/
static LISP set_cell_width(void)
{
	char b[256];
	int width = 0;
	int ccw;

	b[0] = 0;

	/* current cell width as default */
	ccw = cell_width(buffer_of_window(w_list), w_list->sht,
	               w_list->buf->sht[w_list->sht].point_pos.col);
	sprintf(b, "%d", ccw);

	if (ask_for_str("Width:", b))
		sscanf(b, "%d", &width);

	if (width > 5 && width < 500) {
		set_width(buffer_of_window(w_list), w_list->sht,
			get_point(w_list).col, width);
		pr_scr_flag = 1;
	}
	return NIL;
}

/* ---
*/
static LISP get_cell_width(LISP col)
{
	int c = get_c_long(col);
	return flocons(cell_width(buffer_of_window(w_list), w_list->sht, c));
}

/* ---
*/
static LISP set_standard_width(void)
{
	w_list->buf->sw = cell_width(w_list->buf, w_list->sht,
				get_point(w_list).col);
	pr_scr_flag = w_list->buf->change = TRUE;
	return NIL;
}

/* ---
*/
static LISP set_standard_height(void)
{
	w_list->buf->sh = cell_height(w_list->buf, w_list->sht,
				get_point(w_list).row);
	pr_scr_flag = w_list->buf->change = TRUE;
	return NIL;
}

/* ---
*/
static LISP set_standard_format(void)
{
	w_list->buf->sf = ret_format(w_list->buf, w_list->sht,
					get_point(w_list).row,
					get_point(w_list).col);
	pr_scr_flag = w_list->buf->change = TRUE;
	return NIL;
}

/* ---
*/
static LISP set_cell_height(void)
{
	char b[256];
	int height = 0;
	int cch;

	b[0] = 0;

	/* current cell heigth as default */
	cch = cell_height(buffer_of_window(w_list), w_list->sht,
			  w_list->buf->sht[w_list->sht].point_pos.row);
	sprintf(b, "%d", cch);

	if (ask_for_str("Height:", b))
		sscanf(b, "%d", &height);

	if (height > 5 && height < 500) {
		set_height(buffer_of_window(w_list), w_list->sht,
				get_point(w_list).row, height);
		pr_scr_flag = 1;
	}
	return NIL;
}

/* ---
*/
static LISP get_cell_height(LISP row)
{
	int r = get_c_long(row);
	return flocons(cell_height(buffer_of_window(w_list),
					w_list->sht, r));
}

/* ---
*/
static int select_style(void)
{
	int i, n, s;
	char **list = style_list(&n);
	i = select_from_list("Expression style:", list, n);
	if (i < 0) s = -1;
	else s = name2style(list[i]);
	MwFree(list);
	return s;
}

/* ---
*/
static LISP set_block_style(void)
{
	int style;
	int r, c, r2, c2;
	buffer *b = buffer_of_window(w_list);
	int s = w_list->sht;
	MwFmt fmt;

	style = select_style();
	if (style < 0) return NIL;

	r2 = b->sht[s].alloc_lines;
	for (r = block_upper(w_list).row; r <= block_lower(w_list).row; r++) {
		if (r > r2) break;
		c2 = b->sht[s].alloc_cols[r];
		for (c = block_upper(w_list).col;
			c <= block_lower(w_list).col; c++) {
			if (c > c2) break;
			MwDecodeFormat(ret_format(w_list->buf, s, r, c),
					~0, &fmt);
			fmt.style = style;
			ins_format(w_list->buf, s, r, c,
					MwEncodeFormat(~0, &fmt));
		}
	}
	pr_scr_flag = 1;
	return NIL;
}

/* ---
*/
static LISP set_cell_style(void)
{
	int style;
	MwFmt fmt;
	int r = get_point(w_list).row;
	int c = get_point(w_list).col;
	int s = w_list->sht;

	style = select_style();
	if (style < 0) return NIL;

	MwDecodeFormat(ret_format(w_list->buf, s, r, c),
			~0, &fmt);
	fmt.style = style;
	ins_format(w_list->buf, s, r, c,
			MwEncodeFormat(~0, &fmt));

	pr_scr_flag = 1;
	return NIL;
}

/* ---
*/
static LISP define_style(void)
{
	int i;
	char name[1000], fmt[1000];

	i = select_style();
	if (i < 0) return NIL;

	strcpy(name, style_table[i].name);
	strcpy(fmt, style_table[i].fmt);
	if (ask_for_str(name, fmt))
		lookup_style(name, fmt, style_table[i].type);

	pr_scr_flag = 1;
	return NIL;
}

/* ---
*/
static LISP print_version(void)
{
	llpr(version);
	return NIL;
}

/* ---
*/
static LISP buffer_changed(LISP bname)
{
	buffer *buf;

	if (NULLP(bname)) buf = buffer_of_window(w_list);
	else buf = find_buffer_by_name(get_c_string(bname));

	if (buf) buf->change = TRUE;
	return NIL;
}

/* ---
Returns value if successful, otherwise NIL
*/

static LISP set_data(LISP bname, LISP text, LISP value, LISP type, LISP pos)
{
	buffer *buf;
	char *tx;
	cval val;
	short t;
	char *i;
	int r, c;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;

	i = tx = get_c_string(text);
	if (i == NULL) return NIL;
	t = get_c_long(type);
	switch (t) {
	case EXPRESSION:
		val.number = get_c_double(value);
		break;
	case STRING:
		val.text = i;
		break;
	default:
		val.number = 0;
	}
	r = POSITION_ROW(pos);
	c = POSITION_COL(pos);

	if (ins_data(buf, siod_interpreter, i, val, t, s, r, c)) {
		buf->change = TRUE;
		return value;
	}
	return NIL;
}

/* ---
*/
static LISP lswap_cells(LISP bname, LISP row1, LISP col1, LISP row2, LISP col2)
{
	buffer *buf;
	int r1, c1, r2, c2;
	int s;

	if (NULLP(bname)) {
		buf = w_list->buf;
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;

	r1 = get_c_long(row1);
	c1 = get_c_long(col1);
	r2 = get_c_long(row2);
	c2 = get_c_long(col2);
	swap_cells(buf, s, r1, c1, r2, c2);
	buf->change = TRUE;

	return NIL;
}

/* ---
*/
static LISP set_format(LISP bname, LISP pos, LISP format)
{
	buffer *buf;
	int fmt, r, c;
	double retval;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;

	r = POSITION_ROW(pos);
	c = POSITION_COL(pos);
	fmt = get_c_long(format);

	buf->change = TRUE;
	retval = ins_format(buf, s, r, c, fmt);
	return flocons(retval);
}

/* ---
*/
static LISP get_format(LISP bname, LISP pos)
{
	buffer *buf;
	int r, c;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;

	r = POSITION_ROW(pos);
	c = POSITION_COL(pos);
	return flocons(ret_format(buf, s, r, c));
}

/* ---
*/
static LISP ldownshift_matrix(LISP bname, LISP row)
{
	buffer *buf;
	int r;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;
	r = get_c_long(row);

	downshift_matrix(buf, s, r);
	return NIL;
}

/* ---
*/
static LISP lupshift_matrix(LISP bname, LISP row)
{
	buffer *buf;
	int r;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;
	r = get_c_long(row);

	upshift_matrix(buf, s, r);
	return NIL;
}

/* ---
*/
static LISP lrightshift_matrix(LISP bname, LISP col)
{
	buffer *buf;
	int c;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;
	c = get_c_long(col);

	rightshift_matrix(buf, s, c);
	return NIL;
}

/* ---
*/
static LISP lleftshift_matrix(LISP bname, LISP col)
{
	buffer *buf;
	int c;
	int s;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;
	c = get_c_long(col);

	leftshift_matrix(buf, s, c);
	return NIL;
}

/* ---
*/
static LISP lcalc_matrix(LISP bname)
{
	buffer *buf;

	if (NULLP(bname)) {
		buf = buffer_of_window(w_list);
	} else {
		buf = find_buffer_by_name(get_c_string(bname));
	}
	if (!buf) return NIL;

	buf->recalc = 1;	/* mark for recalculation */
	return NIL;
}

/* ---
*/
static LISP lask_for_str(LISP prompt, LISP buf)
{
	char *p, b[256];

	strncpy(b, get_c_string(buf), 255);
	p = get_c_string(prompt);
	if (ask_for_str(p, b))
		return strcons(strlen(b), b);
	else
		return NIL;
}

/* ---
*/
static LISP ledit_cell(LISP prompt, LISP buf)
{
	char *p, b[256];

	strncpy(b, get_c_string(buf), 255);
	p = get_c_string(prompt);
	if (edit_cell(p, b)) {
		return strcons(strlen(b), b);
	} else {
		return NIL;
	}
}

extern char *psformat;	/* in fileio_ps.c */

/* ---
*/
static LISP lpsformat(void)
{
	return strcons(strlen(psformat), psformat);
}

/* ---
*/
static LISP lgrid_lines(LISP n)
{
	grid_lines = get_c_long(n);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP la1_refs(LISP n)
{
	int old = w_list->buf->a1_refs;
	w_list->buf->a1_refs = a1_refs = get_c_long(n);
	if (a1_refs != old) {
		w_list->buf->change = w_list->buf->recalc = pr_scr_flag = TRUE;
	}
	return NIL;
}

/* ---
*/
static LISP a1_refs_get(void)
{
	return flocons(w_list->buf->a1_refs);
}


/* ---
*/
static LISP lexecute(LISP cmd)
{
	execute(get_c_string(cmd));
	return NIL;
}

/* ---
*/
static LISP linput_warp_pointer(LISP value)
{
	input_warp_pointer = get_c_long(value);
	return NIL;
}

extern int interpreter_count;
extern char* intnames[];


/* ---
*/
static LISP interpreter_test(LISP lprompt, LISP intnam, LISP args)
{
	buffer *buf = buffer_of_window(w_list);
	position pos = get_point(w_list);
	int interpreter, type = ERROR;
	char prompt[80], b[1024], *p;
	char *i;
	cval val;
	int n;
	int s = w_list->sht;

	val.number = 0;
	if (NULLP(intnam)) {
		interpreter = select_from_list("Which interpreter?",
					intnames, interpreter_count);
	} else {
		interpreter = name2interpreter(get_c_string(intnam));
	}
	if (interpreter < 0) return NIL;

	if (NULLP(lprompt)) {
		sprintf(prompt, _("%s expression:"),
			interpreter2name(interpreter));
	} else {
		strcpy(prompt, _(get_c_string(lprompt)));
	}
Again:
	if (NULLP(args)) {
		p = ret_text(buf, s, pos.row, pos.col);
	} else {
		p = get_c_string(args);
	}
	b[0] = 0;
	if (p) strncpy(b, p, 1023);
	if ((n = edit_cell(prompt, b))) {
		int r = pos.row;
		int c = pos.col;
		if ((i = b) == 0) return NIL;
		undo_save(buf, s, r, c, r, c);
		ins_data(buf, interpreter, i, val, type, s, r, c);
		buf->change = TRUE;
		calc_matrix(buf);
		if (ret_type(buf, s, r, c) != ERROR) buf->recalc = 1;
		pr_scr_flag = TRUE;
	}
	if (n > 2) {
		pos.row++;
		set_point(w_list, pos);
		goto Again;
	}
	return NIL;
}

static LISP edit_unknown(LISP args)
{
	buffer *buf = buffer_of_window(w_list);
	position pos = get_point(w_list);
	int type = ERROR;
	char *prompt = _("Edit:"), b[1024], *p;
	char *i, *endp;
	cval val;
	int n;
	int s = w_list->sht;
	int interpreter;

	if (ret_type(buf, s, pos.row, pos.col) == EXPRESSION) {
		interpreter = ret_interpreter(buf, s, pos.row, pos.col);
	} else {
		interpreter =
		    name2interpreter(
			get_c_string(
			    symbol_value(
				cintern("*standard-interpreter*"),
				NIL)));
	}
	if (interpreter < 0) return NIL;

Again:
	if (NULLP(args)) {
		p = ret_text(buf, s, pos.row, pos.col);
	} else {
		p = get_c_string(args);
	}
	b[0] = 0;
	if (p) strncpy(b, p, 1023);
	if ((n = edit_cell(prompt, b))) {
		int r = pos.row;
		int c = pos.col;
		if ((i = b) == 0) return NIL;
		undo_save(buf, s, r, c, r, c);
		val.number = strtod(i, &endp);
		if (endp != i && only_space(endp)) {
			type = CONSTANT;
		}
		ins_data(buf, interpreter, i, val, type, s, r, c);
		buf->change = TRUE;
		calc_matrix(buf);
		if (ret_type(buf, s, r, c) == ERROR) {
			ins_data(buf, interpreter, i, val, LABEL, s, r, c);
		} else {
			buf->recalc = 1;
		}
		pr_scr_flag = TRUE;
	}
	if (n > 2) {
		pos.row++;
		set_point(w_list, pos);
		goto Again;
	}
	return NIL;
}


/* ---
*/
static LISP lundo_save(LISP r1, LISP c1, LISP r2, LISP c2)
{
	double retval = undo_save(buffer_of_window(w_list), w_list->sht,
				get_c_long(r1), get_c_long(c1),
				get_c_long(r2), get_c_long(c2));
	return flocons(retval);
}

/* ---
*/
static LISP lundo_restore(void)
{
	buffer *b = buffer_of_window(w_list);
	double retval = undo_restore(b);
	b->change = TRUE;
	calc_matrix(b);
	pr_scr_flag = TRUE;
	return flocons(retval);
}

#define MAGIC_MARKER ";;; Do not change or add anything below this line.\n"

/* ---
*/
static LISP save_preferences(void)
{
	char fn1[1024], fn2[1024];
	FILE *fp1, *fp2;
	char b[1024];

	sprintf(fn1, "%s/.siag/siag.scm", getenv("HOME"));
	sprintf(fn2, "%s.BAK", fn1);
	rename(fn1, fn2);
	fp2 = fopen(fn2, "r");
	fp1 = fopen(fn1, "w");
	if (!fp1) {
		rename(fn2, fn1);
		return NIL;
	}
	while (fp2 && fgets(b, sizeof b, fp2)) {
		if (!strcmp(b, MAGIC_MARKER)) break;
		fputs(b, fp1);
	}
	fputs(MAGIC_MARKER, fp1);
	fprintf(fp1, "(define *standard-interpreter* '%s)\n",
		get_c_string(symbol_value(cintern("*standard-interpreter*"),
					  NIL)));
	fprintf(fp1, "(grid-lines %d)\n", grid_lines);
	fprintf(fp1, "(tooltip-mode %ld)\n",
		get_c_long(symbol_value(cintern("*tooltip-mode*"), NIL)));
	fprintf(fp1, "(a1-refs-set %d)\n", a1_refs);
	fprintf(fp1, "(set! viewer-command \"%s\")\n",
		get_c_string(symbol_value(cintern("viewer-command"), NIL)));
	fprintf(fp1, "(set! lpr-command \"%s\")\n",
		get_c_string(symbol_value(cintern("lpr-command"), NIL)));

	if (fp2) fclose(fp2);
	fclose(fp1);
	return NIL;
}

/* ---
*/
static LISP lpack_area(LISP r1, LISP c1, LISP r2, LISP c2)
{
	int size;
	char *b;
	LISP result;

	b = pack_area(w_list->buf, w_list->sht, get_c_long(r1), get_c_long(c1),
		get_c_long(r2), get_c_long(c2), &size);
	result = strcons(size, b);
	MwFree(b);
	return result;
}

/* ---
*/
static LISP lunpack_area(LISP b, LISP r, LISP c)
{
	unpack_area(w_list->buf, get_c_string(b),
		w_list->sht, get_c_long(r), get_c_long(c));
	return NIL;
}

/* ---
*/
static LISP ladd_sheet(void)
{
	buffer *b = w_list->buf;
	int n = b->nsht;
	int m = buffer_add_sheet(b, n);
	pr_scr_flag = 1;
	b->change = 1;
	return flocons(m);
}

/* ---
*/
static LISP lremove_sheet(void)
{
	buffer *b = w_list->buf;
	int m;
	pr_scr_flag = 1;
	b->change = 1;
	m = buffer_remove_sheet(b, w_list->sht);
	if (w_list->sht >= m) {	/* removed last sheet */
		w_list->sht = m-1;
		pr_scr_flag = 1;
	}
	return flocons(m);
}

/* ---
*/
static LISP lrename_sheet(void)
{
	char name[1000];
	buffer *b = w_list->buf;

	strcpy(name, b->sht[w_list->sht].name);
	if (ask_for_str("Name:", name)) {
		buffer_rename_sheet(b, w_list->sht, name);
		pr_scr_flag = 1;
		b->change = 1;
	}
	return NIL;
}

/* ---
Move a sheet in the stack.
*/

static LISP lmove_sheet(LISP f, LISP t)
{
	int from = get_c_long(f);
	int to = get_c_long(t);
	buffer *b = w_list->buf;
	int nsht = b->nsht;
	sheet sht;
	int i;

	if (from < 0 || from >= nsht || to < -1 || to >= nsht)
		return NIL;	/* out of bounds */

	if (to == -1) to = nsht-1;

	sht = b->sht[from];	/* copy the sheet to be moved */

	for (i = from; i < nsht-1; i++) {
		b->sht[i] = b->sht[i+1];	/* close the gap */
	}

	for (i = nsht-1; i > to; i--) {
		b->sht[i] = b->sht[i-1];	/* make a new one */
	}

	b->sht[to] = sht;	/* copy sheet back */

	w_list->sht = to;

	pr_scr_flag = 1;
	b->change = 1;
	return NIL;
}

static LISP lget_sheet(void)
{
	return flocons(w_list->sht);
}

/* ---
*/
static LISP lregister_style(LISP n, LISP f, LISP t)
{
	return flocons(lookup_style(get_c_string(n),
					get_c_string(f),
					get_c_long(t)));
}

/* ---
Takes an associative list of the format
((name value) (name value) ... )
and turns it into an index into the format table.
*/
static LISP lencode_format(LISP f)
{
	MwFmt fmt;
	int result;
	MwDecodeFormat(0, ~0, &fmt);	/* put valid fields in fmt */
	while (NNULLP(f)) {
		char *name = get_c_string(CAR(CAR(f)));
		int mask = MwFmtAttrToMask(name);
		LISP value = CDR(CAR(f));
		f = CDR(f);
		switch (mask) {
		case MW_FMT_FAMILY:
			fmt.family = get_c_string(value);
			break;
		case MW_FMT_SIZE:
			fmt.size = get_c_long(value);
			break;
		case MW_FMT_FG:
			fmt.fg = get_c_string(value);
			break;
		case MW_FMT_BG:
			fmt.bg = get_c_string(value);
			break;
		case MW_FMT_BORDERS:
			fmt.borders = get_c_long(value);
			break;
		case MW_FMT_BOLD:
			fmt.bold = get_c_long(value);
			break;
		case MW_FMT_ITALIC:
			fmt.italic = get_c_long(value);
			break;
		case MW_FMT_ULINE:
			fmt.uline = get_c_long(value);
			break;
		case MW_FMT_HADJ:
			fmt.hadj = get_c_long(value);
			break;
		case MW_FMT_VADJ:
			fmt.vadj = get_c_long(value);
			break;
		case MW_FMT_STYLE:
			fmt.style = name2style(get_c_string(value));
			break;
		default:
			break;
		}
		
	}
	result = MwEncodeFormat(~0, &fmt);
	return flocons(result);
}

static LISP SS(char *name, char *value)
{
	return cons(strcons(strlen(name), name),
		    strcons(strlen(value), value));
}

static LISP SL(char *name, int value)
{
	return cons(strcons(strlen(name), name),
		    flocons(value));
}

/* ---
Takes an index into the format table and turns it into an
associative list of the format
((name value) (name value) ... )
*/
static LISP ldecode_format(LISP f)
{
	MwFmt fmt;
	MwDecodeFormat(get_c_long(f), ~0, &fmt);
	return cons(SS("family", fmt.family),
		 cons(SL("size", fmt.size),
		   cons(SS("fg", fmt.fg),
		     cons(SS("bg", fmt.bg),
		       cons(SL("bold", fmt.bold),
			 cons(SL("italic", fmt.italic),
			   cons(SL("uline", fmt.uline),
			     cons(SL("hadj", fmt.hadj),
			       cons(SL("vadj", fmt.vadj),
				 cons(SL("borders", fmt.borders),
				   cons(SS("style", style2name(fmt.style)),
				      NIL)))))))))));
}

/* ---
*/
static LISP lselect_attribute(LISP attr)
{
	char *name = get_c_string(attr);
   	int choice = -1;
	char **list = NULL;
	int nlist = 0;

	if (!MwStrcasecmp(name, "family"))
     		list = MwFontList(&nlist);
	else if (!MwStrcasecmp(name, "style"))
		list = style_list(&nlist);
	else if (!MwStrcasecmp(name, "fg") || !MwStrcasecmp(name, "bg"))
		list = MwColorList(&nlist);

	if (list) {
		char prompt[250];
	   	sprintf(prompt, _("Change %s:"), name);
		choice = select_from_list(prompt, list, nlist);
	}
	if (choice < 0) return NIL;
	return strcons(strlen(list[choice]), list[choice]);
}

/* ---
start of experimental swallowing code
originally from TkSteal
*/

static LISP lplugin_register(LISP desc, LISP ext, LISP cmd)
{
	plugin_register(get_c_string(desc),
			get_c_string(ext),
			get_c_string(cmd));
	return NIL;
}

static void insert_plugin(int mode, char *plugin_name)
{
	static char path[1024], name[1024];
	char fn[1024];
	char pn[1024];
	char cmd[1024];
	char fmt[80];
	char p[1024];
	buffer *buf = w_list->buf;
	int n;
	plugin_t plugin;

	/* ask for file name */
	if (path[0] == '\0') getcwd(path, 1024);
	name[0] = fn[0] = '\0';

	if (plugin_name) {
		char *q = strrchr(plugin_name, '/');
		if (q) strcpy(name, q+1);
		else strcpy(name, plugin_name);
		strcpy(fn, plugin_name);
	} else {
		if (!select_file(path, name, plugin_patterns, fmt, 0)) return;
		sprintf(fn, "%s/%s", path, name);
	}

	plugin_unique_name(name, pn);

        plugin.row = get_point(w_list).row;
        plugin.col = get_point(w_list).col;
        plugin.displayed = 0;

	if (mode == PLUGIN_COPY) {
		plugin.name = MwStrdup(pn);
		/* copy the file */
		plugin_basedir(p, buf->name);
		sprintf(pn, "%s/%s", p, plugin.name);
		sprintf(cmd, "(mkdir %s;cp %s %s)2>/dev/null", p, fn, pn);
		system(cmd);
	} else {
		strcpy(pn, fn);
		plugin.name = MwStrdup(pn);
	}

	/* start the plugin */
	plugin.ph = plugin_start(pn);
	if (plugin.ph == -1) return;

	/* prepare the buffer */
	n = buf->sht[w_list->sht].nplugin++;
	buf->sht[w_list->sht].plugin = (plugin_t *)MwRealloc(buf->sht[w_list->sht].plugin,
		buf->sht[w_list->sht].nplugin*sizeof(plugin_t));
	buf->sht[w_list->sht].plugin[n] = plugin;
	buf->change = TRUE;
        pr_scr_flag = TRUE;
}

static LISP lplugin_import(LISP pn)
{
	if (NULLP(pn)) insert_plugin(PLUGIN_COPY, NULL);
	else insert_plugin(PLUGIN_COPY, get_c_string(pn));
	return NIL;
}

/* ---
broken because it still takes the name relative to basedir
*/

static LISP lplugin_link(LISP pn)
{
	if (NULLP(pn)) insert_plugin(PLUGIN_LINK, NULL);
        else insert_plugin(PLUGIN_LINK, get_c_string(pn));
        return NIL;
}

/* ---
return buffer's plug handler index or -1 for error
*/

static int select_plugin(buffer *b, int s)
{
	char *plugin[20];
	int i;
	if (b->sht[s].nplugin == 0) return -1;
	if (b->sht[s].nplugin == 1) return 0;
	for (i = 0; i < 20 && i < b->sht[s].nplugin; i++)
		plugin[i] = b->sht[s].plugin[i].name;
	i = select_from_list("Select Plugin:", plugin, i);
	return i;
}

static LISP lplugin_select(void)
{
	return flocons(select_plugin(w_list->buf, w_list->sht));
}

static LISP lplugin_export(void)
{
	int n;
	char p[1024], fn[1024], pn[1024], cmd[1024], path[1024], fmt[80];
	buffer *buf = w_list->buf;
	int s = w_list->sht;

	/* pick plugin from list */
	n = select_plugin(buf, s);
	if (n == -1) return NIL;

	/* get description of the handler */
	/* ask for file name */
	getcwd(path, 1024);
	strcpy(fn, buf->sht[s].plugin[n].name);
	if (!select_file(path, fn, NULL, fmt, 1)) return NIL;

	/* save the file */
	sprintf(pn, "%s/%s", plugin_basedir(p, buf->name),
		buf->sht[s].plugin[n].name);
	sprintf(cmd, "cp %s %s/%s", pn, path, fn);
	system(cmd);
	return NIL;
}

static LISP lplugin_delete(void)
{
	int n;
	buffer *buf = w_list->buf;
	int s = w_list->sht;

	/* pick plugin from list */
	n = select_plugin(buf, s);
	if (n == -1) return NIL;

	/* remove plugin */
	plugin_stop(buf->sht[s].plugin[n].ph);
	MwFree(buf->sht[s].plugin[n].name);
	buf->sht[s].nplugin--;
	for (; n < buf->sht[s].nplugin; n++)
		buf->sht[s].plugin[n] = buf->sht[s].plugin[n+1];
	buf->change = pr_scr_flag = TRUE;
	return NIL;
}

static LISP lplugin_move(void)
{
	int n;
	buffer *buf = w_list->buf;
	int s = w_list->sht;

	/* pick plugin from list */
	n = select_plugin(buf, s);
	if (n == -1) return NIL;

	buf->sht[s].plugin[n].row = get_point(w_list).row;
	buf->sht[s].plugin[n].col = get_point(w_list).col;
	buf->change = pr_scr_flag = TRUE;
	return NIL;
}

static LISP lplugin_resize(void)
{
	int n;
	buffer *buf = w_list->buf;
	int s = w_list->sht;
	int oldw, oldh, neww, newh;
	char b[100];

	n = select_plugin(buf, s);
	if (n == -1) return NIL;

	if (plugin_size_get(n, &oldw, &oldh) == -1) return NIL;

	sprintf(b, "%d", oldw);
	if (ask_for_str("Width:", b) == 0) return NIL;
	if (sscanf(b, "%d", &neww) < 1) return NIL;
	if (neww <= 0 || neww >= 1000) return NIL;

	sprintf(b, "%d", oldh);
	if (ask_for_str("Height:", b) == 0) return NIL;
	if (sscanf(b, "%d", &newh) < 1) return NIL;
	if (newh <= 0 || newh >= 1000) return NIL;

	plugin_size_set(n, neww, newh);
	return NIL;
}

/* ---
Returns number of bytes written if successful, otherwise NIL.
*/

static LISP lplugin_write(LISP ph, LISP text)
{
	int p = get_c_long(ph);
	char *t = get_c_string(text);
	int n = plugin_write(p, t);
	if (n) return flocons(n);
	else return NIL;
}

/* ---
Returns string read if succesful, otherwise NIL.
*/

static LISP lplugin_read(LISP ph)
{
	char b[1024];
	int p = get_c_long(ph);
	int n = plugin_read(p, b);
	if (n) return strcons(strlen(b), b);
	else return NIL;
}

/* ---
Get current paper name, size and orientation. Gets from named buffer
if b is a string, from the current buffer is b is "" and from the
global defaults if b is NIL.

Returns (name width height orientation)
*/

static LISP lpaper_get(LISP b)
{
	char *n;
	int w, h, o;

	if (NULLP(b)) {
		n = paper_name;
		w = paper_width;
		h = paper_height;
		o = orientation;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		n = buf->paper_name;
		w = buf->paper_width;
		h = buf->paper_height;
		o = buf->orientation;
	}
	return cons(strcons(strlen(n), n),
		 cons(flocons(w),
		   cons(flocons(h),
		     cons(flocons(o), NIL))));
}

/* ---
Set current paper name, size and orientation. Arguments as above.
*/

static LISP lpaper_set(LISP b, LISP p)
{
	char *n;
	int w, h, o;

	n = MwStrdup(get_c_string(CAR(p)));
	w = get_c_long(CAR(CDR(p)));
	h = get_c_long(CAR(CDR(CDR(p))));
	o = get_c_long(CAR(CDR(CDR(CDR(p)))));
	if (NULLP(b)) {
		paper_name = n;
		paper_width = w;
		paper_height = h;
		orientation = o;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		buf->paper_name = n;
		buf->paper_width = w;
		buf->paper_height = h;
		buf->orientation = o;
		buf->change = pr_scr_flag = TRUE;
		buffer_cleanup(buf);	/* doesn't do anything yet */
	}
	return NIL;
}


/* ---
Get current top, bottom, left, right, header and footer margins.
Gets from global defaults if b is nil, from the current buffer if
b is "" and from the named buffer if b is any other string.

Returns (top bottom left right header footer)
*/

static LISP lmargin_get(LISP b)
{
	int tm, bm, lm, rm, hm, fm;

	if (NULLP(b)) {
		tm = top_margin;
		bm = bottom_margin;
		lm = left_margin;
		rm = right_margin;
		hm = header_margin;
		fm = footer_margin;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		tm = buf->top_margin;
		bm = buf->bottom_margin;
		lm = buf->left_margin;
		rm = buf->right_margin;
		hm = buf->header_margin;
		fm = buf->footer_margin;
	}
	return cons(flocons(tm),
		 cons(flocons(bm),
		   cons(flocons(lm),
		     cons(flocons(rm),
		       cons(flocons(hm),
			 cons(flocons(fm), NIL))))));
}

/* ---
Set current top, bottom, left, right, header and footer margins.
Arguments as above.
*/

static LISP lmargin_set(LISP b, LISP m)
{
	int tm, bm, lm, rm, hm, fm;

	tm = get_c_long(CAR(m));
	bm = get_c_long(CAR(CDR(m)));
	lm = get_c_long(CAR(CDR(CDR(m))));
	rm = get_c_long(CAR(CDR(CDR(CDR(m)))));
	hm = get_c_long(CAR(CDR(CDR(CDR(CDR(m))))));
	fm = get_c_long(CAR(CDR(CDR(CDR(CDR(CDR(m)))))));
	if (NULLP(b)) {
		top_margin = tm;
		bottom_margin = bm;
		left_margin = lm;
		right_margin = rm;
		header_margin = hm;
		footer_margin = fm;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		buf->top_margin = tm;
		buf->bottom_margin = bm;
		buf->left_margin = lm;
		buf->right_margin = rm;
		buf->header_margin = hm;
		buf->footer_margin = fm;
		buf->change = pr_scr_flag = TRUE;
		buffer_cleanup(buf);
	}
	return NIL;
}

/* ---
Get current header and footer strings. Get global defaults if b is nil.
Get from current buffer if b is "" and from named buffer otherwise.

Returns (header footer)
*/

static LISP lheadfoot_get(LISP b)
{
	char *h, *f;
	int fp;

	if (NULLP(b)) {
		h = header;
		f = footer;
		fp = header_on_first;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		h = buf->header;
		f = buf->footer;
		fp = buf->header_on_first;
	}
	return cons(strcons(strlen(h), h),
		 cons(strcons(strlen(f), f),
		   cons(flocons(fp), NIL)));
}

/* ---
Set current header and footer strings. Args as above.
*/

static LISP lheadfoot_set(LISP b, LISP hf)
{
	char *h, *f;
	int fp;

	h = MwStrdup(get_c_string(CAR(hf)));
	f = MwStrdup(get_c_string(CAR(CDR(hf))));
	fp = get_c_long(CAR(CDR(CDR(hf))));
	if (NULLP(b)) {
		header = h;
		footer = f;
		header_on_first = fp;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		buf->header = h;
		buf->footer = f;
		buf->header_on_first = fp;
		buf->change = pr_scr_flag = TRUE;
	}
	return NIL;
}

static LISP lfirst_page_get(LISP b)
{
	int n;

	if (NULLP(b)) {
		n = first_page_number;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		n = buf->first_page_number;
	}
	return flocons(n);
}

static LISP lfirst_page_set(LISP b, LISP no)
{
	int n = get_c_long(no);

	if (NULLP(b)) {
		first_page_number = n;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		buf->first_page_number = n;
		buf->change = pr_scr_flag = TRUE;
	}
	return NIL;
}

static LISP lrespect_prot_get(LISP b)
{
	int n;

	if (NULLP(b)) {
		n = respect_protection;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		n = buf->respect_protection;
	}
	return flocons(n);
}

static LISP lrespect_prot_set(LISP b, LISP no)
{
	int n = get_c_long(no);

	if (NULLP(b)) {
		respect_protection = n;
	} else {
		buffer *buf;
		char *bn = get_c_string(b);
		if (bn[0]) buf = find_buffer_by_name(bn);
		else buf = w_list->buf;
		if (!buf) return NIL;
		buf->respect_protection = n;
		buf->change = pr_scr_flag = TRUE;
	}
	return NIL;
}

static LISP lmake_backups(LISP p)
{
	make_backups = get_c_long(p);
	return NIL;
}

static LISP lTRACEME(LISP foo)
{
	fprintf(stderr, "%s\n", get_c_string(foo));
	MW_TRACE((f, "%s", get_c_string(foo)));
	return NIL;
}

static LISP lset_zoom(LISP n)
{
	zoom = get_c_double(n);
	if (zoom < .1) zoom = .1;
	if (zoom > 10) zoom = 10;
	pr_scr_flag = 1;
	return NIL;
}

/* Commands that take no arguments */
static s_fn_table0 fn_table[] =
{
	/* moving around */
	{"go-to", go_to},
	{"cmalloc_stats", lcmalloc_stats},
	{"set-standard-width", set_standard_width},
	{"set-standard-height", set_standard_height},
	{"set-standard-format", set_standard_format},
	{"set-cell-width", set_cell_width},
	{"set-cell-height", set_cell_height},
/*	{"set-cell-format", set_cell_format},
	{"set-block-format", set_block_format},
*/
	{"set-block-style", set_block_style},
	{"set-cell-style", set_cell_style},
	{"define-style", define_style},

	/* block commands */
	{"fill-block", fill_block},
	{"smart-fill-block", smart_fill_block},
	/* new window */
	{"delete-window", delete_window},
	{"delete-other-windows", delete_other_windows},
	{"split-window-vertically", split_window_vertically},
	{"other-window", other_window},

	/* buffers and windows */
	{"switch-to-buffer", switch_to_buffer},
	{"load-buffer", load_buffer},
	{"save-buffer", save_buffer},
	{"save-buffer-as", save_buffer_as},
	{"load-external", load_external},
	{"save-external", save_external},

	/* help commands */
	{"print-version", print_version},

	/* misc. */
	{"execute-extended-command", execute_extended_command},
	{"quit-program", quit_siag},
	{"undo-restore", lundo_restore},
	{"save-preferences", save_preferences},
	{"a1-refs-get", a1_refs_get},

	/* keyboard macros */

	/* low level functions */
	{"psformat", lpsformat},
	{"add-sheet", ladd_sheet},
	{"remove-sheet", lremove_sheet},
	{"rename-sheet", lrename_sheet},
	{"get-sheet", lget_sheet},

	{(char *) 0, (LISP(*)(void))0}
};

/* Commands that take 1 argument */

static s_fn_table1 fn_table1[] = {
	{"get-cell-width", get_cell_width},
	{"get-cell-height", get_cell_height},
	{"auto-recalc", auto_recalc},
	{"block-borders", block_borders},

	{"spawn", lspawn},
	{"execute-interpreter-command", execute_interpreter_command},

	/* low level functions */
	{"kill-buffer", kill_buffer},
	{"buffer-changed", buffer_changed},
	{"calc-matrix", lcalc_matrix},
	{"grid-lines", lgrid_lines},
	{"a1-refs-set", la1_refs},
	{"execute", lexecute},
	{"input-warp-pointer", linput_warp_pointer},

	{"confirm-overwrite", lconfirm_overwrite},
	{"decode-format", ldecode_format},
	{"encode-format", lencode_format},
	{"select-attribute", lselect_attribute},
	{"paper-get", lpaper_get},
	{"margin-get", lmargin_get},
	{"headfoot-get", lheadfoot_get},
	{"first-page-get", lfirst_page_get},
	{"respect-prot-get", lrespect_prot_get},
	{"make-backups", lmake_backups},
	{"TRACEME", lTRACEME},
	{"set-zoom", lset_zoom},
	{NULL, NULL}
};

/* ---
Set up the table of functions and names
*/

void init_cmds(void)
{
	int i;

	for (i = 0; fn_table[i].name; i++)
		init_subr_0(fn_table[i].name, fn_table[i].function);
	for (i = 0; fn_table1[i].name; i++)
		init_subr_1(fn_table1[i].name, fn_table1[i].function);
	init_subr_3("interpreter-test", interpreter_test);
	init_subr_1("edit-unknown", edit_unknown);
	init_subr_2("downshift-matrix", ldownshift_matrix);
	init_subr_2("upshift-matrix", lupshift_matrix);
	init_subr_2("rightshift-matrix", lrightshift_matrix);
	init_subr_2("leftshift-matrix", lleftshift_matrix);
	init_subr_2("ask-for-str", lask_for_str);
	init_subr_2("edit-cell", ledit_cell);
	init_subr_2("alertbox", lalertbox);
	init_subr_0("aboutbox", laboutbox);
	init_subr_0("aboutsiag", laboutsiag);
	init_subr_3("set-format", set_format);
	init_subr_2("get-format", get_format);
	init_subr_5("set-data", set_data);
	init_subr_5("swap-cells", lswap_cells);
	init_subr_3("savematrix", lsavematrix);
	init_subr_4("undo-save", lundo_save);
	init_subr_2("deletia-add", ldeletia_add);
	init_subr_4("pack-area", lpack_area);
	init_subr_3("unpack-area", lunpack_area);
	init_subr_3("register-style", lregister_style);
	init_subr_2("move-sheet", lmove_sheet);
	init_subr_3("plugin-register", lplugin_register);
	init_subr_0("plugin-select", lplugin_select);
	init_subr_1("plugin-import", lplugin_import);
	init_subr_0("plugin-export", lplugin_export);
	init_subr_1("plugin-link", lplugin_link);
	init_subr_0("plugin-delete", lplugin_delete);
	init_subr_0("plugin-move", lplugin_move);
	init_subr_0("plugin-resize", lplugin_resize);
	init_subr_2("plugin-write", lplugin_write);
	init_subr_1("plugin-read", lplugin_read);
	init_subr_2("paper-set", lpaper_set);
	init_subr_2("margin-set", lmargin_set);
	init_subr_2("headfoot-set", lheadfoot_set);
	init_subr_2("first-page-set", lfirst_page_set);
	init_subr_2("respect-prot-set", lrespect_prot_set);
}

/* begin stuff copied from calc-cmds.c */

static int is_prefix;   /* indicate if lastc is part of a prefix */
static int prefix_length;
static char prefix_keys[256];
int lastc;	/* remove from window.c */

typedef struct s_kbd_table {
  int length;
  char *keys;
  char *function;       /* the name of the function */
} kbd_table;

static kbd_table keymap[1000];

static int argument = 0, keep_argument;
/* argument is reset after each command that doesn't set keep_argument */

static void install(char *k, int l, char *s)
{
  int i, j;

  for (i = 0; keymap[i].keys != NULL; i++) {
    if (keymap[i].length == l)
      if (!memcmp(k, keymap[i].keys, l)) {
	if (keymap[i].function != NULL) free(keymap[i].function);
	keymap[i].function = MwStrdup(s);
	return;
      }
  }
  keymap[i].length = l;
  keymap[i].keys = MwMalloc(l);
  for (j = 0; j < l; j++) keymap[i].keys[j] = k[j];
  keymap[i].function = MwStrdup(s);
}

/* ---
	char *k;   the keypresses
	int l;     the # of keypresses
*/

static char *get_command(char *k, int l)
{
  int i;

  for (i = 0; keymap[i].keys != NULL; i++) {
    if (keymap[i].length == l)
      if (!memcmp(keymap[i].keys, k, l)) break;
  }
  return keymap[i].function;
}

static int keycode(char *p)
{
  static struct {
    char *t;
    int c;
  } spec[] = {
    {"LFD",     '\n'},
    {"RET",     '\r'},
    {"TAB",     '\t'},
    {"ESC",     ESC},
    {"SPC",     ' '},
    {"DEL",     DEL},
    {(char *)0, 0}};
  
  int i;
  
  if (p == NULL) return -1;
  
  switch (strlen(p)) {
  case 1:
    if (isprint(p[0])) return p[0];
    break;
  case 3:
    for (i = 0; spec[i].t != NULL; i++)
      if (!strcmp(p, spec[i].t)) return spec[i].c;
    if (p[0] == 'C' && p[1] == '-' && isprint(p[2])) return CTRL(p[2]);
    if (p[0] == 'M' && p[1] == '-' && isprint(p[2])) return ALT(p[2]);
  default:
    break;
  }
  return -1;
}

static void decode(char *kbd)
{
  int i, is_comment;
  char *keys, *cmd, p[256], *q;
  char kseq[256];

  if (kbd[0] == '\t') return;
  strcpy(p, kbd);

  is_comment = FALSE;
  if ((keys = strtok(p, "\t")) != NULL) {
    cmd = strtok((char *)0, "\t\n");
    for (q = strtok(keys, " "), i = 0;
	 q != NULL;
	 q = strtok((char *)0, " "), i++)
      is_comment |= ((kseq[i] = keycode(q)) == -1);
    if (!is_comment) install(kseq, i, cmd);
  }
}

/* ---
static void start_kbd_macro(void)

Starts recording a keyboard macro.  From now on, everything that is
typed is also recorded into a buffer.  There is a limit to the size of
the buffer, but the limit is big enough for any reasonable use.
*/

static LISP start_kbd_macro(void)
{
  if (macro_flag) {
    macro_flag = FALSE;
    llpr("Already defining kbd macro!");
  }
  else {
    macro_flag = TRUE;
    kbd_macro.size = 0;
    llpr("Defining kbd macro...");
  }
  return NIL;
}

/* ---
static void end_kbd_macro()

Stops recording the macro.  This command must be bound to a key sequence,
because it actually gets recorded before it is invoked.  It removes itself
from the macro by deleting as many charactesr as the key sequence that
invoked it.  Typing "M-x end-kbd-macro" will only delete the last two
characters from the buffer.
*/

static LISP end_kbd_macro(void)
{
  if (!macro_flag) llpr("Not defining kbd macro");
  else {
    macro_flag = FALSE;
    kbd_macro.size -= prefix_length; /* strip off the closing C-x ) */
    llpr("Keyboard macro defined");
  }
  return NIL;
}

extern int add_str_to_input_queue(textbuf);	/* FIXME */

/* ---
static void call_last_kbd_macro()

Executes the last macro that was recorded with C-x (.
There can only be one macro defined at a time.
*/

static LISP call_last_kbd_macro(void)
{
  if (macro_flag) {
    llpr("Can't execute anonymous macro while defining one");
    macro_flag = FALSE;
  }
  do {
    if (!add_str_to_input_queue(kbd_macro))
      llpr("Input buffer overflow; macro execution terminated");
  } while (--argument > 0);
  return NIL;
}

/* ---
*/
static LISP prefix_command(void)
{
  is_prefix = keep_argument = TRUE;
  return NIL;
}

/* ---
*/
static LISP universal_argument(void)
{
  if (argument) argument *= 4;
  else argument = 4;
  keep_argument = TRUE;
  return NIL;
}

/* ---
*/
static LISP argument_digit(void)
{
  argument = 10*argument+sign(argument)*todigit(lastc);
  keep_argument = TRUE;
  return NIL;
}

/* ---
*/
static LISP argument_sign(void)
{
  argument = -argument;
  keep_argument = TRUE;
  return NIL;
}

/* ---
*/
static LISP add_keybinding(LISP keys, LISP cmd)
{
	char *k = get_c_string(keys);
	char *c = get_c_string(cmd);
	int i = 0;
	char kseq[256], p[256], *q;
	strcpy(p, k);
	for (q = strtok(p, " "); q; q = strtok(NULL, " "))
		if ((kseq[i++] = keycode(q)) == -1) return NIL;
	install(kseq, i, c);
	return NIL;
}


static s_fn_table0 calc_fn_table[] = {
  {"start-kbd-macro", 		start_kbd_macro},
  {"end-kbd-macro",		end_kbd_macro},
  {"call-last-kbd-macro",	call_last_kbd_macro},
  {"universal-argument",        universal_argument},
  {"argument-digit",            argument_digit},
  {"argument-sign",             argument_sign},

  /* misc. */
  {"prefix",                    prefix_command},
  {"execute-extended-command",  execute_extended_command},
  {"quit",                      quit},
  {NULL,                        NULL}
};

/* ---
Set up the table of keycodes and functions
*/

void init_calc_cmds(void)
{
  	int i;

    	decode("C-x\t(prefix)\n");
    	decode("C-x C-c\t(quit-program)\n");

	for (i = 0; calc_fn_table[i].name; i++)
                init_subr_0(calc_fn_table[i].name, calc_fn_table[i].function);
	init_subr_2("add-keybinding", add_keybinding);
}

/* ---
*/
void do_cmd(int c)
{
  char *cmd;

  	if (!is_prefix) prefix_length = 0;
  	prefix_keys[prefix_length++] = c;
  	is_prefix = keep_argument = FALSE;
  	if ((cmd = get_command(prefix_keys, prefix_length))) {
	  	execute(cmd);
	} else {
		char b[100];
		sprintf(b, "(edit-unknown \"%c\")", c);
		execute(b);
	}
  	if (!keep_argument) argument = 0;
}

