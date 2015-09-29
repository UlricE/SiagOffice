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

/* ---
      cmds.c

      This rather bulky module contains all the functions that implement
      commands.  It also handles initialization of the interface to those
      functions.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../common/plugin.h"
#include "../siod/siod.h"

#include "egon.h"
#include "user_interface.h"

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

/* moving around */

static char *testlist[] = {	"testitem0",
	"testitem1", "testitem2", "testitem3", "testitem4", "testitem5",
	"testitem6", "testitem7", "testitem8", "testitem9"
};

/* ---
*/
static LISP listsel_test(void)
{
	int i = select_from_list("Choose an item", testlist, 10);
	if (i < 0)
		printf("User clicked Cancel\n");
	else
		printf("User chose %d\n", i);
	return NIL;
}

/* ---
*/
static LISP what_cursor_position(void)
{
	return NIL;
}

/* ---
*/
static LISP lspawn(LISP command)
{
	MwSpawn(get_c_string(command));
	return NIL;
}

/* Windows and buffers */

/* ---
*/
static LISP ask_for_file(void)
{
	static char path[1024], name[1024];
	char fn[1024];
	char fmt[80];
	if (path[0] == '\0')
		getcwd(path, 1024);
	name[0] = fn[0] = '\0';
	if (!select_file(path, name, NULL, fmt, 0))
		return NIL;
	sprintf(fn, "%s/%s", path, name);
	return strcons(strlen(fn), fn);
}

/* ---
   Load a buffer from file.
*/

static LISP load_buffer(void)
{
	static char path[1024], name[1024];
	char fn[1024];
	char fmt[80];
	buffer *b;
	static int need_init = 1;

	if (need_init) {
		getcwd(path, 1024);
		need_init = 0;
	}
	strcpy(name, "");
	fn[0] = '\0';
	if (select_file(path, name, loader_patterns, fmt, 0)) {
		sprintf(fn, "%s/%s", path, name);
		b = new_buffer(buffer_name(fn), fn);
		llpr("Loading");

		w_list->buf = b;
		activate_window(w_list);
	/* loadmatrix doesn't use the buffer argument in Egon,
	   the Scheme code is simply loaded and executed */
		if (loadmatrix(fn, b, fmt))
			llpr("New file");
	/* this will fail if load returns 1 because the string pool is full */

		b->change = FALSE;
		w_list->buf = b;
		w_list->sht = 0;
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

		sprintf(fnl, "%s/egon/external.load", datadir);
		if ((fp = fopen(fnl, "r")) == NULL) {
			llpr("Can't open header file");
			return NIL;
		}
		while (!feof(fp) && loaders < 20) {
			fgets(b, 250, fp);
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
		sprintf(fn, "%s/egon%ld", siag_tmpdir, (long)getpid());
		sprintf(cmd, "%s %s > %s", loadprog[i], param, fn);
		if (system(cmd)) {
			llpr("External program failed");
			return NIL;
		}

		b = new_buffer(buffer_name(fn), fn);
		llpr("Loading");

		if (loadmatrix(fn, b, NULL))
			llpr("New file");

		b->change = FALSE;
		w_list->buf = b;
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
	while (remove_window(w_list->next));
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
	activate_window(w_list->next);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
   Save the buffer in the currently active window to file.
*/

static LISP save_buffer(void)
{
	llpr("Saving");

	if (savematrix(w_list->buf->path, w_list->buf, NULL))
		error_box("Couldn't save");
	else {
		w_list->buf->change = FALSE;
		pr_scr_flag = TRUE;
	}
	return NIL;
}

static LISP lsavematrix(LISP path, LISP bname, LISP format)
{
	char *p, *fmt;
	buffer *b;

	if (NULLP(bname)) b = w_list->buf;
	else b = find_buffer_by_name(get_c_string(bname));
	if (!b) {
		llpr("No such buffer");
		return NIL;
	}
	p = get_c_string(path);
	if (NULLP(format)) fmt = guess_file_format(p);
	else fmt = get_c_string(format);

	if (savematrix(p, b, fmt)) {
		llpr("File saved");
		return NIL;
	} else {
		return a_true_value();
	}
}

/* ---
   Save the buffer in the currently active window to a named file.
*/

static LISP save_buffer_as(void)
{
	static char path[1024], name[1024];
	char fn[1024];
	char fmt[80];
	char *p;
	static int need_init = 1;

	if (need_init) {
		getcwd(path, 1024);
		need_init = 0;
	}
	p = strrchr(w_list->buf->path, '/');
	if (p) strcpy(name, p+1);
	else strcpy(name, w_list->buf->path);
	fn[0] = '\0';
	if (select_file(path, name, saver_patterns, fmt, 1)) {
		sprintf(fn, "%s/%s", path, name);
		llpr("Saving");
		if (savematrix(fn, w_list->buf, fmt))
			error_box("Couldn't save");
		else {
			w_list->buf->change = FALSE;
			strcpy(w_list->buf->path, fn);
		}
		pr_scr_flag = TRUE;
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

		sprintf(fnl, "%s/egon/external.save", datadir);
		if ((fp = fopen(fnl, "r")) == NULL) {
			llpr("Can't open saver file");
			return NIL;
		}
		while (!feof(fp) && savers < 20) {
			fgets(b, 250, fp);
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

		sprintf(fn, "%s/egon%ld", siag_tmpdir, (long)getpid());
		if (savematrix(fn, w_list->buf, NULL)) {
			error_box("Couldn't save");
			return NIL;
		}
		sprintf(cmd, "%s %s < %s", program, param, fn);
		if (system(cmd)) {
			llpr("External program failed");
			return NIL;
		}
	}
	return NIL;
}

/* ---
*/
static LISP switch_to_buffer(void)
{
	buffer *b;
	char *blist[100];
	int nblist = 0, n;

	b = w_list->buf;
	do {
		b = b->next;
		blist[nblist++] = b->name;
	} while (b != w_list->buf);
	if ((n = select_from_list("Change Buffer:", blist, nblist)) >= 0)
		w_list->buf = find_buffer_by_name(blist[n]);
	activate_window(w_list);
	return NIL;
}

/* ---
*/
static LISP kill_buffer(void)
{
	buffer *b, *next_b;
	window *w;
	char *blist[100];
	int nblist = 0, n;

	b = w_list->buf;
	do {
		b = b->next;
		blist[nblist++] = b->name;
	} while (b != w_list->buf);
	if ((n = select_from_list("Kill Buffer:", blist, nblist)) >= 0) {
		if ((b = find_buffer_by_name(blist[n])) != NULL) {
			if (b != b->next) {
				next_b = free_buffer(b);
				w = w_list;
				do {
					if (w->buf == b)
						w->buf = next_b;
					w = w->next;
				} while (w != NULL && w != w_list);
				pr_scr_flag = TRUE;
			}
			else llpr("Couldn't kill last buffer");
		}
	}
	activate_window(w_list);
	return NIL;
}


extern char *psformat;

static LISP lpsformat(void)
{
	return strcons(strlen(psformat), psformat);
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
	sprintf(prompt, "%s command: ", intname);
	b[0] = '\0';
	if (ask_for_str(prompt, b))
		exec_expr(intp, b);
	return NIL;
}

static char *quit_buttons[] = {"Yes", "No", "Cancel"};

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
		_("An animation program for X"),
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
static LISP quit_egon(void)
{
	char prompt[256], cmd[1024];
	buffer *b = b_list;
	do {
		if (b->change) {
			sprintf(prompt, "Save %s?", b->name);
			switch (alert_box(prompt, quit_buttons, 3)) {
			case 0:
				savematrix(b->path, b, NULL);
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

	if (NULLP(bname)) buf = w_list->buf;
	else buf = find_buffer_by_name(get_c_string(bname));

	if (buf) buf->change = TRUE;
	return NIL;
}

/* ---
*/
static LISP set_format(LISP format)
{
	ins_format(w_list->buf, w_list->object, w_list->script,
			get_c_long(format));
	return NIL;
}

/* ---
*/
static LISP get_format(void)
{
	long fmt = ret_format(w_list->buf, w_list->object, w_list->script);
	return flocons(fmt);
}

/* These functions allow implementation of commands in Scheme
	rather than in C with Scheme wrappers */

/* ---
*/
static LISP lask_for_str(LISP prompt, LISP buf)
{
	char *p, b[256];

	strcpy(b, get_c_string(buf));
	p = get_c_string(prompt);
	if (ask_for_str(p, b))
		return strcons(strlen(b), b);
	else
		return NIL;
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

#define MAGIC_MARKER ";;; Do not change or add anything below this line.\n"

/* ---
*/
static LISP save_preferences(void)
{
        char fn1[1024], fn2[1024];
        FILE *fp1, *fp2;
        char b[1024];

        sprintf(fn1, "%s/.siag/egon.scm", getenv("HOME"));
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
        fprintf(fp1, "(tooltip-mode %ld)\n",
                get_c_long(symbol_value(cintern("*tooltip-mode*"), NIL)));

        if (fp2) fclose(fp2);
        fclose(fp1);
        return NIL;
}

/* Beginning of animation functions */

/* ---
*/
static LISP ani_c_test(void)
{
	return NIL;
}

/* ---
*/
static LISP ani_begin(void)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	sheet *s = &b->sht[sht];
	/* should free old objects first */
	s->cast = NULL;
	s->delta = 100;
	s->duration = 1000;
	b->width = 600;
	b->height = 400;
	s->bg = NULL;
	b->change = TRUE;
	w_list->object = NULL;
	w_list->script = NULL;
	pr_scr_flag = TRUE;
	return NIL;
}


/* ---
*/
static LISP select_object(LISP name)
{
	char *string = get_c_string(name);

	int sht = w_list->sht;
	MwAniObject *o = w_list->buf->sht[sht].cast;

	while (o) {
		if (!strcmp(o->name, string)) break;
		o = o->next;
	}
	if (!o) {
		llpr("No such object");
	} else {
		w_list->object = o;
		w_list->script = o->script;
	}
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP select_tick(LISP tick)
{
	MwAniScript *s;
	int t = get_c_long(tick);
	if (!w_list->object) {
		llpr("No objects!");
		return NIL;
	}
	s = w_list->object->script;
	while (s) {
		if (s->time == t) break;
		s = s->next;
	}
	if (!s) {
		llpr("No such tick");
	} else {
		w_list->script = s;
	}
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP lani_object(LISP obj)
{
	buffer *b = w_list->buf;
	MwAniObject *lasto = w_list->object, *newo;
	MwAniScript *lasts;
	int t = get_c_long(obj);
	int sht = w_list->sht;

	newo = (MwAniObject *)MwMalloc(sizeof(MwAniObject));
	if (lasto) {	/* put newo after lasto */
		newo->next = lasto->next;
		lasto->next = newo;
	} else {	/* put newo first in object list */
		b->sht[sht].cast = newo;
		newo->next = NULL;
	}
	w_list->object = newo;
	newo->type = t;
	newo->name = type2name(t);
	lasts = newo->script = (MwAniScript *)MwMalloc(sizeof(MwAniScript));
	lasts->x = 0;
	lasts->y = 0;
	lasts->width = 0;
	lasts->height = 0;
	lasts->visible = 1;
	lasts->time = 0;
	newo->fmt = 0;
	newo->string = NULL;
	lasts->next = NULL;
	w_list->script = lasts;
	b->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP set_type(LISP type)
{
	int t = get_c_long(type);

	if (!w_list->object) {
		llpr("No object selected");
	} else {
		w_list->object->type = t;
		MwFree(w_list->object->name);
		w_list->object->name = type2name(t);
	}
	w_list->buf->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
FIXME: these two should free the memory used
*/

static LISP del_object(void)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	MwAniObject *o = w_list->object;
	MwAniObject *o1 = w_list->buf->sht[sht].cast;

	if (!o) {
		llpr("None selected");
		return NIL;
	}
	if (o == o1) {	/* first object */
		w_list->buf->sht[sht].cast = o->next;
		w_list->object = o->next;
	} else {	/* some other object; find the one before */
		while (o1->next != o) o1 = o1->next;
		o1->next = o->next;
		w_list->object = o1;	/* select previous object */
	}
	if (w_list->object)
		w_list->script = w_list->object->script;
	else
		w_list->script = NULL;
	b->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP del_time(void)
{
	buffer *b = w_list->buf;
	MwAniObject *o = w_list->object;
	MwAniScript *s, *s1;

	if (!o) {
		llpr("None selected");
		return NIL;
	}
	s = w_list->script;
	s1 = o->script;
	if (s->time == 0) {
		llpr("Can't delete tick 0");
		return NIL;
	}
	while (s1->next != s) s1 = s1->next;
	s1->next = s->next;
	w_list->script = s1;
	b->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP ani_background(LISP bgp)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	if (b->sht[sht].bg) MwFree(b->sht[sht].bg);
	if (NULLP(bgp)) {
		b->sht[sht].bg = NULL;
	} else {
		b->sht[sht].bg = MwStrdup(get_c_string(bgp));
	}
	b->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP ani_bgrad(LISP bgp)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	if (b->sht[sht].bgrad) MwFree(b->sht[sht].bgrad);
	if (NULLP(bgp)) {
		b->sht[sht].bgrad = NULL;
	} else {
		b->sht[sht].bgrad = MwStrdup(get_c_string(bgp));
	}
	b->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

static LISP get_bgrad(void)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	if (b->sht[sht].bgrad) return strcons(-1, b->sht[sht].bgrad);
	return strcons(-1, "");
}

/* ---
*/
static LISP ani_time(LISP tim)
{
	MwAniObject *o = w_list->object;
	MwAniScript *s, *news;
	unsigned int t = get_c_long(tim);
	
	if (!o) return NIL;

	/* find the place to put new tick after */
	s = o->script;
	while (s->next && s->next->time <= t)
		s = s->next;

	if (s->time == t) return NIL;	/* dup tick */

	/* simply copy the last tick, but with a new time */
	news = (MwAniScript *)MwMalloc(sizeof(MwAniScript));
	*news = *s;
	news->next = s->next;
	news->time = t;
	s->next = news;
	w_list->script = news;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP lani_duration(LISP tim)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	b->sht[sht].duration = get_c_long(tim);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP lani_delta(LISP tim)
{
	buffer *b = w_list->buf;
	int sht = w_list->sht;
	b->sht[sht].delta = get_c_long(tim);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP lani_geometry(LISP w, LISP h)
{
	buffer *b = w_list->buf;
	b->width = get_c_long(w);
	b->height = get_c_long(h);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP ani_end(void)
{
/*
	buffer *b = w_list->buf;
	ani_show(b->cast, 0L, b->delta, b->duration, b->width, b->height,
		b->bg, b->path);
*/
	return NIL;
}

static LISP ladd_sheet(void)
{
	buffer *b = w_list->buf;
	int n = b->nsht;
	int m = buffer_add_sheet(b, n);
	pr_scr_flag = 1;
	b->change = 1;
	return flocons(m);
}

static LISP lremove_sheet(void)
{
	buffer *b = w_list->buf;
	int m;
	pr_scr_flag = 1;
	b->change = 1;
	m = buffer_remove_sheet(b, w_list->sht);
	if (w_list->sht >= m) {
		w_list->sht = m-1;
	}
	return flocons(m);
}

static LISP lrename_sheet(void)
{
	char name[1000];
	buffer *b = w_list->buf;
	strcpy(name, b->sht[w_list->sht].name);
	if (ask_for_str("Name: ", name)) {
		buffer_rename_sheet(b, w_list->sht, name);
		pr_scr_flag = 1;
		b->change = 1;
	}
	return NIL;
}

static LISP lmove_sheet(LISP f, LISP t)
{
	int from = get_c_long(f);
	int to = get_c_long(t);
	buffer *b = w_list->buf;
	int nsht = b->nsht;
	sheet sht;
	int i;

	if (from < 0 || from >= nsht || to < -1 || to >= nsht)
		return NIL;

	if (to == -1) to = nsht-1;

	sht = b->sht[from];

	for (i = from; i < nsht-1; i++)
		b->sht[i] = b->sht[i+1];

	for (i = nsht-1; i > to; i--)
		b->sht[i] = b->sht[i-1];

	b->sht[to] = sht;

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
static LISP labort(void)
{
	abort();
	return NIL;
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
			fmt.style = name2type(get_c_string(value));
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
				 cons(SS("style", type2name(fmt.style)),
				      NIL))))))))));
}

/* ---
*/
int egon_plugin_save(int ph, char *fn)
{
	return plugin_save(ph, fn);
}

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
        char p[1024], pn[1024];
        char cmd[1024];
        char fmt[80];
        buffer *buf = w_list->buf;
        int n;
        plugin_t plugin;
	int sht = w_list->sht;

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

        plugin.row = 1;
        plugin.col = 1;
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
        n = buf->sht[sht].nplugin++;
        buf->sht[sht].plugin = (plugin_t *)MwRealloc(buf->sht[sht].plugin,
                buf->sht[sht].nplugin*sizeof(plugin_t));
        buf->sht[sht].plugin[n] = plugin;
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

static int select_plugin(buffer *b)
{
        char *plugin[20];
        int i;
	int sht = w_list->sht;

        if (b->sht[sht].nplugin == 0) return -1;
        if (b->sht[sht].nplugin == 1) return 0;
        for (i = 0; i < 20 && i < b->sht[sht].nplugin; i++)
                plugin[i] = b->sht[sht].plugin[i].name;
        i = select_from_list("Select Plugin:", plugin, i);
        return i;
}

static LISP lplugin_select(void)
{
	return flocons(select_plugin(w_list->buf));
}

static LISP lplugin_export(void)
{
        int n;
        char p[1024], fn[1024], pn[1024], cmd[1024], path[1024], fmt[80];
        buffer *buf = w_list->buf;
	int sht = w_list->sht;

        /* pick plugin from list */
        n = select_plugin(buf);
        if (n == -1) return NIL;

        /* get description of the handler */
        /* ask for file name */
        getcwd(path, 1024);
        strcpy(fn, buf->sht[sht].plugin[n].name);
        if (!select_file(path, fn, NULL, fmt, 1)) return NIL;

        /* save the file */
        sprintf(pn, "%s/%s", plugin_basedir(p, buf->name),
			buf->sht[sht].plugin[n].name);
        sprintf(cmd, "cp %s %s/%s", pn, path, fn);
        system(cmd);
        return NIL;
}

static LISP lplugin_delete(void)
{
        int n;
        buffer *buf = w_list->buf;
	int sht = w_list->sht;

        /* pick plugin from list */
        n = select_plugin(buf);
        if (n == -1) return NIL;

        /* remove plugin */
        plugin_stop(buf->sht[sht].plugin[n].ph);
        MwFree(buf->sht[sht].plugin[n].name);
        buf->sht[sht].nplugin--;
        for (; n < buf->sht[sht].nplugin; n++)
                buf->sht[sht].plugin[n] = buf->sht[sht].plugin[n+1];
        buf->change = pr_scr_flag = TRUE;
        return NIL;
}

static LISP lplugin_move(void)
{
        int n;
        buffer *buf = w_list->buf;
	int sht = w_list->sht;

        /* pick plugin from list */
        n = select_plugin(buf);
        if (n == -1) return NIL;

        buf->sht[sht].plugin[n].row = 1;
        buf->sht[sht].plugin[n].col = 1;
        buf->change = pr_scr_flag = TRUE;
        return NIL;
}

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
	}
	return NIL;
}

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
	}
	return NIL;
}

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


static LISP lmake_backups(LISP p)
{
	make_backups = get_c_long(p);
	return NIL;
}

/* Commands that take no arguments */
s_fn_table0 fn_table[] =
{
	{"listsel-test", listsel_test},

	/* moving around */
	{"what-cursor-position", what_cursor_position},

	/* editing */

	{"psformat", lpsformat},

	/* new window */
	{"delete-window", delete_window},
	{"delete-other-windows", delete_other_windows},
	{"split-window-vertically", split_window_vertically},
	{"other-window", other_window},

	/* buffers and windows */
	{"switch-to-buffer", switch_to_buffer},
	{"kill-buffer", kill_buffer},
	{"ask-for-file", ask_for_file},
	{"load-buffer", load_buffer},
	{"save-buffer", save_buffer},
	{"save-buffer-as", save_buffer_as},
	{"load-external", load_external},
	{"save-external", save_external},

	/* help commands */
	{"print-version", print_version},

	{"quit-program", quit_egon},
	/* low level functions */

	{"add-sheet", ladd_sheet},
	{"remove-sheet", lremove_sheet},
	{"rename-sheet", lrename_sheet},
	{"get-sheet", lget_sheet},
	{"get-bgrad", get_bgrad},

	{"ani-begin", ani_begin},
	{"ani-end", ani_end},
	{"del-object", del_object},
	{"del-time", del_time},
	{"ani-c-test", ani_c_test},
	{"get-format", get_format},
	{"save-preferences", save_preferences},
	{"abort", labort},
	{"aboutbox", laboutbox},
	{"aboutsiag", laboutsiag},
	{(char *) 0, (LISP(*)(void))0}
};

/* Commands that take 1 argument */

s_fn_table1 fn_table1[] = {
	/* low level functions */
	{"spawn", lspawn},
	{"execute-interpreter-command", execute_interpreter_command},
	{"buffer-changed", buffer_changed},
	{"execute", lexecute},
	{"input-warp-pointer", linput_warp_pointer},

	{"ani-object", lani_object},
	{"ani-background", ani_background},
	{"ani-bgrad", ani_bgrad},
	{"ani-time", ani_time},
	{"ani-duration", lani_duration},
	{"ani-delta", lani_delta},
	{"select-object", select_object},
	{"select-tick", select_tick},
	{"set-type", set_type},
	{"set-format", set_format},
	{"decode-format", ldecode_format},
	{"encode-format", lencode_format},
	{"paper-get", lpaper_get},
	{"margin-get", lmargin_get},
	{"headfoot-get", lheadfoot_get},
	{"first-page-get", lfirst_page_get},
	{"make-backups", lmake_backups},
	{NULL, NULL}
};

/* Commands that take 2 arguments */
s_fn_table2 fn_table2[] = {
	{"ask-for-str", lask_for_str},
	{"alertbox", lalertbox},

	{"ani-geometry", lani_geometry},
	{"move-sheet", lmove_sheet},
	{"paper-set", lpaper_set},
	{"margin-set", lmargin_set},
	{"headfoot-set", lheadfoot_set},
	{"first-page-set", lfirst_page_set},
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
	for (i = 0; fn_table2[i].name; i++)
		init_subr_2(fn_table2[i].name, fn_table2[i].function);
	init_subr_3("savematrix", lsavematrix);
	init_subr_3("plugin-register", lplugin_register);
	init_subr_0("plugin-select", lplugin_select);
        init_subr_1("plugin-import", lplugin_import);
        init_subr_0("plugin-export", lplugin_export);
        init_subr_1("plugin-link", lplugin_link);
        init_subr_0("plugin-delete", lplugin_delete);
        init_subr_0("plugin-move", lplugin_move);
}

