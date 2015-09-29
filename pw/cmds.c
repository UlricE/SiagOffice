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
#include <sys/time.h>
#include <sys/types.h>

#include <Mowitz/MwUtils.h>
#include "../common/common.h"
#include "../common/plugin.h"
#include "pw.h"
#include "../siod/siod.h"

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
	char b[256];
	position p = w_list->buf->sht[w_list->sht].point_pos;

	sprintf(b, "[%d,%d]", p.row, p.col);
	llpr(b);
	return NIL;
}

/* ---
*/
static LISP insert_line(void)
{
	downshift_text(w_list->buf, w_list->sht, get_point(w_list).row);
	w_list->buf->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP remove_line(void)
{
	upshift_text(w_list->buf, w_list->sht, get_point(w_list).row);
	w_list->buf->change = TRUE;
	pr_scr_flag = TRUE;
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
   static void load_buffer(void)

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
		name[0] = '\0';
		need_init = 0;
	}
	strcpy(name, "");
	fn[0] = '\0';
	if (select_file(path, name, loader_patterns, fmt, 0)) {
		sprintf(fn, "%s/%s", path, name);
		b = new_buffer(buffer_name(fn), fn);
		llpr("Loading");

		if (loadmatrix(fn, b, fmt))
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

		sprintf(fnl, "%s/pw/external.load", datadir);
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
	i = select_from_list("External Program:",
			loadname, loaders);

	if (i >= 0 && ask_for_str("Parameters:", param)) {
		sprintf(fn, "%s/pw%ld", siag_tmpdir, (long)getpid());
		sprintf(cmd, "%s %s > %s", loadprog[i], param, fn);
		if (system(cmd)) {
			llpr("External program failed");
			return NIL;
		}

		b = new_buffer(buffer_name(fn), fn);
		llpr("Loading");

		if (loadmatrix(fn, b, guess_file_format(fn)))
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
   static void save_buffer()

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

/* ---
*/
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

	if (need_init) {
		getcwd(path, 1024);
		name[0] = '\0';
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

		sprintf(fnl, "%s/pw/external.save", datadir);
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

		sprintf(fn, "%s/pw%ld", siag_tmpdir, (long)getpid());
		if (savematrix(fn, w_list->buf, NULL)) {
			error_box("Couldn't save");
			return NIL;
		}
		sprintf(cmd, "%s %s < %s", program, param, fn);
		if (system(cmd)) {
			error_box("External program failed");
			return NIL;
		}
	}
	return NIL;
}

#if USE_COMPLETION
/* ---
   static void complete_name(char *name)
   This function takes a partially completed buffer name
   and returns the first buffer name that matches it.
*/

static int complete_name(name)
char *name;
{
	buffer *b;
	int len;

	b = w_list->buf;	/* start with the next buffer */
	do {
		b = b->next;
		if ((len = strlen(name)) == 0 
				|| !strncmp(b->name, name, len)) {
			strcpy(name, b->name);
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

extern char *psformat; /* in fileio_ps.c */

/* ---
*/
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
		_("A word processor for X"),
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
static LISP quit_pw(void)
{
	char prompt[256], cmd[1024];
	buffer *b = b_list;
	do {
		if (b->change /*&& !plugin*/) {
			sprintf(prompt, _("Save %s?"), b->name);
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
static LISP go_to(void)
{
	char b[256];
	int tr = 0, tc = 0;
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;

	b[0] = '\0';
	if (ask_for_str("Go to: ", b))
		sscanf(b, "%d %d", &tr, &tc);

	if ((tr > 0) && (tr <= max_lines)) {
		st[s].point_pos.row = tr;
		st[s].point_pos.col = col_last_used(w_list->buf, s, tr);
	}
	if ((tc > 0) && (tc <= st[s].point_pos.col))
		st[s].point_pos.col = tc;
	return NIL;
}

/* ---
*/
static LISP set_line_height(void)
{
	char b[256];
	int height = 0;
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;

	sprintf(b, "%d", line_height(w_list->buf, s, st[s].point_pos.row));
	if (ask_for_str("Height:", b))
		height = atoi(b);

	if (height > 5 && height < 200) {
		alloc_line(w_list->buf, s, st[s].point_pos.row);
		w_list->buf->sht[s].text[st[s].point_pos.row].height = height;
		pr_scr_flag = 1;
	}
	return NIL;
}

static long format_copy = 0;
static int height_copy = 20;

/* ---
*/
static LISP copy_current_format(void)
{
	sheet *st = w_list->buf->sht;
	int s = w_list->sht;
	format_copy = w_list->current_fmt;
	height_copy = line_height(w_list->buf, w_list->sht,
				st[s].point_pos.row);
	return NIL;
}

/* ---
*/
static LISP use_copied_format(void)
{
	w_list->current_fmt = format_copy;
	return NIL;
}

/* ---
*/
static LISP define_style(void)
{
	char b[256];
	sheet *st = w_list->buf->sht;
	int r = st[w_list->sht].point_pos.row;
	int sty = ret_style(w_list->buf, w_list->sht, r);
	MwFmt fmt;
	style s;

	MwDecodeFormat(w_list->current_fmt, ~0, &fmt);
	s = style_table[fmt.style];
	lookup_style(s.name, style2name(s.follower),
		fmt.family, fmt.size, fmt.bold, fmt.italic,
		fmt.uline, fmt.strike, fmt.fg);
	sprintf(b, MwTranslate("Style %s changed"), style_table[sty].name);
	llpr(b);
	return NIL;
}

/* ---
*/
static LISP cleanup_style(void)
{
	buffer *b = w_list->buf;
	int s = w_list->sht;
	sheet *st = b->sht;
	int r = st[s].point_pos.row;
	int sty = ret_style(b, s, r);
	long fmt = style_table[sty].format;

	for (r = 1; r <= line_last_used(b, s); r++) {
		if (ret_style(b, s, r) == sty) {
			ins_format(b, s, r, 0, MwRcStrlen(b->sht[s].text[r].p), fmt);
			b->sht[s].text[r].height = check_line_height(b, s, r);
			b->change = TRUE;
			pr_scr_flag = TRUE;
		}
	}
	llpr("set every char of the current style to the default format");;
	return NIL;
}

/* ---
Spelling checker

This checker does the following for each line in the buffer:

For each word in the line, feed it to ispell like this:
^word

The '^' tells ispell to spell check it and nothing else.

Crucial to the proper operation of this interface is
that PW and ispell agree on what a word is. For example,
"shouldn't" shouldn't be an error, although it contains a
non-alphabetic character. Also, the Swedish word "räksmörgås"
(shrimp sandwich) should be treated as a single word.

So here is my attempt to feed ispell words it will recognize as words.
Note that the list of characters that ispell should understand is
incomplete.

0. Start ispell in a subprocess with the -a -wåäöÅÄÖ_-' options.

1. Find the longest stretch of characters consisting of characters
   entirely in the alphabetic set, or one of the characters in
   this string:
   åäöÅÄÖ_-´

2. Trim away leading and trailing characters in this string: _-'

3. Feed the word as "^word\n" to ispell and read the response.

4. If ispell prints a line beginning with '*' or '+', the word
   is correct, otherwise a dialog with available options is
   displayed.

5. If the user decides to replace the word, this is done by
   deleting the original, inserting the replacement and
   forwarding point to directly after the newly inserted word.
   Otherwise point is advanced to directly after the original
   word.

6. The checking sequence is then continued with step 1.

Ispell doesn't have a mode in which every line read on stdin
is treated as a single word, regardless of character set.
I consider this to be a bug in ispell.

Before running the checker, PW offers to save the buffer.

This is still only a test version. Error checking is mostly
missing.
--- */

int pfd[2], qfd[2];

/* ---
write a line to ispell
*/

void spell_write(char *b)
{
        write(pfd[1], b, strlen(b));
}

/* ---
read a line from ispell
*/

int spell_read(char *b)
{
        fd_set rfds;
        struct timeval tv;
        int retval, n;

        FD_ZERO(&rfds);
        FD_SET(qfd[0], &rfds);
        tv.tv_sec = 5;
        tv.tv_usec = 0;
        retval = select(qfd[0]+1, &rfds, NULL, NULL, &tv);
        if (!retval) return 0;
        n = read(qfd[0], b, 1024);
        b[n] = '\0';
        return n;
}

static char *letters = "åäöÅÄÖ";
static char *others = "_-'";

static int letter(int c)
{
	return isalpha(c) || strchr(letters, c);
}

static int other(int c)
{
	return strchr(others, c) != NULL;
}

static LISP dump_words(void)
{
	char *p, b[1024];
	int i, start = -1, end = -1;
	buffer *buf = w_list->buf;
	sheet *st = buf->sht;
	int s = w_list->sht;
	int row = st[s].point_pos.row;

	if (row > line_last_used(buf, s)) return NIL;

	p = (char *)MwRcMakeplain(buf->sht[s].text[row].p);
	llpr(p);

	for (i = 0; p[i]; i++) {
		if (start == -1) {
			if (letter(p[i])) start = end = i;
		} else {
			if (letter(p[i])) end = i;
			else if (!other(p[i])) {
				strncpy(b, p+start, end-start+1);
				b[end-start+1] = '\0';
				fprintf(stderr, "%s\n", b);
				start = -1;
			}
		}
	}
	/* pick up slack */
	if (start != -1) {
		strncpy(b, p+start, end-start+1);
		b[end-start+1] = '\0';
		fprintf(stderr, "%s\n", b);
	}
	MwFree(p);
	return NIL;
}

/* ---
asks ispell about the word, replacing it in the buffer if needed
returns index of first char after checked/replaced word
*/

static int spell_word(buffer *buf, char *p, int s, unsigned int r,
		int start, int end)
{
	char b[1024], d[1024], *q;
	int n, newend;
	unsigned long fmt;

	strncpy(b, p+start, end-start+1);
	b[end-start+1] = '\0';
	strcat(b, "\n");
	spell_write(b);
	if (!spell_read(d)) {
		fprintf(stderr, "ispell process probably hosed\n");
		return -1;
	}
	if (d[0] == '*' || d[0] == '+' || d[0] == '-')
		return end;	/* all is well */
	if (d[0] == '&') {	/* use first suggestion */
		q = strchr(d, ':');
		if (q == NULL) return -1;	/* bogus string */
		/* can't strcpy because the strings overlap */
		memmove(d, q+2, strlen(q));	/* skip one space */
		q = strchr(d, ',');
		if (q) *q = '\0';
	} else {		/* use incorrect word */
		strcpy(d, b);
	}
	n = spell_select(b, d);
	MwChomp(b);
	MwChomp(d);
	switch (n) {
	case SPELL_REPLACE:
		/* replace in the buffer */
		fmt = ret_format(buf, s, r, start);
		del_text(buf, s, make_position(r, start), end-start+1);
		ins_text(buf, s, make_position(r, start),
			(unsigned char *)d, fmt);
		/* replace in the plaintext string */
		newend = start+strlen(d)-1;
		memmove(p+newend+1, p+end+1, strlen(p+end+1)+1);
		memmove(p+start, d, strlen(d));
		return newend+1;
		break;
	case SPELL_ACCEPT:
		sprintf(d, "@%s\n", b);
		spell_write(d);
		break;
	case SPELL_INSERT:
		sprintf(d, "*%s\n", b);
		spell_write(d);
		break;
	case SPELL_SKIP:
		break;
	case SPELL_CANCEL:
		return -1;
		break;
	default:
		fprintf(stderr, "Spelling checker is broken\n");
		return -1;
	}
	return end+1;
}

/* ---
returns 0 if we want to continue, otherwise 1
*/

static int spell_line(buffer *buf, int s, unsigned int r)
{
	char *p;
	char b[1024];
	int i, start = -1, end = -1;

	if (ret_style(buf, s, r) == MW_STY_EMBED) return 0;
	p = (char *)peek_line(buf, s, r);
	if (!p) return 1;
	strncpy(b, p, 1020);
	b[1020] = '\0';
	MwFree(p);
	llpr(b);	/* display it so we know where we are */

	for (i = 0; b[i]; i++) {
		if (start == -1) {
			if (letter(b[i])) start = end = i;
		} else {
			if (letter(b[i])) end = i;
			else if (!other(b[i])) {
				i = spell_word(buf, b, s, r, start, end);
				if (i == -1) {	/* abort */
					return 1;
				}
				start = end = -1;
			}
		}
	}
	/* pick up slack */
	if (start != -1) {
		i = spell_word(buf, b, s, r, start, end);
		if (i == -1) {	/* abort */
			return 1;
		}
	}
	return 0;
}

/* ---
*/
static LISP spell_buffer(void)
{
	buffer *b = w_list->buf;
	unsigned int row;
	int pid;
	int s = w_list->sht;

	if (b->change) {
		char prompt[1024];
		sprintf(prompt, _("Save %s before checking spelling?"),
			b->name);
		switch(alert_box(prompt, quit_buttons, 3)) {
		case 0:
			savematrix(b->path, b, NULL);
			break;
		case 1:
			break;
		default:
			return NIL;
		}
	}

        pipe(pfd);
        pipe(qfd);
        pid = fork();
        if (pid == -1) {        /* error */
                exit(EXIT_FAILURE);
        } else if (pid == 0) {  /* child */
                /* redir stdin */
                close(pfd[1]);
                dup2(pfd[0], 0);
                close(pfd[0]);
                /* redir stdout */
                close(qfd[0]);
                dup2(qfd[1], 1);
                close(qfd[1]);
                execlp("ispell", "ispell", "-a", "-wåäöÅÄÖ_-'", (char *)0);
                fprintf(stderr, "We don't want to be here\n");
                exit(EXIT_FAILURE);
        } else {                /* parent */
                char q[1024];
		char r[1024];
		close(pfd[0]);
		close(qfd[1]);
                if (!spell_read(r)) {
                        llpr("No response\n");
                        return NIL;
                }
                llpr(r);
                q[0] = '\0';
		for (row = 1; row <= line_last_used(b, s); row++)
			if (spell_line(b, s, row)) break;
		position_kludge2();
		spell_write("#\n");	/* save personal dictionary */
		close(pfd[1]);
		close(qfd[0]);

        }
        return NIL;
}

/* ---
*/
static LISP spell_test(void)
{
        int pid;

        pipe(pfd);
        pipe(qfd);
        pid = fork();
        if (pid == -1) {        /* error */
                exit(EXIT_FAILURE);
        } else if (pid == 0) {  /* child */
                /* redir stdin */
                close(pfd[1]);
                dup2(pfd[0], 0);
                close(pfd[0]);
                /* redir stdout */
                close(qfd[0]);
                dup2(qfd[1], 1);
                close(qfd[1]);
                execlp("ispell", "ispell", "-a", "-wåäöÅÄÖ_-'", (char *)0);
                fprintf(stderr, "We don't want to be here\n");
                exit(EXIT_FAILURE);
        } else {                /* parent */
                char b[1024];
		close(pfd[0]);
		close(qfd[1]);
                if (!spell_read(b)) {
                        llpr("No response\n");
                        return NIL;
                }
		llpr(b);
		b[0] = '\0';
                while (ask_for_str("Word:", b)) {
			strcat(b, "\n");
                        spell_write(b);
                        if (spell_read(b)) {
                                llpr(b);
                        }
                }
		close(pfd[1]);
		close(qfd[0]);
        }
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
static LISP set_current_fmt(LISP fmt)
{
	w_list->current_fmt = get_c_long(fmt);
	return NIL;
}

/* ---
*/
static LISP get_current_fmt(void)
{
	return flocons(w_list->current_fmt);
}

/* ---
*/
static LISP set_format(LISP bname, LISP row, LISP col, LISP format)
{
	buffer *buf;
	long fmt;
	int r, c;
	int s;

	if (NULLP(bname)) {
		buf = w_list->buf;
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}

	if (!buf) return NIL;

	r = get_c_long(row);
	c = get_c_long(col);
	fmt = get_c_long(format);

	alloc_line(buf, s, r);	/* make sure we have all the lines */

	pr_scr_flag = TRUE;
	buf->change = TRUE;

	ins_format(buf, s, r, c, c+1, fmt);
	return NIL;
}

/* ---
*/
static LISP get_format(LISP bname, LISP row, LISP col)
{
	buffer *buf;
	int r, c;
	long fmt;
	int s;
	sheet *st;

	if (NULLP(bname)) {
		buf = w_list->buf;
		s = w_list->sht;
	} else {
		buf = find_sheet_by_name(get_c_string(bname), w_list->buf, &s);
	}
	if (!buf) return NIL;

	st = buf->sht;
	r = get_c_long(row);
	c = get_c_long(col);
	if (r == st[s].point_pos.row && c == st[s].point_pos.col)
		fmt = w_list->current_fmt;
	else
		fmt = ret_format(buf, s, r, c);
	return flocons(fmt);
}

/* ---
*/
static LISP lset_style(LISP row, LISP sty)
{
	int r = get_c_long(row);
	int s;
	int sht = w_list->sht;
	buffer *buf = w_list->buf;
	int fmt;

	if (FLONUMP(sty)) {
		s = get_c_long(sty);
	} else {
		s = name2style(get_c_string(sty));
	}
	fmt = style_table[s].format;
	w_list->current_fmt = fmt;
	set_style(buf, sht, r, s);
	buf->change = TRUE;
	ins_format(buf, sht, r, 0, MwRcStrlen(buf->sht[sht].text[r].p), fmt);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP get_style(LISP row)
{
	int r = get_c_long(row);
	buffer *buf = w_list->buf;
	int s = w_list->sht;
	return flocons(ret_style(buf, s, r));
}

static LISP lset_bop(LISP row, LISP b)
{
	int r = get_c_long(row);
	int sht = w_list->sht;
	buffer *buf = w_list->buf;
	int bop = get_c_long(b);

	set_bop(buf, sht, r, bop);
	buf->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

static LISP lget_bop(LISP row)
{
	int r = get_c_long(row);
	buffer *buf = w_list->buf;
	int s = w_list->sht;
	return flocons(ret_bop(buf, s, r));
}

/* ---
*/
static LISP set_hadjust(LISP row, LISP hadjust)
{
	int r = get_c_long(row);
	int hadj = get_c_long(hadjust);
	buffer *buf = w_list->buf;
	int s = w_list->sht;

	alloc_line(buf, s, r);
	buf->sht[s].text[r].adj = hadj;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP get_hadjust(LISP row)
{
	return flocons(ret_hadj(w_list->buf, w_list->sht, get_c_long(row)));
}

/* ---
*/
static LISP style_follower(LISP sty)
{
	return flocons(style_table[get_c_long(sty)].follower);
}

/* ---
These functions allow implementation of commands in Scheme
rather than in C with Scheme wrappers
--- */

/* ---
*/
static LISP insert_text(LISP text)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	char *p = get_c_string(text);

	ins_text(w_list->buf, s, st[s].point_pos,
		p, w_list->current_fmt);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP remove_text(LISP len)
{
	int s = w_list->sht;
	sheet *st = w_list->buf->sht;
	del_text(w_list->buf, s, st[s].point_pos, get_c_long(len));
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP join_lines_at(LISP row)
{
	int r = get_c_long(row);
	join_lines(w_list->buf, w_list->sht, r);
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP split_line_at(LISP row, LISP col)
{
	int s = w_list->sht;
	int r = get_c_long(row), c = get_c_long(col);
	split_line(w_list->buf, s, r, c);
	w_list->buf->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

/* ---
*/
static LISP lrebreak_line(LISP row)
{
	int s = w_list->sht;
	int r = get_c_long(row);
	w_list->buf->change = TRUE;
	return flocons(rebreak_line(w_list->buf, s, r));
}

/* ---
*/
static LISP delete_lines(LISP row, LISP count)
{
	int s = w_list->sht;
	int r = get_c_long(row);
	int c = get_c_long(count);
	del_lines(w_list->buf, s, r, c);
	w_list->buf->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

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

        sprintf(fn1, "%s/.siag/pw.scm", getenv("HOME"));
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
	if (ask_for_str("Name: ", name)) {
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
ap is a list of 9 arguments
*/

static LISP lregister_style_wrapper(LISP ap)
{
	char *name, *next, *family, *fg;
	int size, b, i, u, s;

	name = get_c_string(CAR(ap));
	ap = CDR(ap);
	next = get_c_string(CAR(ap));
	ap = CDR(ap);
	family = get_c_string(CAR(ap));
	ap = CDR(ap);
	size = get_c_long(CAR(ap));
	ap = CDR(ap);
	b = get_c_long(CAR(ap));
	ap = CDR(ap);
	i = get_c_long(CAR(ap));
	ap = CDR(ap);
	u = get_c_long(CAR(ap));
	ap = CDR(ap);
	s = get_c_long(CAR(ap));
	ap = CDR(ap);
	fg = get_c_string(CAR(ap));

	return flocons(lookup_style(name, next, family, size, b, i, u, s, fg));
}

static LISP ldebug_format(void)
{
	buffer *b = w_list->buf;
	int s = w_list->sht;
	sheet *st = b->sht;
	int r = st[s].point_pos.row;
	int c = st[s].point_pos.col;
	MwFmt fmt;
	int f = ret_format(b, s, r, c);
	char *hadj;
	MwDecodeFormat(f, ~0, &fmt);
	switch (fmt.hadj) {
	case MW_HADJ_LEFT:		hadj = "left "; break;
	case MW_HADJ_CENTER:	hadj = "center "; break;
	case MW_HADJ_RIGHT:	hadj = "right "; break;
	case MW_HADJ_FULL:		hadj = "full "; break;
	default:		hadj = "ERROR "; break;
	}
	fprintf(stderr, "%s %d %s%s%s%s%s%s %s/%s\n",
			fmt.family,
			fmt.size,
			fmt.bold?"bold ":"",
			fmt.italic?"italic ":"",
			fmt.uline?"uline ":"",
			fmt.strike?"strike ":"",
			hadj,
			style2name(fmt.style),
			fmt.fg,
			fmt.bg);
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
		case MW_FMT_STRIKE:
			fmt.strike = get_c_long(value);
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
			     cons(SL("strike", fmt.strike),
			       cons(SL("hadj", fmt.hadj),
			         cons(SL("vadj", fmt.vadj),
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
cool, all this was copied from Siag with cut and paste
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
	int s = w_list->sht;
	sheet *st = buf->sht;

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

        plugin.row = st[s].point_pos.row;
        plugin.col = st[s].point_pos.col;
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

static LISP lplugin_create(void)
{
        /* get list of registered handlers */
        /* ask for file name */
        /* start the plugin */
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
	sheet *st = buf->sht;

        /* pick plugin from list */
        n = select_plugin(buf, s);
        if (n == -1) return NIL;

	alloc_line(buf, s, buf->sht[s].plugin[n].row);
	buf->sht[s].text[buf->sht[s].plugin[n].row].height = style_height(0);
        buf->sht[s].plugin[n].row = st[s].point_pos.row;
        buf->sht[s].plugin[n].col = st[s].point_pos.col;
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

	int n = plugin_write(get_c_long(ph), get_c_string(text));
	if (n) return flocons(n);
	else return NIL;
}

/* ---
Returns string read if succesful, otherwise NIL.
*/

static LISP lplugin_read(LISP ph)
{
	char b[1024];
	int n = plugin_read(get_c_long(ph), b);
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


static LISP lmake_backups(LISP p)
{
	LISP result = flocons(make_backups);
        make_backups = get_c_long(p);
        return result;
}

extern int display_bops;

static LISP ldisplay_bops(LISP n)
{
	LISP result = flocons(display_bops);
	display_bops = get_c_long(n);
	return result;
}

static LISP lset_zoom(LISP n)
{
	zoom = get_c_double(n);
	if (zoom < .1) zoom = .1;
	if (zoom > 10) zoom = 10;
	pr_scr_flag = 1;
	return NIL;
}

/* Set up the table of functions and names */

/* Commands that take no arguments */
s_fn_table0 fn_table[] =
{
	{"listsel-test", listsel_test},

	/* moving around */
	{"what-cursor-position", what_cursor_position},
	{"go-to", go_to},
	{"set-line-height", set_line_height},
/*	{"set-segment-format", set_segment_format},
*/
	{"copy-current-format", copy_current_format},
	{"use-copied-format", use_copied_format},
	{"define-style", define_style},
	{"cleanup-style", cleanup_style},

	/* editing */
	{"insert-line", insert_line},
	{"remove-line", remove_line},
	/* block commands */
	{"psformat", lpsformat},

	/* new window */
	{"delete-window", delete_window},
	{"delete-other-windows", delete_other_windows},
	{"split-window-vertically", split_window_vertically},
	{"other-window", other_window},

	/* buffers and windows */
	{"switch-to-buffer", switch_to_buffer},
	{"kill-buffer", kill_buffer},
	{"load-buffer", load_buffer},
	{"save-buffer", save_buffer},
	{"save-buffer-as", save_buffer_as},
	{"load-external", load_external},
	{"save-external", save_external},

	/* help commands */
	{"print-version", print_version},

	/* screen layout */
	{"quit-program", quit_pw},
	/* low level functions */

	/* misc */
	{"dump-words", dump_words},
	{"spell-buffer", spell_buffer},
	{"spell-test", spell_test},
	{"save-preferences", save_preferences},
	{"add-sheet", ladd_sheet},
	{"remove-sheet", lremove_sheet},
	{"rename-sheet", lrename_sheet},
	{"get-sheet", lget_sheet},
	{"debug-format", ldebug_format},
	{"get-current-fmt", get_current_fmt},
	{"aboutbox", laboutbox},
	{"aboutsiag", laboutsiag},
	{NULL, NULL}
};

/* Commands that take 1 argument */

s_fn_table1 fn_table1[] = {
/*	{"auto-recalc", auto_recalc},*/
	/* low level functions */
	{"spawn", lspawn},
	{"execute-interpreter-command", execute_interpreter_command},
	{"buffer-changed", buffer_changed},
	{"join-lines", join_lines_at},
	{"rebreak-line", lrebreak_line},
	{"remove-text", remove_text},
	{"insert-text", insert_text},
	{"execute", lexecute},
	{"input-warp-pointer", linput_warp_pointer},
	{"get-style", get_style},
	{"get-bop", lget_bop},
	{"get-hadjust", get_hadjust},
	{"style-follower", style_follower},
	{"register-style-wrapper", lregister_style_wrapper},
	{"decode-format", ldecode_format},
	{"encode-format", lencode_format},
	{"set-current-fmt", set_current_fmt},
	{"select-attribute", lselect_attribute},
	{"paper-get", lpaper_get},
	{"margin-get", lmargin_get},
	{"headfoot-get", lheadfoot_get},
	{"first-page-get", lfirst_page_get},
	{"make-backups", lmake_backups},
	{"display-bops", ldisplay_bops},
	{"set-zoom", lset_zoom},
	{NULL, NULL}
};

/* Commands that take 2 arguments */
s_fn_table2 fn_table2[] = {
	{"split-line", split_line_at},
	{"delete-lines", delete_lines},
	{"ask-for-str", lask_for_str},
	{"alertbox", lalertbox},
	{"set-style", lset_style},
	{"set-bop", lset_bop},
	{"set-hadjust", set_hadjust},
	{"move-sheet", lmove_sheet},
	{"paper-set", lpaper_set},
	{"margin-set", lmargin_set},
	{"headfoot-set", lheadfoot_set},
	{"first-page-set", lfirst_page_set},
	{NULL, NULL}
};

/* ---
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
	init_subr_4("set-format", set_format);
	init_subr_3("get-format", get_format);
	init_subr_3("savematrix", lsavematrix);
	init_subr_4("pack-area", lpack_area);
	init_subr_3("unpack-area", lunpack_area);
        init_subr_3("plugin-register", lplugin_register);
        init_subr_1("plugin-import", lplugin_import);
	init_subr_1("plugin-link", lplugin_link);
	init_subr_0("plugin-select", lplugin_select);
        init_subr_0("plugin-export", lplugin_export);
        init_subr_0("plugin-create", lplugin_create);
        init_subr_0("plugin-delete", lplugin_delete);
        init_subr_0("plugin-move", lplugin_move);
	init_subr_0("plugin-resize", lplugin_resize);
	init_subr_2("plugin-write", lplugin_write);
	init_subr_1("plugin-read", lplugin_read);
}

