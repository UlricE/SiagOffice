#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../siod/siod.h"
#include "../siag/calc.h"
#include "tsiag.h"

static LISP lbogus(void) { return NIL; }
static LISP lbogus1(LISP a) { return NIL; }
static LISP lbogus2(LISP a, LISP b) { return NIL; }
static LISP lbogus3(LISP a, LISP b, LISP c) { return NIL; }
static LISP lbogus4(LISP a, LISP b, LISP c, LISP d) { return NIL; }

int select_file(char *path, char *name, char **patterns, char *fmt, int e)
{
	int nchoices, format;

	MW_TRACE((f, "select_file"));

	for (nchoices = 0; patterns[nchoices]; nchoices++);

	format = select_from_list("File format:", patterns, nchoices);
	if (format < 0) return 0;
	strcpy(fmt, patterns[format]);

	return ask_for_str("Filename:", name);
}

int alert_box(char *text, char **buttons, int nbuttons)
{
	return select_from_list(text, buttons, nbuttons);
}

void about_box(char *text)
{
	llpr(text);
}

void about_siag(void)
{
	llpr("Siag Office");
}

void error_box(char *message)
{
	llpr(message);
}

static LISP set_block(void)
{
	MW_TRACE((f, "set_block is starting"));

        if (get_point(w_list).row < get_mark(w_list).row) {
                set_blku_row(w_list, get_point(w_list).row);
                set_blkl_row(w_list, get_mark(w_list).row);
        } else {
                set_blku_row(w_list, get_mark(w_list).row);
                set_blkl_row(w_list, get_point(w_list).row);
        }

        if (get_point(w_list).col < get_mark(w_list).col) {
                set_blku_col(w_list, get_point(w_list).col);
                set_blkl_col(w_list, get_mark(w_list).col);
        } else {
                set_blku_col(w_list, get_mark(w_list).col);
                set_blkl_col(w_list, get_point(w_list).col);
        }

        pr_scr_flag = TRUE;
        return NIL;
}

static LISP unset_block()
{
	MW_TRACE((f, "unset_block is starting"));

        set_blku(w_list, sub_position(P_MIN, make_position(1, 1)));
        set_blkl(w_list, sub_position(P_MIN, make_position(1, 1)));

        pr_scr_flag = TRUE;
        return NIL;
}

/* this one overrides the one in siag/cmds.c */
static LISP execute_extended_command()
{
	char b[256];

	MW_TRACE((f, "execute_extended_command"));

	b[0] = '\0';

	if (ask_for_str("M-x ", b)) {
		execute(b);
		pr_scr_flag = TRUE;
	}
	return NIL;
}

void embed_load() { ; }

int font_input(int *format, int *mask) { return 0; /* ABORT */ }

/* Dummy Postscript functions */
int ps_text_width(int index, char *s) { return 80; } 
int ps_font_descent(int index) { return 5; } 
int ps_font_height(int font) { return 10; } 
int ps_embed_print(FILE *fp, char *tag, int x_base, int y_base) { return 0; } 
int ps_font_size(int index) { return 10; }

/* plugins always fail :( */
char **plugin_patterns = NULL;
int plugin_write(int ph, char *b)			{ return 0; }
int plugin_read(int ph, char *b)			{ return 0; }
int plugin_register(char *desc, char *ext, char *cmd)	{ return -1; }
int plugin_start(char *fn)				{ return -1; } 
int plugin_stop(int ph)					{ return -1; }
int plugin_save(int ph, char *fn)			{ return -1; }
int plugin_hide(int ph)					{ return -1; }
int plugin_size_get(int ph, int *w, int *h)		{ return -1; }
int plugin_size_set(int ph, int w, int h)		{ return -1; }
int plugin_print(int ph, FILE *fp, int x, int y, int t)	{ return -1; }

void interp_startup()
{
	MW_TRACE((f,"interp_startup"));

	init_subr_0("get-geometry", lbogus);
	init_subr_0("fit-block-width", lbogus);
	init_subr_0("fit-block-height", lbogus);
	init_subr_0("embed-object", lbogus);
	init_subr_0("embed-remove", lbogus);
	init_subr_0("embed-open", lbogus);
	init_subr_0("embed-save", lbogus);
	init_subr_0("copy-block", lbogus);
	init_subr_0("set-block", set_block);
	init_subr_0("unset-block", unset_block);
	init_subr_0("execute-extended-command", execute_extended_command);
	init_subr_1("add-menu", lbogus1);
	init_subr_3("add-menu-entry", lbogus3);
	init_subr_2("add-submenu", lbogus2);
	init_subr_4("add-submenu-entry", lbogus4);
	init_subr_1("remove-menu", lbogus1);
	init_subr_3("remove-menu-entry", lbogus3);
	init_subr_2("remove-submenu", lbogus2);
	init_subr_4("remove-submenu-entry", lbogus4);
	init_subr_1("tooltip-mode", lbogus1);
	init_subr_3("plugin-register", lbogus3);
}

