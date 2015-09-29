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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/xpm.h>
#include <Mowitz/Mowitz.h>
#include "../siod/siod.h"
#include "../common/common.h"
#include "../xcommon/plugin.h"
#include "../egon/egon.h"
#include "xegon.h"

/* ---
*/
int select_file(char *path, char *name, char *patterns[], char *fmt, int e)
{
        char extra[1024];
        sprintf(extra, "Home=%s:Examples=%s/egon/examples",
                getenv("HOME"), docdir);
        return MwFileselInput(topLevel, path, name, patterns, fmt, extra, e);
}

/* ---
*/
int alert_box(char *text, char *buttons[], int nbuttons)
{
        return MwAlertBox(topLevel, text, buttons, nbuttons);
}

void about_box(char *text)
{
        MwAboutBox(topLevel, "egon.xpm", text);
}
 
void about_siag(void)
{
        MwAboutSiag(topLevel);
}

/* ---
*/
int select_from_list(char *text, char *choices[], int nchoices)
{
        return MwListBox(topLevel, text, choices, nchoices);
}

/* ---
*/
void error_box(char *message)
{
        MwErrorBox(topLevel, message);
}

/* ---
this was copied from insert_plugin and botched to load an image file into
   the structured file system. It is not robust, can only be used in the
   way it is in egon.scm and will be replaced with something less crappy.
   I'm doing this to make Egon releaseable. Have mercy.
*/

static LISP limage_filename(void)
{
        static char path[1024], name[1024];
        char fn[1024];
        char pn[1024];
        char cmd[1024];
        char fmt[80];
        char p[1024];
        buffer *buf = w_list->buf;

        /* ask for file name */
        if (path[0] == '\0') getcwd(path, 1024);
        name[0] = fn[0] = '\0';

        if (!select_file(path, name, NULL, fmt, 0)) return NIL;
        sprintf(fn, "%s/%s", path, name);
        plugin_unique_name(name, pn);
	strcpy(name, pn);

        /* copy the file */
        plugin_basedir(p, buf->name);
        sprintf(pn, "%s/%s", p, name);
        sprintf(cmd, "(mkdir %s;cp %s %s)2>/dev/null", p, fn, pn);
        system(cmd);

	/* and now we don't start a plugin, but return the filename */
	return strcons(strlen(pn), pn);
}

static LISP ani_property(LISP name, LISP value)
{
	buffer *b = w_list->buf;
	MwAniScript *lasts = w_list->script;
	MwAniObject *lasto = w_list->object;
	int n = get_c_long(name);

	if (!lasts) err("Last script is NULL", NIL);

	if (FLONUMP(value)) {
		int lv = get_c_long(value);
		switch (n) {
		case MW_ANI_X:
			lasts->x = lv;
			break;
		case MW_ANI_Y:
			lasts->y = lv;
			break;
		case MW_ANI_WIDTH:
			lasts->width = lv;
			break;
		case MW_ANI_HEIGHT:
			lasts->height = lv;
			break;
		case MW_ANI_VISIBLE:
			lasts->visible = lv;
			break;
		case MW_ANI_FORMAT:
			lasto->fmt = lv;
			break;
		default:
			err("No such property", name);
		}
	} else {
		char *tv = get_c_string(value);
		switch (n) {
		case MW_ANI_TEXT:
			lasto->string = MwStrdup(tv);
			break;
		default:
			err("No such property", name);
		}
	}
	b->change = TRUE;
	pr_scr_flag = TRUE;
	return NIL;
}

static LISP lani_ctl(LISP mode, LISP now)
{
	int m = get_c_long(mode);
	stage_init(w_list);
	XtVaSetValues(w_list->ui->stage,
		XtNanimatorMode, m,
		(char *)0);
	return NIL;
}

static LISP lnext_page(void)
{
	int sht = w_list->sht+1;
	buffer *b = w_list->buf;

	if (sht >= b->nsht) return NIL;

	b->state = MW_ANI_STOP;
	b->sht[sht].now = 0;

	/* stop the animation */
	XtVaSetValues(w_list->ui->stage,
		XtNanimatorMode, b->state,
		(char *)0);
	/* replace the cast */
	XtVaSetValues(w_list->ui->stage,
		XtNanimatorCast, b->sht[sht].cast,
		XtNanimatorDelta, b->sht[sht].delta,
		XtNanimatorDuration, b->sht[sht].duration,
		XtNanimatorBgPixmap, b->sht[sht].bg,
		XtNanimatorNow, b->sht[sht].now,
		(char *)0);
	w_list->sht = sht;
	return NIL;
}

static LISP lprevious_page(void)
{
	int sht = w_list->sht-1;
	buffer *b = w_list->buf;

	if (sht < 0) return NIL;

	b->state = MW_ANI_STOP;
	b->sht[sht].now = 0;

	/* stop the animation */
	XtVaSetValues(w_list->ui->stage,
		XtNanimatorMode, b->state,
		(char *)0);
	/* replace the cast */
	XtVaSetValues(w_list->ui->stage,
		XtNanimatorCast, b->sht[sht].cast,
		XtNanimatorDelta, b->sht[sht].delta,
		XtNanimatorDuration, b->sht[sht].duration,
		XtNanimatorBgPixmap, b->sht[sht].bg,
		XtNanimatorNow, b->sht[sht].now,
		(char *)0);
	w_list->sht = sht;
	return NIL;
}

static void execute_siod_action(Widget w, XEvent *event,
			String *params, Cardinal *num_params)
{
	char b[256];
	int i;

	strcpy(b, "(");
	strcat(b, params[0]);
	for (i = 1; i < *num_params; i++) {
		strcat(b, " ");
		strcat(b, params[i]);
	}
	strcat(b, ")");
	exec_expr(siod_interpreter, b);
}

#ifdef HAVE_LIBGUILE
static void execute_guile_action(Widget w, XEvent * event,
			   String * params, Cardinal * num_params)
{
	char b[256];
	int i;

	strcpy(b, "(");
	strcat(b, params[0]);
	for (i = 1; i < *num_params; i++) {
		strcat(b, " ");
		strcat(b, params[i]);
	}
	strcat(b, ")");
#ifdef DEBUG
	printf("guile(%s)\n", b);
#endif
	exec_expr(name2interpreter("Guile"), b);
}
#endif

#ifdef HAVE_LIBTCL
static void execute_tcl_action(Widget w, XEvent *event,
			   String *params, Cardinal *num_params)
{
	char b[256];
	int i;

	strcpy(b, params[0]);
	for (i = 1; i < *num_params; i++) {
		strcat(b, " ");
		strcat(b, params[i]);
	}
	exec_expr(name2interpreter("Tcl"), b);
}
#endif

static XtActionsRec actions[] = {
#ifdef HAVE_LIBTCL
     {"tcl", execute_tcl_action},
#endif
#ifdef HAVE_LIBGUILE
     {"guile", execute_guile_action},
#endif
     {"execute", execute_siod_action}
};

/* ---
*/
void interp_startup(void)
{
	XtAppContext app_context = XtWidgetToApplicationContext(topLevel);
	init_subr_0("image-filename", limage_filename);
	init_subr_2("ani-property", ani_property);
	init_subr_2("ani-ctl", lani_ctl);
	init_subr_0("next-page", lnext_page);
	init_subr_0("previous-page", lprevious_page);
	XtAppAddActions(app_context, actions, XtNumber(actions));
}

