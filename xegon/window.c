/*
   Egon Animator
   Copyright (C) 1997-2006  Ulric Eriksson <ulric@siag.nu>

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
   Module name:    window.c

   This module handles windows: creating, destroying, printing on windows.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Repeater.h>
#include <X11/Xaw/List.h>
#include <X11/xpm.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>

#include <X11/Xaw/Paned.h>
#include <X11/xpm.h>

#include <Mowitz/Mowitz.h>

#include "../siod/siod.h"
#include "../egon/egon.h"
#include "xegon.h"
/*#include "drop.h"*/

#include "../common/common.h"
#include "../xcommon/embed.h"
#include <Mowitz/Mowitz.h>
#include "../xcommon/xcommon.h"
#include "../xcommon/plugin.h"
#include "../common/bitmaps/egon.xpm"

#define MENUBAR (1)
#define TOOLBAR (2)
#define FORMATBAR (4)

#define APPNAME "Egon"

String fallback_resources[] = {
#include "../xcommon/xcommon-ad.h"
#include "../xcommon/dialogs-ad.h"
#include "../xcommon/filesel-ad.h"
#include "../xcommon/nws-ad.h"
#include "app-defaults.h"
	NULL
};

static XtAppContext app_context;

Widget topLevel;
static Widget topbox, frame1, frame2, frame3, menubox, toolbox, formatbox;
static Widget textbox, statusbox, gridpane;
static Widget label1, label2, label3;

static Widget btnFont, btnSize, btnStyle, btnColor;
static Widget cmdBold, cmdItalic, cmdHLeft, cmdHCenter, cmdHRight;
static Widget shortcuts, tooltip;

static int bars = 0;

static AppData app_data;

#define XtNplugin "plugin"
#define XtCPlugin "Plugin"
#define XtNplay "play"
#define XtCPlay "Play"

static XtResource resources[] = {
	{
		XtNplugin,
		XtCPlugin,
		XtRBoolean,
		sizeof(Boolean),
		XtOffsetOf(AppData, plugin),
		XtRImmediate,
		(XtPointer)False,
	}, {
		XtNplay,
		XtCPlay,
		XtRBoolean,
		sizeof(Boolean),
		XtOffsetOf(AppData, play),
		XtRImmediate,
		(XtPointer)False,
	}
};

static MwFmt fmt0 = {"Helvetica", 120, 0, 0, 0, 0, "black", "white", 0, 0, 0, 0};

static XrmOptionDescRec options[] = {
	{"-plugin", "*plugin", XrmoptionNoArg, "True"},
	{"-play", "*play", XrmoptionNoArg, "True"},
};

window *w_list;
static int stage_popped = 0;

static Atom wm_delete_window;	/* Atom sent to destroy a window */
extern Atom target_atom;	/* used for selection */
static Atom *drop_types;

static void draw_buffer(window *);
static window *find_window_by_widget(Widget);

/* ---
*/
int ask_for_str_comp(char *prompt, char *buffr, int (*comp) (char *))
{
	return MwDialogInput(topLevel, prompt, buffr);
}

/* ---
   static int nocomp(char *b)
   This particular completion function doesn't complete at all, it just
   returns TRUE, making TAB equivalent to RET and LFD.
*/

static int nocomp(char *b)
{
	return TRUE;
}

/* ---
   int ask_for_str(char *prompt, char *buffr)
   Calls ask_for_str_comp with nocomp as completion function.
   X
95-06-29: changed "buffer" to "buffr" to please gcc
*/

int ask_for_str(char *prompt, char *buffr)
{
	return ask_for_str_comp(prompt, buffr, nocomp);
}

static void pr_grid(Widget w, XEvent * event,
		    String * params, Cardinal * num_params)
{
	if (ok2print) {
		draw_buffer(find_window_by_widget(w));
		show_cur(w_list);
	}
}

static LISP ltooltip_mode(LISP newmode)
{
	int mode = get_c_long(newmode);
	XtVaSetValues(tooltip,
		XtNtooltipMode, mode,
		XtNtooltipLabel, label2,
		(char *)0);
	return NIL;
}

static void siaghelp_action(Widget w, XEvent *event,
	String *params, Cardinal *num_params)
{
	char b[1256];

	sprintf(b, "siagrun help file://localhost%s/egon/%s",
		docdir, params[0]);
	MwSpawn(b);
}

static void place_shortcuts(Widget w, XEvent *event,
                String *p, Cardinal *n)
{
        XButtonEvent *bev = (XButtonEvent *)event;
        int x, y;

        x = event->xbutton.x;
        y = event->xbutton.y;
        XtVaSetValues(shortcuts,
                XtNx, bev->x_root,
                XtNy, bev->y_root,
                (char *)0);
}

static void
execute_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *b = (char *) client_data;

	execute(b);
}

static struct {
	char *label;
	Widget button, menu;
} *menubar;

static int menucount = 0;

static LISP add_menu(LISP lisp_label)
{
	char *label;
	char button_name[80];

	if (topbox == None) return NIL;

	label = MwStrdup(get_c_string(lisp_label));

	sprintf(button_name, "btn%s", label);
	menubar = MwRealloc(menubar, (menucount+1)*(sizeof *menubar));
	menubar[menucount].label = MwStrdup(label);
	menubar[menucount].button = XtVaCreateManagedWidget(button_name,
		mwMBButtonObjectClass, menubox,
		XtNmenu_name, label,
		XtNlabel, _(label),
		XtNgravitation, (strcmp(label, "Help")?XtCleft:XtCright),
		(char *)0);
	menubar[menucount].menu = XtVaCreatePopupShell(label,
		mwMenuWidgetClass, menubox,
		(char *)NULL);

	menucount++;

	return NIL;
}

static void remake_ylayout(void)
{
	char b[100];
	sprintf(b, "%s %s %s 30 100%% 30",
		(bars&MENUBAR) ? "30" : "0",
		(bars&TOOLBAR) ? "30" : "0",
		(bars&FORMATBAR) ? "30" : "0");
	XtVaSetValues(topbox,
		XtNyLayout, b,
		(char *)0);
}

static void attach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget vw = (Widget)call_data;
	if (vw == frame1) bars |= MENUBAR;
	if (vw == frame2) bars |= TOOLBAR;
	if (vw == frame3) bars |= FORMATBAR;
	remake_ylayout();
}

static void detach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget vw = (Widget)call_data;
	if (vw == frame1) bars &= ~MENUBAR;
	if (vw == frame2) bars &= ~TOOLBAR;
	if (vw == frame3) bars &= ~FORMATBAR;
	remake_ylayout();
}


static void init_menu(void)
{
	bars |= MENUBAR;
	frame1 = XtVaCreateManagedWidget("frame1",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(frame1, frame1, detach_bar, attach_bar);
	menubox = XtVaCreateManagedWidget("menubox",
		mwMenuBarWidgetClass, frame1,
		XtNgridx, 1,
		(char *)0);
}

static Widget find_menu_by_name(char *label)
{
	int i;

	if (!MwStrcasecmp("Shortcuts", label))
		return shortcuts;

	for (i = 0; i < menucount; i++) {
		if (!MwStrcasecmp(menubar[i].label, label))
			return menubar[i].menu;
	}
	return NULL;
}

static LISP add_menu_entry(LISP menu, LISP label, LISP function)
{
	Widget entry, menuw;
	char *lbl = MwStrdup(get_c_string(label));

	menuw = find_menu_by_name(get_c_string(menu));
	if (!menuw) {
		return NIL;
	}

	if (!strcmp(lbl, "-"))	/* line pane */
		entry = XtVaCreateManagedWidget("-",
				mwLineMEObjectClass, menuw,
				(char *)0);
	else {
		entry = XtVaCreateManagedWidget(get_c_string(function),
				mwLabelMEObjectClass, menuw,
				XtNlabel, _(lbl),
			        (char *)0);
		XtAddCallback(entry,
			XtNcallback, execute_callback,	
			MwStrdup(get_c_string(function)));
	}
	return NIL;
}

static struct {
        char *label, *sublabel;
        Widget entry, menu;
} *submenus;

static int submenucount = 0;

static Widget find_submenu_by_name(char *label, char *sublabel)
{
        int i;

        for (i = 0; i < submenucount; i++) {
                if (!MwStrcasecmp(submenus[i].label, label) &&
                                !MwStrcasecmp(submenus[i].sublabel, sublabel))
                        return submenus[i].menu;
        }
        return None;
}

static LISP add_submenu(LISP label, LISP sublabel)
{
        Widget menuw;
	char button_name[80];
	char *sublbl = MwStrdup(get_c_string(sublabel));

        menuw = find_menu_by_name(get_c_string(label));

        if (!menuw) {
                return NIL;
        }
	sprintf(button_name, "btn%s", sublbl);
        submenus = MwRealloc(submenus, (submenucount+1)*(sizeof *submenus));
        submenus[submenucount].label = MwStrdup(get_c_string(label));
        submenus[submenucount].sublabel = sublbl;
        submenus[submenucount].entry =
                XtVaCreateManagedWidget(button_name,
                        mwSubMEObjectClass, menuw,
			XtNmenu_name, sublbl,
                        XtNlabel, _(sublbl),
                        (char *)0);
        submenus[submenucount].menu = XtVaCreatePopupShell(sublbl,
                mwMenuWidgetClass, menuw,
		(char *)0);
        submenucount++;
        return NIL;
}

static LISP add_submenu_entry(LISP menu, LISP submenu,
                                LISP label, LISP function)
{
        Widget entry, menuw;
	char *lbl = MwStrdup(get_c_string(label));

        menuw = find_submenu_by_name(get_c_string(menu),
                                get_c_string(submenu));
        if (!menuw) {
                return NIL;
        }

        if (!strcmp(get_c_string(label), "-")) {        /* line pane */
                entry = XtVaCreateManagedWidget("-",
                        mwLineMEObjectClass, menuw,
                        (char *)0);
        } else {
                entry = XtVaCreateManagedWidget(get_c_string(function),
                        mwLabelMEObjectClass, menuw,
                        XtNlabel, _(lbl),
                        (char *)NULL);
                XtAddCallback(entry,
                        XtNcallback, execute_callback,
                        MwStrdup(get_c_string(function)));
        }
        return NIL;
}

static XtActionsRec actions[] =
{
	{"pr-grid", pr_grid},
	{"siaghelp", siaghelp_action},
	{"place-shortcuts", place_shortcuts},
};


static Widget make_toggle(char *cmd, Widget pw, char *pm, char *tip)
{
        Widget w;
        Pixmap pm_return;
        Pixel color;

        XtVaGetValues(pw, XtNbackground, &color, (char *)0);

        w = XtVaCreateManagedWidget("toolbar_toggle",
                toggleWidgetClass, pw, (char *)NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);

        XtVaSetValues(w,
                XtNbitmap, pm_return,
		XtNforeground, color,
                (char *)0);
        XtAddCallback(w,
                XtNcallback, execute_callback, (XtPointer)cmd);
	MwTooltipAdd(tooltip, w, _(tip));
        return w;
}

static Widget make_command(char *cmd, Widget pw, char *pm, char *tip)
{
        Widget w;
        Pixmap pm_return;
        Pixel color;

        XtVaGetValues(pw, XtNbackground, &color, (char *)0);

        w = XtVaCreateManagedWidget("toolbar_command",
                commandWidgetClass, pw, (char *)NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);

        XtVaSetValues(w,
                XtNbitmap, pm_return,
		XtNforeground, color,
                (char *)0);

        XtAddCallback(w,
                XtNcallback, execute_callback, (XtPointer)cmd);
	MwTooltipAdd(tooltip, w, _(tip));
        return w;
}

static void make_vsep(Widget pw)
{
	unsigned long bg;

	XtVaGetValues(topbox,
		XtNbackground, &bg,
		(char *)0);
	XtVaCreateManagedWidget("vsep",
		labelWidgetClass, pw,
		XtNborderColor, bg,
		(char *)0);
}

/* ---
The toolbar
*/

static void init_toolbar(void)
{
	frame2 = XtVaCreateManagedWidget("toolbar",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(frame2, frame2, detach_bar, attach_bar);
	bars |= TOOLBAR;
	toolbox = XtVaCreateManagedWidget("frame2",
		mwFrameWidgetClass, frame2,
		XtNgridx, 1,
		XtNshadowWidth, 1,
		(char *)0);
	toolbox = XtVaCreateManagedWidget("toolbox",
		boxWidgetClass, toolbox,
		(char *)0);
	frame3 = XtVaCreateManagedWidget("formatbar",
		mwRudegridWidgetClass, topbox,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
	MwMakeHandle(frame3, frame3, detach_bar, attach_bar);
	bars |= FORMATBAR;
	formatbox = XtVaCreateManagedWidget("frame3",
		mwFrameWidgetClass, frame3,
		XtNgridx, 1,
		XtNshadowWidth, 1,
		(char *)0);
	formatbox = XtVaCreateManagedWidget("formatbox",
		boxWidgetClass, formatbox,
		(char *)0);
	make_command("(new-buffer)", toolbox, "new.xpm",
		     "Start another instance of Egon Animator");
	make_command("(load-buffer)", toolbox, "fld_open.xpm",
		     "Open an Egon Animator document");
	make_command("(save-buffer-as)", toolbox, "save.xpm",
		     "Save the contents of the current buffer");
	make_command("(preview)", toolbox, "preview.xpm",
		     "Preview the contents of the current buffer");
	make_command("(print)", toolbox, "printer.xpm",
		     "Print the contents of the current buffer");
	make_vsep(toolbox);
	make_command("(ani-ctl ANI_STOP 0)", toolbox, "stop.xpm",
		     "Pause the animation");
	make_command("(ani-ctl ANI_PREVIOUS 0)", toolbox, "previous.xpm",
		     "Previous animation frame");
	make_command("(ani-ctl ANI_CONTINUE 0)", toolbox, "play.xpm",
		     "Play the animation");
	make_command("(ani-ctl ANI_NEXT 0)", toolbox, "next.xpm",
		     "Next animation frame");
	make_vsep(toolbox);
	make_command("(help-contents)", toolbox, "info.xpm",
		     "Display the Egon online documentation");
	make_command("(help-copyright)", toolbox, "copyright.xpm",
		     "Display the Gnu general public license");
}

static void init_toggle(void)
{
	cmdBold = make_toggle("(toggle-format \"bold\")",
		formatbox, "bold.xpm", "Bold text");
	cmdItalic = make_toggle("(toggle-format \"italic\")",
		formatbox, "italic.xpm", "Italic text");
}

static char *combo_sizes[] = {
	"8", "9", "10", "11", "12", "14", "16", "18",
	"20", "22", "24", "26", "28", "36", "48", "72"
};

static char *combo_objects[] = {
	"Line", "Rectangle", "Arc", "Ellipse",
	"Pixmap", "String", "Point", "Filled Rectangle",
	"Filled Arc", "Filled Ellipse"
};

static char **combo_fonts, **combo_colors;
static int ncombo_fonts, ncombo_colors;

static void cb_font(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"family\" \"%s\")", p);
	activate_window(w_list);
}

static void cb_size(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"size\" (* 10 %s))", p);
	activate_window(w_list);
}

static void cb_object(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"style\" \"%s\")", p);
	activate_window(w_list);
}

static void cb_color(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	va_execute("(new-format \"fg\" \"%s\")", p);
	activate_window(w_list);
}

/* ---
*/
static void setup_buttons(void)
{
	unsigned long bg;

	if (topbox == None) return;

	XtVaGetValues(topbox,
		XtNbackground, &bg,
		(char *)0);
	combo_fonts = MwFontList(&ncombo_fonts);
	btnFont = XtVaCreateManagedWidget("format_command",
		mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNcomboTop, topLevel,
		XtNcomboData, combo_fonts,
		XtNcomboNData, ncombo_fonts,
		XtNwidth, 160,
		(char *)0);
	XtAddCallback(btnFont, XtNlistCallback, cb_font, NULL);
	XtAddCallback(btnFont, XtNtextCallback, cb_font, NULL);
	MwTooltipAdd(tooltip, btnFont, _("Change the font family"));

	btnSize = XtVaCreateManagedWidget("format_command",
		mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNcomboTop, topLevel,
		XtNcomboData, combo_sizes,
		XtNcomboNData, XtNumber(combo_sizes),
		XtNwidth, 40,
		(char *)0);
	XtAddCallback(btnSize, XtNlistCallback, cb_size, NULL);
	XtAddCallback(btnSize, XtNtextCallback, cb_size, NULL);
	MwTooltipAdd(tooltip, btnSize, _("Change the font size"));

	btnStyle = XtVaCreateManagedWidget("format_command",
		mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNcomboTop, topLevel,
		XtNcomboData, combo_objects,
		XtNcomboNData, XtNumber(combo_objects),
		XtNwidth, 100,
		(char *)0);
	XtAddCallback(btnStyle, XtNlistCallback, cb_object, NULL);
	XtAddCallback(btnStyle, XtNtextCallback, cb_object, NULL);
	MwTooltipAdd(tooltip, btnStyle, _("Change the display style"));

	combo_colors = MwColorList(&ncombo_colors);
	btnColor = XtVaCreateManagedWidget("format_command",
		mwComboWidgetClass, formatbox,
		XtNborderColor, bg,
		XtNcomboTop, topLevel,
		XtNcomboData, combo_colors,
		XtNcomboNData, ncombo_colors,
		XtNwidth, 80,
		(char *)0);
	XtAddCallback(btnColor, XtNlistCallback, cb_color, NULL);
	XtAddCallback(btnColor, XtNtextCallback, cb_color, NULL);
	MwTooltipAdd(tooltip, btnColor, _("Change the color"));
}


static void reset_lists(window *w)
{
	static String fallback[1] = {"<none>"};
	XawListChange(w->ui->objl, fallback, 1, 0, True);
	XawListChange(w->ui->tickl, fallback, 1, 0, True);
	XawListChange(w->ui->propl, fallback, 1, 0, True);
}

static LISP popdown_stage(void)
{
	stage_popped = 0;
	XtPopdown(w_list->ui->ani_shell);
	XtDestroyWidget(w_list->ui->stage);
	w_list->ui->stage = NULL;
	XtDestroyWidget(w_list->ui->ani_shell);
	w_list->ui->ani_shell = NULL;
	draw_buffer(w_list);
	return NIL;
}


/* ---
*/
void activate_window(window *w)
{
	char b[256];
	int tab, s = w->sht;

	w_list = w;
	if (w) {
		strcpy(b, "Egon Animator: ");
		strncat(b, w->buf->name, 200);
		XtVaSetValues(topLevel, XtNtitle, b, NULL);
		tab = MwTabbingTextToPos(w->ui->tab, w->buf->sht[s].name);
		if (tab < 0) {
			w_list->sht = s = tab = 0;
		}
		XtVaSetValues(w->ui->tab, XtNtabbingSelected, tab, (char *)0);
	}
}

static void object_select(Widget w, XtPointer client_data, XtPointer call_data)
{
	XawListReturnStruct *list_struct = (XawListReturnStruct *)call_data;
	String string = list_struct->string;
	char cmd[256];

	sprintf(cmd, "(select-object \"%s\")", string);
	execute(cmd);
	draw_buffer(w_list);
}

static void tick_select(Widget w, XtPointer client_data, XtPointer call_data)
{
	XawListReturnStruct *list_struct = (XawListReturnStruct *)call_data;
	String string = list_struct->string;
	char cmd[256];

	sprintf(cmd, "(select-tick %s)", string);
	execute(cmd);
	draw_buffer(w_list);
}

/* ---
*/
static void draw_input(char *text)
{
	char b[256];
	buffer *buf = w_list->buf;
	int sht = w_list->sht;

	if (!w_list->object)
		sprintf(b, "No object selected");
	else
		sprintf(b,
			_("[%s at %d] [size = %dx%d] [duration=%d delta=%d]"),
			w_list->object->name, w_list->script->time,
			buf->width, buf->height,
			buf->sht[sht].duration, buf->sht[sht].delta);
	MwLabelSet(label1, b);
	MwLabelSet(label3, "Egon");
}

/* ---
*/
static void draw_status(char *text)
{
	MwLabelSet(label2, text);
	XFlush(XtDisplay(topLevel));
}

/* ---
   void llpr(char *p)
   Prints the string p on the bottom line of the screen.  If p is empty and
   the last string printed was also empty, the string isn't printed.
*/

void llpr(char *p)
{
	static int isclear = FALSE;

	if (isclear && p[0] == '\0')
		return;
	isclear = (p[0] == '\0');

	draw_status(p);
}

#define FREE_LIST(l,n)\
	if (l) {\
		for (i = 0; i < (n); i++) MwFree((l)[i]);\
		MwFree(l);\
	}

#undef FREE_LIST

#define FREE_LIST(l,n)

/* ---
*/
static void draw_buffer(window *w)
{
	buffer *b = w->buf;
	MwAniObject *o;
	MwAniScript *s;
	char p[256];
	static int on = 0, sn = 0, pn = 0;
	static char **ol = NULL, **sl = NULL, **pl = NULL;
	int sht = w->sht;

	FREE_LIST(ol, on)
	FREE_LIST(sl, sn)
	FREE_LIST(pl, pn)

	/* select the first object if none is selected */
	if (!w->object) {
		w->object = b->sht[sht].cast;
		if (w->object) w->script = w->object->script;
	}

	on = 0;
	for (o = b->sht[sht].cast; o; o = o->next) on++;

	if (!on) {
		reset_lists(w);
		sn = pn = 0;
		goto The_end;
	}

	ol = (char **)MwMalloc(on*sizeof(char *));
	on = 0;
	for (o = b->sht[sht].cast; o; o = o->next) {
		ol[on] = MwStrdup(o->name);
		on++;
	}
	XawListChange(w->ui->objl, ol, on, 0, True);

	o = w_list->object;
	sn = 0;
	for (s = o->script; s; s = s->next) sn++;
	sl = (char **)MwMalloc(sn*sizeof(char *));
	sn = 0;
	for (s = o->script; s; s = s->next) {
		sprintf(p, "%d", s->time);
		sl[sn] = MwStrdup(p);
		sn++;
	}
	XawListChange(w->ui->tickl, sl, sn, 0, True);

	s = w_list->script;
	pn = 0;
	pl = (char **)MwMalloc(9*sizeof(char *));
	sprintf(p, "Position = (%d, %d)", s->x, s->y);
	pl[pn++] = MwStrdup(p);
	sprintf(p, "Size = %dx%d", s->width, s->height);
	pl[pn++] = MwStrdup(p);
	sprintf(p, "Visible = %s", s->visible?"True":"False");
	pl[pn++] = MwStrdup(p);
	sprintf(p, "Format = %d", o->fmt);
	pl[pn++] = MwStrdup(p);
	sprintf(p, "String = %s", o->string?o->string:"EMPTY");
	pl[pn++] = MwStrdup(p);
	XawListChange(w->ui->propl,
		pl, pn, 0, True);

The_end:

	if (w->ui->ani_shell) {
		XtVaSetValues(w->ui->stage,
			XtNanimatorCast, b->sht[sht].cast,
			XtNanimatorDelta, b->sht[sht].delta,
			XtNanimatorDuration, b->sht[sht].duration,
			XtNanimatorBgPixmap, b->sht[sht].bg,
			XtNgradient, b->sht[sht].bgrad,
			XtNanimatorNow, b->sht[sht].now,
			XtNanimatorMode, b->state,
			(char *)0);
		XtVaSetValues(w->ui->ani_shell,
			XtNwidth, b->width,
			XtNheight, b->height,
			(char *)0);
	}
	draw_input(NULL);
}

/* ---
*/
static window *find_window_by_widget(Widget wdg)
{
	window *w = w_list;
	egon_ui *u;
	do {
		u = w->ui;
		if (u->viewport == wdg ||
		    u->objv == wdg || u->objl == wdg ||
		    u->objf == wdg || u->objt == wdg ||
		    u->tickv == wdg || u->tickl == wdg ||
		    u->tickf == wdg || u->tickt == wdg ||
		    u->propv == wdg || u->propl == wdg ||
		    u->propf == wdg || u->propt == wdg ||
		    u->tabbox == wdg || u->tab == wdg ||
		    u->tabl == wdg || u->tabr == wdg)
			return w;
		w = w->next;
	} while (w != w_list);
	return NULL;
}

/* ---
*/
void free_window(window *w)
{
	window *pw;

	for (pw = w_list; pw->next != w && pw->next != pw; pw = pw->next);
	pw->next = w->next;

	if (w_list == w) w_list = w_list->next;
	if (w_list == w) w_list = NULL;
	if (w->ui->viewport != None)
		XtDestroyWidget(w->ui->viewport);
	MwFree(w);
}

static void replace_tabs(window *w)
{
	int n, top;
	buffer *b;
	XtVaGetValues(w->ui->tab,
		XtNtabbingCount, &n,
		XtNtabbingTop, &top,
		(char *)0);
	while (n) MwTabbingRemove(w->ui->tab, --n);
	for (n = 0; n < w->buf->nsht; n++) {
		MwTabbingInsert(w->ui->tab, w->buf->sht[n].name, n);
	}
	if (top >= w->buf->nsht)
		top = w->buf->nsht-1;

	b = b_list;
	do {
		if (b != w->buf) {
			char p[1000];
			sprintf(p, "%s:", b->name);
			MwTabbingInsert(w->ui->tab, p, -1);
		}
		b = b->next;
	} while (b != b_list);

	XtVaSetValues(w->ui->tab,
		XtNtabbingSelected, w->sht,
		XtNtabbingTop, top,
		(char *)0);
}

static void select_tab(Widget w, XtPointer client_data, XtPointer call_data)
{
	buffer *b;
	int s = (int)call_data;
	char *name = MwTabbingPosToText(w, s);
	if (name == NULL) return;

	w_list = find_window_by_widget(w);
	if (w_list == NULL) {
		fprintf(stderr, "find_window_by_widget returns NULL\n");
		return;
	}
	b = find_sheet_by_name(name, w_list->buf, &s);
	if (b == NULL) return;

	hide_cur(w_list);

	w_list->buf = b;
	w_list->sht = s;
	pr_scr_flag = TRUE;
	replace_tabs(w_list);
	activate_window(w_list);
	show_cur(w_list);
}

static void rename_tab(Widget w, XtPointer client_data, XtPointer call_data)
{
        char *name = MwTabbingPosToText(w, (int)call_data);
        if (name == NULL) return;
	hide_cur(w_list);
        buffer_rename_sheet(w_list->buf, w_list->sht, name);
        pr_scr_flag = 1;
        w_list->buf->change = 1;
	replace_tabs(w_list);
	activate_window(w_list);
	show_cur(w_list);
}

static void tabs_left(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget tw = (Widget)client_data;
	int n;

	XtVaGetValues(tw, XtNtabbingTop, &n, (char *)0);
	XtVaSetValues(tw, XtNtabbingTop, n-1, (char *)0);
}

static void tabs_right(Widget w, XtPointer client_data, XtPointer call_data)
{
	Widget tw = (Widget)client_data;
	int n;

	XtVaGetValues(tw, XtNtabbingTop, &n, (char *)0);
	XtVaSetValues(tw, XtNtabbingTop, n+1, (char *)0);
}


/* ---
When running as player, return a window structure without any widgets
Slightly modified for Egon: only one window at a time
*/

window *new_window(buffer *b, window *prev)
{
	window *w;

	if (w_list) w = w_list;
	else w = (window *)MwMalloc(sizeof(window));

	if (w == NULL) return NULL;

	w->ui = (egon_ui *)MwMalloc(sizeof(egon_ui));
	w->ui->stage = w->ui->ani_shell = None;
	w->sht = 0;
	if (w->ui == NULL) {
		MwFree(w);
		return NULL;
	}
	w->buf = b;

	if (prev == NULL) prev = w;
	else w->next = prev->next;
	prev->next = w;

	w->object = NULL;	/* none selected yet */
	w->script = NULL;

	w->ui->viewport = XtVaCreateManagedWidget("viewport",
		mwRudegridWidgetClass, gridpane,
		(char *)0);

	w->ui->objf = XtVaCreateManagedWidget("objf",
		mwFrameWidgetClass, w->ui->viewport,
		(char *)0);

	w->ui->objt = XtVaCreateManagedWidget("objt",
		labelWidgetClass, w->ui->objf,
		(char *)0);
	MwLabelSet(w->ui->objt, "Objects");
	XtVaSetValues(w->ui->objf,
		XtNtitle, w->ui->objt,
		(char *)0);
	w->ui->objv = XtVaCreateManagedWidget("objv",
		viewportWidgetClass, w->ui->objf,
		(char *)0);
	w->ui->objl = XtVaCreateManagedWidget("objl",
		listWidgetClass, w->ui->objv, NULL);
	XtAddCallback(w->ui->objl, XtNcallback, object_select, NULL);
	w->ui->tickf = XtVaCreateManagedWidget("tickf",
		mwFrameWidgetClass, w->ui->viewport,
		(char *)0);
	w->ui->tickt = XtVaCreateManagedWidget("tickt",
		labelWidgetClass, w->ui->tickf,
		(char *)0);
	MwLabelSet(w->ui->tickt, "Ticks");
	XtVaSetValues(w->ui->tickf,
		XtNtitle, w->ui->tickt,
		(char *)0);
	w->ui->tickv = XtVaCreateManagedWidget("tickv",
		viewportWidgetClass, w->ui->tickf,
		(char *)0);
	w->ui->tickl = XtVaCreateManagedWidget("tickl",
		listWidgetClass, w->ui->tickv, NULL);
	XtAddCallback(w->ui->tickl, XtNcallback, tick_select, NULL);

	w->ui->propf = XtVaCreateManagedWidget("propf",
		mwFrameWidgetClass, w->ui->viewport,
		(char *)0);
	w->ui->propt = XtVaCreateManagedWidget("propt",
		labelWidgetClass, w->ui->propf,
		(char *)0);
	MwLabelSet(w->ui->propt, "Properties");
	XtVaSetValues(w->ui->propf,
		XtNtitle, w->ui->propt,
		(char *)0);
	w->ui->propv = XtVaCreateManagedWidget("propv",
		viewportWidgetClass, w->ui->propf,
		(char *)0);
	w->ui->propl = XtVaCreateManagedWidget("propl",
		listWidgetClass, w->ui->propv, (char *)0);

	w->ui->tabbox = XtVaCreateManagedWidget("tabbox",
		mwRudegridWidgetClass, w->ui->viewport,
		(char *)0);
	w->ui->tab = XtVaCreateManagedWidget("tab",
		mwTabbingWidgetClass, w->ui->tabbox,
		(char *)0);
	XtAddCallback(w->ui->tab, XtNselectCallback, select_tab, NULL);
        XtAddCallback(w->ui->tab, XtNrenameCallback, rename_tab, NULL);
	w->ui->tabl = XtVaCreateManagedWidget("tabl",
		repeaterWidgetClass, w->ui->tabbox,
		(char *)0);
	XtAddCallback(w->ui->tabl, XtNcallback,
		tabs_left, (XtPointer)w->ui->tab);
	w->ui->tabr = XtVaCreateManagedWidget("tabr",
		repeaterWidgetClass, w->ui->tabbox,
		(char *)0);
	XtAddCallback(w->ui->tabr, XtNcallback,
		tabs_right, (XtPointer)w->ui->tab);


	return w;
}

/* ---
*/
int ani_ctl(int mode, unsigned int now)
{
	XtVaSetValues(w_list->ui->stage,
		XtNanimatorMode, mode,
		(char *)0);
        return MW_ANI_OK;
}

/* ---
*/
int remove_window(window *w)
{
	
	if (w == w->next) return FALSE;
	free_window(w);
	return TRUE;
}

/* ---
*/
int split_window(window *w)
{
	window *w2 = new_window(w->buf, w);

	if (w2 == NULL) return FALSE;
	return TRUE;
}

/* ---
this does probably not belong here in window.c,
   because it doesn't depend on X
*/

static void save_plugin(char *p)
{
        if (*p++ != ' ' || *p == '\0') printf("501 File name missing\n");
        else {
                if (savematrix(p, w_list->buf, NULL)) {
                        printf("501 Can't save %s\n", p);
                } else {
                        printf("250 Saved %s\n", p);
                }
        }
}

static void load_plugin(char *p)
{
        if (*p++ != ' ' || *p == '\0') printf("501 File name missing\n");
        else {
                if (loadmatrix(p, w_list->buf, NULL)) {
                        printf("501 Can't load %s\n", p);
                } else {
                        printf("250 Loaded %s\n", p);
                }
        }
}

static void exec_plugin(char *p)
{
        if (*p++ != ' ' || *p == '\0') printf("501 Command missing\n");
        else {
                execute(p);
                printf("250 OK\n");
        }
}

static void help_plugin(char *p)
{
        printf("214 SAVE LOAD EXEC HELP NOOP QUIT PRNT\n");
}

static void noop_plugin(char *p)
{
        printf("250 OK\n");
}

static void win_plugin(char *p)
{
        printf("250 %lx\n", (unsigned long)XtWindow(topLevel));
}

static void quit_plugin(char *p)
{
        printf("221 Over and out\n");
        execute("(quit-program)");
}

static void prnt_plugin(char *p)
{
        printf("502 Can't print yet\n");
}

static struct {
        char *verb;
        void (*cb)(char *);
} plugin_cmds[] = {
        {"SAVE", save_plugin},
        {"LOAD", load_plugin},
        {"EXEC", exec_plugin},
        {"HELP", help_plugin},
        {"NOOP", noop_plugin},
        {"WIN", win_plugin},
        {"QUIT", quit_plugin},
        {"PRNT", prnt_plugin},
        {NULL, NULL}
};

static void read_plugin_cmd(XtPointer client_data, int *fid, XtInputId *id)
{
        char b[1024], *p;
        int i, n;

        if ((n = read(*fid, b, 1020)) == -1) return;

        b[n] = '\0';
        if ((p = strchr(b, '\n')) == NULL) {
                printf("501 Incomplete command\n");
                fflush(stdout);
                return;
        }

        *p = '\0';
        for (i = 0; plugin_cmds[i].verb; i++) {
                if (!strncmp(b, plugin_cmds[i].verb,
                                strlen(plugin_cmds[i].verb)))
                        break;
        }
        if (plugin_cmds[i].verb)
                (*plugin_cmds[i].cb)(b+strlen(plugin_cmds[i].verb));
        else
                printf("500 What are you talking about\n");
        fflush(stdout);
}

/* ---
handle spontaneous exit of plugins
*/

static void handle_plugin_exit(int ph)
{
        buffer *b = b_list;
	int s = w_list->sht;

        do {
                int n = buffer_plugin2index(b, ph);
                if (n != -1) {
                        MwFree(b->sht[s].plugin[n].name);
                        b->sht[s].nplugin--;
                        for (; n < b->sht[s].nplugin; n++)
                                b->sht[s].plugin[n] = b->sht[s].plugin[n+1];
                        b->change = pr_scr_flag = TRUE;
                }
                b = b->next;
        } while (b != b_list);
}

void stage_init(window *w)
{
	buffer *b = w->buf;
	Display *dpy = XtDisplay(topLevel);
	Window win, dummy, root;
	int x, y;
	unsigned int width, height, border, depth;
	int xem;
	int revert;

	if (w->ui->ani_shell) return;

	if (b->width == 0 || b->height == 0) {	/* full screen */
		XGetGeometry(dpy, XtWindow(topLevel), &root,
			&x, &y, &width, &height, &border, &depth);
		XGetGeometry(dpy, root, &root,
			&x, &y, &width, &height, &border, &depth);
		w->ui->ani_shell = XtVaCreatePopupShell("ani_shell",
			overrideShellWidgetClass, topLevel,
			(char *)0);
	} else {
		width = b->width;
		height = b->height;
		w->ui->ani_shell = XtVaCreatePopupShell("ani_shell",
			topLevelShellWidgetClass, topLevel,
			(char *)0);
	}
	XtRealizeWidget(w->ui->ani_shell);
	XtUnrealizeWidget(w->ui->ani_shell);
	XtVaSetValues(w->ui->ani_shell,
		XtNwidth, width,
		XtNheight, height,
		(char *)0);
	XtPopup(w->ui->ani_shell, XtGrabNone);
	XtVaSetValues(w->ui->ani_shell,
		XtNwidth, width,
		XtNheight, height,
		(char *)0);
	w->ui->stage = XtVaCreateManagedWidget("stage",
		mwAnimatorWidgetClass, w->ui->ani_shell,
		XtNanimatorCast, NULL,
		XtNanimatorMode, MW_ANI_STOP,
		XtNanimatorNow, 0,
		(char *)0);


	win = XtWindow(w->ui->stage);
	xem = KeyPressMask
		| KeyReleaseMask
		| ButtonPressMask
		| StructureNotifyMask
		| ExposureMask
		| ButtonReleaseMask
		| Button1MotionMask;
	XSelectInput(dpy, win, xem);
	XGetInputFocus(dpy, &dummy, &revert);
	XSetInputFocus(dpy, win, RevertToParent, CurrentTime);

	XSetWMProtocols(XtDisplay(w_list->ui->ani_shell),
			XtWindow(w_list->ui->ani_shell),
			&wm_delete_window, 1);
	stage_popped = 1;
	draw_buffer(w);
}

static void cb_drag(Widget w, XtPointer client_data, XtPointer call_data)
{                                                                        
        DropPosition *where = (DropPosition *)call_data;
        printf("cb_drag at (%d,%d)\n", where->x, where->y);
}                                                          
 
static void cb_drop(Widget w, XtPointer client_data, XtPointer call_data)
{                                                                        
        int prot = (int)call_data;
        int dnd_type;             
        Atom xdnd_type;
        char *data;    
                   
        switch (prot) {
        case DndDrop:  
                printf("cb_drop received OffiX drop\n");
                XtVaGetValues(w,                        
                        XtNdndType, &dnd_type,
                        XtNdropData, &data,   
                        (char *)0);
                printf("type = %d, data = '%s'\n", dnd_type, data);
                break;
        case XdndDrop:
                printf("cb_drop received Xdnd drop\n");
                XtVaGetValues(w,
                        XtNxdndType, &xdnd_type,
                        XtNdropData, &data,
                        (char *)0);
                printf("type = '%s', data = '%s'\n",
                        XGetAtomName(XtDisplay(w), xdnd_type), data);
                break;
        case MotifDrop:
                printf("cb_drop received Motif drop\n");
                break;
        default:
                printf("cb_drop received unknown drop\n");
                break;
        }
}

static void init_windows1(int *argc, char **argv)
{
	XtSetLanguageProc(NULL, (XtLanguageProc) NULL, NULL);

	topLevel = XtVaOpenApplication(
		    &app_context,	/* application context */
		    "Egon",		/* application class */
		    options,		/* command line options list */
		    XtNumber(options),
		    argc, argv,	/* command line args */
		    fallback_resources,	/* for missing app-defaults file */
		    mwApplicationShellWidgetClass,
		    NULL);		/* terminate varargs list */

	drop_types[0] = XInternAtom(XtDisplay(topLevel), "text/plain", False);
	drop_types[1] = XInternAtom(XtDisplay(topLevel), "text/uri-list", False);
	drop_types[2] = None;
	XtVaSetValues(topLevel,
		XtNdropTypes, drop_types,
		(char *)0);
	XtAddCallback(topLevel, XtNdragCallback, cb_drag, NULL);
	XtAddCallback(topLevel, XtNdropCallback, cb_drop, NULL);

	theme_init(XtDisplay(topLevel));

	XSetErrorHandler(MwXErrorHandler);

        XtGetApplicationResources(topLevel, &app_data, resources,
                        XtNumber(resources), NULL, 0);

	XSynchronize(XtDisplay(topLevel), True);

	XtAppAddActions(app_context, actions, XtNumber(actions));

	topbox = XtCreateManagedWidget("topbox",
		mwRudegridWidgetClass, topLevel, NULL, 0);
	XtOverrideTranslations(topLevel,
		XtParseTranslationTable(
			"<Message>WM_PROTOCOLS: execute(quit-program)"));

	MwHighlightInit(topLevel);
	tooltip = XtVaCreatePopupShell("tooltip",
		mwTooltipWidgetClass, topLevel,
		(char *)0);
	shortcuts = XtVaCreatePopupShell("shortcuts",
                mwMenuWidgetClass, topLevel, (char *)0);

	init_menu();
	init_toolbar();
	setup_buttons();
	init_toggle();

	textbox = XtVaCreateManagedWidget("textbox",
		mwRudegridWidgetClass, topbox,
		(char *)0);
	label1 = XtVaCreateManagedWidget("label1",
		labelWidgetClass, textbox,
		(char *)0);

	gridpane = XtVaCreateManagedWidget("gridpane",
		panedWidgetClass, topbox,
		XtNallowResize, True,
		(char *)0);

	statusbox = XtVaCreateManagedWidget("statusbox",
		mwRudegridWidgetClass, topbox,
		(char *)0);
	label2 = XtVaCreateManagedWidget("label2",
		labelWidgetClass, statusbox,
		(char *)0);
	label3 = XtVaCreateManagedWidget("label3",
		labelWidgetClass, statusbox,
		(char *)0);
}

static void handle_plugin_cmd(char *p)
{
	execute(p);
}


/* ---
   void init_windows(buffer *b)
   Sets up the whole initial window structure and initializes scrupd.
   The window list w_list is set to a list with a single window with the
   buffer b.
*/

void init_windows(buffer * b, int *argc, char **argv)
{
	Display *dpy;

	start_splash();

	init_windows1(argc, argv);
	interp_startup();

	dpy = XtDisplay(topLevel);

	MwInitFormat(dpy);
	MwEncodeFormat(~0, &fmt0);

	w_list = new_window(b, NULL);
	XtRealizeWidget(topLevel);

        plugin_init(topLevel, handle_plugin_exit, handle_plugin_cmd);

	stage_init(w_list);

	activate_window(w_list);

	wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
	XtOverrideTranslations(topLevel,
		XtParseTranslationTable(
			"<Message>WM_PROTOCOLS: execute(quit-program)"));

	XSetWMProtocols(dpy, XtWindow(topLevel), &wm_delete_window, 1);

	draw_status("");

	/* Set up selection */
	target_atom = XInternAtom(dpy, "EGON_BLOCK", False);

	embed_init(topLevel);

	ok2print = 1;

	init_subr_0("popdown-stage", popdown_stage);
	init_subr_1("add-menu", add_menu);
	init_subr_3("add-menu-entry", add_menu_entry);
	init_subr_2("add-submenu", add_submenu);
        init_subr_4("add-submenu-entry", add_submenu_entry);
	init_subr_1("tooltip-mode", ltooltip_mode);

	init_form(topLevel);

	MwSetIcon(topLevel, egon_xpm);

	stop_splash();
}

/* ---
   void exit_windows()
   Cleans up after Calc before exit.  All buffers and windows are freed.
*/

void exit_windows(void)
{
	/* free all buffers */
	while (b_list != NULL)
		free_buffer(b_list);
	while (w_list != NULL)
		free_window(w_list);
}

/* ---
   static void pr_scr()
   Prints and refreshes all the windows.
   Sets pr_scr_flag to FALSE.
*/

static void pr_scr(void)
{
	window *w;
	int i;
	int s = w_list->sht;

	draw_status("");
	w = w_list;
	do {
		draw_buffer(w);
		for (i = 0; i < w->buf->sht[s].nplugin; i++) {
                        if (!w->buf->sht[s].plugin[i].displayed) {
                                plugin_show(w->buf->sht[s].plugin[i].ph,
                                                w->ui->stage);
                                w->buf->sht[s].plugin[i].displayed = 1;
                        }
                }
		replace_tabs(w);
		w = w->next;
	} while (w != w_list);
	pr_scr_flag = FALSE;
}	/* pr_scr */




/* ---
*/
static void show_format(void)
{
	static int last_fmt = -1;
	int f = ret_format(w_list->buf,
				w_list->object,
				w_list->script);
	MwFmt fmt;
	int hadj = MW_HADJ_LEFT;
	int hadj_left = (hadj == MW_HADJ_LEFT);
	int hadj_center = (hadj == MW_HADJ_CENTER);
	int hadj_right = (hadj == MW_HADJ_RIGHT);
	char b[100];

	MwDecodeFormat(f, ~0, &fmt);
	/* menus */
	if (1) {
	   	int type;
	   	if (!w_list->buf || !w_list->object || !w_list->script)
	     		type = 0;
		else type = w_list->object->type;
		MwComboTextChange(btnFont, fmt.family);
		sprintf(b, "%d", fmt.size/10);
		MwComboTextChange(btnSize, b);
		MwComboTextChange(btnStyle, type2name(type));
		MwComboTextChange(btnColor, fmt.fg);
	}

	/* toggle buttons */
	MwStateSet(cmdBold, (fmt.bold?1:0), 1, 0);
	MwStateSet(cmdItalic, (fmt.italic?1:0), 1, 0);
	MwStateSet(cmdHLeft, (hadj_left?1:0), 1, 0);
	MwStateSet(cmdHCenter, (hadj_center?1:0), 1, 0);
	MwStateSet(cmdHRight, (hadj_right?1:0), 1, 0);
	last_fmt = f;
}

/* ---
   void show_cur(window *w)
   Moves the cursor to reflect the position of point in w.
   If point is not visible, the window is moved so that point is in
   the middle of the screen.
*/

void show_cur(window *w)
{
	if (w) {
		if (pr_scr_flag) pr_scr();
		show_format();
	}
}	/* show_cur */

/* ---
*/
void hide_cur(window *w)
{
}

/* ---
*/
void mainloop(void)
{
        if (app_data.plugin) {
                /* control plugin from stdin */
                XtAppAddInput(XtWidgetToApplicationContext(topLevel),
                        fileno(stdin), (XtPointer)XtInputReadMask,
                        read_plugin_cmd, NULL);
                printf("220 %s\n", version);
                fflush(stdout);
        }

        XtAppMainLoop(XtWidgetToApplicationContext(topLevel));

        exit(0);
}

