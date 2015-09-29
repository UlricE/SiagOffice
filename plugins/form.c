/*
   form.c
   Copyright (C) 1997-2003  Ulric Eriksson <ulric@siag.nu>

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
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/List.h>
#include <Mowitz/Mowitz.h>

String fallback_resources[] = {
#include "form-ad.h"
	NULL
};

static Widget topLevel;
static Atom plugin_protocol;
static Window ewin = None;
static int ph = 0;
Widget topbox, notebook, frame, command, check;
Dimension width, height;


/* semi-experimental code to read form definition files */

enum {
	TEXT_CTL,
	COMBO_CTL,
	LIST_CTL,
	SPINNER_CTL,
	RADIO_CTL,
	CHECK_CTL,
	NO_CTL
};

static struct {
	char *name;
	int type;
} typename[] = {
	{"text", TEXT_CTL},
	{"combo", COMBO_CTL},
	{"list", LIST_CTL},
	{"spinner", SPINNER_CTL},
	{"radio", RADIO_CTL},
	{"check", CHECK_CTL}
};

typedef struct {
	Widget lw, cw;		/* label widget, control widget */
	int type;
	int done;		/* used by printout routines */
	char *name;
	char *caption;
	char *value;
	double min, max, snap;	/* for the spinner */
	int nitems;
	char **items;		/* for combo, list */
} control;

typedef struct {
	Widget box;
	char *tab;
	int nctl;
	control *ctl;
} page;

page *pages = NULL;
int cur_page = -1, cur_ctl = -1, npages;

static int add_control(int type, char *name, char *caption, char *value)
{
	if (type < 0 || type > NO_CTL) {
		fprintf(stderr, "Unknown control type %d\n", type);
		return -1;
	}
	if (type == SPINNER_CTL) {
		fprintf(stderr, "Spinner is not yet implemented\n");
		return -1;
	}

	cur_ctl = pages[cur_page].nctl++;

	pages[cur_page].ctl = realloc(pages[cur_page].ctl,
				pages[cur_page].nctl*sizeof *pages[cur_page].ctl);
	if (pages[cur_page].ctl == NULL) {
		fprintf(stderr, "Out of memory\n");
		return -1;
	}
	pages[cur_page].ctl[cur_ctl].type = type;
	pages[cur_page].ctl[cur_ctl].name = malloc(strlen(name)+1);
	pages[cur_page].ctl[cur_ctl].caption = malloc(strlen(caption)+1);
	pages[cur_page].ctl[cur_ctl].value = malloc(strlen(value)+1);
	if (pages[cur_page].ctl[cur_ctl].name == NULL
			|| pages[cur_page].ctl[cur_ctl].caption == NULL
			|| pages[cur_page].ctl[cur_ctl].value == NULL) {
		fprintf(stderr, "Out of memory\n");
		return -1;
	}
	strcpy(pages[cur_page].ctl[cur_ctl].name, name);
	strcpy(pages[cur_page].ctl[cur_ctl].caption, caption);
	strcpy(pages[cur_page].ctl[cur_ctl].value, value);
	pages[cur_page].ctl[cur_ctl].min = 0;
	pages[cur_page].ctl[cur_ctl].max = 0;
	pages[cur_page].ctl[cur_ctl].snap = 0;
	pages[cur_page].ctl[cur_ctl].nitems = 0;
	pages[cur_page].ctl[cur_ctl].items = NULL;
	return 0;
}

static int add_item(char *item)
{
	char **items;
	int n = pages[cur_page].ctl[cur_ctl].nitems++;

	items = realloc(pages[cur_page].ctl[cur_ctl].items,
			(n+1)*sizeof(char *));
	if (items == NULL) {
		fprintf(stderr, "Out of memory\n");
		return -1;
	}
	items[n] = malloc(strlen(item)+1);
	if (items[n] == NULL) {
		fprintf(stderr, "Out of memory\n");
		return -1;
	}
	strcpy(items[n], item);
	pages[cur_page].ctl[cur_ctl].items = items;
	return 0;
}

static int add_tab(char *tab)
{
	cur_page = npages++;

	pages = realloc(pages, npages*sizeof *pages);
	if (!pages) {
		fprintf(stderr, "Out of memory\n");
		return -1;
	}

	pages[cur_page].tab = malloc(strlen(tab)+1);
	if (pages[cur_page].tab == NULL) {
		fprintf(stderr, "Out of memory\n");
		return -1;
	}
	strcpy(pages[cur_page].tab, tab);
	pages[cur_page].nctl = 0;
	pages[cur_page].ctl = NULL;
	cur_ctl = -1;

	return 0;
}

static int name2type(char *name)
{
	int i;

	for (i = 0; i < NO_CTL; i++)
		if (!strcmp(typename[i].name, name)) break;
	return i;
}

static int parse_line(char *p)
{
	int n;
	char *q, type[80], name[80], rest[1000];

	if (p[0] == '#' || p[0] == '\0') return 0;

	if (p[0] == '[') {
		q = strchr(p, ']');
		if (!q) {
			fprintf(stderr, "Bogus tab: '%s'\n", p);
			return -1;
		}
		*q = '\0';
		return add_tab(p+1);
	}
	if (p[0] == '\t') return add_item(p+1);
	n = sscanf(p, "%s %s %[^\n]", type, name, rest);
	if (n != 3) {
		fprintf(stderr, "Bogus control: '%s'\n", p);
		return -1;
	}
	if (rest[0] == '=') {
		return add_control(name2type(type), name, NULL, rest+1);
	} else {
		q = strchr(rest, '=');
		if (q == NULL) {
			fprintf(stderr, "No value: '%s'\n", rest);
			return -1;
		}
		*q = '\0';
		return add_control(name2type(type), name, rest, q+1);
	}
}

static int parse_form(const char *form)
{
	const char *r = form;
	char *q, p[1000];
	int n = 0;

	if (r == NULL) {
		fprintf(stderr, "NULL form data\n");
		return -1;
	}

	r = form;
	q = strchr(r, '\n');
	if (q) n = q-r;
	
	while (q) {
		if (n > (sizeof p)-1) {
			fprintf(stderr, "Overlong line\n");
			return -1;
		}
		strncpy(p, r, n);
		p[n] = '\0';
		n = parse_line(p);
		if (n) return n;
		r = q+1;
		q = strchr(r, '\n');
		if (q) n = q-r;
	}
	return 0;
}

static int read_form(FILE *fp)
{
	int n;
	char *formdata;

	if (fp == NULL) return -1;

	if ((formdata = malloc(32100)) == NULL) return -1;

	n = fread(formdata, 1, 32000, fp);
	if (n == 0) {
		free(formdata);
		 return -1;
	}

	formdata[n] = '\0';

	n = parse_form(formdata);

	free(formdata);
	return n;
}

static void create_items(Widget w, int nitems, char **items)
{
	int i;

	fprintf(stderr, "Item lists are NYI\n");
	printf("\t\t%d items\n", nitems);
	for (i = 0; i < nitems; i++)
		printf("\t\t'%s'\n", items[i]);
}

static void combo_dummy(Widget w, XtPointer client_data, XtPointer call_data)
{
	char *p = (char *)call_data;
	fprintf(stderr, "Combo selected '%s'\n", p);
}

static Widget find_radio_group(char *name)
{
	int i, j;

	for (i = 0; i < npages; i++) {
		for (j = 0; j < pages[i].nctl; j++) {
			if (!strcmp(pages[i].ctl[j].name, name))
				return pages[i].ctl[j].cw;
		}
	}
	return None;
}

static Widget create_text(Widget pw, int i, control *ctl)
{
	Widget fw;

	ctl[i].lw = XtVaCreateManagedWidget("label_ctl",
		labelWidgetClass, pw,
		XtNlabel, ctl[i].caption,
		(char *)0);
	fw = XtVaCreateManagedWidget("frame_ctl",
		mwFrameWidgetClass, pw,
		XtNfromHoriz, ctl[i].lw,
		(char *)0);
	ctl[i].cw = XtVaCreateManagedWidget("text_ctl",
		mwTextfieldWidgetClass, fw,
		XtNstring, ctl[i].value,
		(char *)0);
	if (ctl[i].lw == None || ctl[i].cw == None) {
		fprintf(stderr, "No widget\n");
		return None;
	}
	return fw;
}

static Widget create_combo(Widget pw, int i, control *ctl)
{
	Widget fw;

	ctl[i].lw = XtVaCreateManagedWidget("label_ctl",
		labelWidgetClass, pw,
		XtNlabel, ctl[i].caption,
		(char *)0);
	fw = XtVaCreateManagedWidget("frame_ctl",
		mwFrameWidgetClass, pw,
		XtNfromHoriz, ctl[i].lw,
		(char *)0);
	ctl[i].cw = XtVaCreateManagedWidget("combo_ctl",
		mwComboWidgetClass, fw,
		XtNcomboTop, topLevel,
		XtNcomboData, (char *)0,
		XtNcomboNData, 0,
		(char *)0);
	XtAddCallback(ctl[i].cw, XtNlistCallback, combo_dummy, NULL);
	XtAddCallback(ctl[i].cw, XtNtextCallback, combo_dummy, NULL);
	if (ctl[i].lw == None || ctl[i].cw == None) {
		fprintf(stderr, "No widget\n");
		return None;
	}
	return fw;
}

static Widget create_list(Widget pw, int i, control *ctl)
{
	Widget fw;

	ctl[i].lw = XtVaCreateManagedWidget("label_ctl",
		labelWidgetClass, pw,
		XtNlabel, ctl[i].caption,
		(char *)0);
	fw = ctl[i].cw = XtVaCreateManagedWidget("list_ctl",
		listWidgetClass, pw,
		XtNfromHoriz, ctl[i].lw,
		(char *)0);
	if (ctl[i].lw == None || ctl[i].cw == None) {
		fprintf(stderr, "No list\n");
		return None;
	}
	return fw;
}

static Widget create_spinner(Widget pw, int i, control *ctl)
{
	Widget fw;

	ctl[i].lw = XtVaCreateManagedWidget("label_ctl",
		labelWidgetClass, pw,
		XtNlabel, ctl[i].caption,
		(char *)0);
	fw = ctl[i].cw = XtVaCreateManagedWidget("spinner_ctl",
		labelWidgetClass, pw,
		XtNlabel, ctl[i].value,
		(char *)0);
	if (ctl[i].lw == None || ctl[i].cw == None) {
		fprintf(stderr, "No spinner\n");
		return None;
	}
	return fw;
}

static Widget create_radio(Widget pw, int i, control *ctl)
{
	Widget grp, fw;

	ctl[i].lw = None;
	fw = ctl[i].cw = XtVaCreateManagedWidget("radio_ctl",
		mwCheckWidgetClass, pw,
		XtNlabel, ctl[i].caption,
		XtNstate, (int)strtol(ctl[i].value, NULL, 0),
		XtNcheckStyle, MwRadioWin,
		XtNradioStart, topbox,
		(char *)0);
	grp = find_radio_group(ctl[i].name);
	if (grp == None) grp = ctl[i].cw;
	XtVaSetValues(ctl[i].cw,
		XtNradioGroup, grp,
		(char *)0);
	if (ctl[i].cw == None) {
		fprintf(stderr, "No widget\n");
		return None;
	}
	return fw;
}

static Widget create_check(Widget pw, int i, control *ctl)
{
	Widget fw;

	ctl[i].lw = None;
	fw = ctl[i].cw = XtVaCreateManagedWidget("check_ctl",
		mwCheckWidgetClass, pw,
		XtNlabel, ctl[i].caption,
		XtNstate, (int)strtol(ctl[i].value, NULL, 0),
		XtNcheckStyle, MwCheckWin,
		(char *)0);
	if (ctl[i].cw == None) {
		fprintf(stderr, "No widget\n");
		return None;
	}
	return fw;
}

static void create_controls(Widget pw, int nctl, control *ctl)
{
	int i;
	Widget lastw = None, fw;

	printf("\t%d controls\n", nctl);
	for (i = 0; i < nctl; i++) {
		printf("\tControl type %d name %s caption '%s' value '%s'\n",
			ctl[i].type, ctl[i].name,
			ctl[i].caption, ctl[i].value);
		switch (ctl[i].type) {
		case TEXT_CTL:
			fw = create_text(pw, i, ctl);
			break;
		case COMBO_CTL:
			fw = create_combo(pw, i, ctl);
			break;
		case LIST_CTL:
			fw = create_list(pw, i, ctl);
			break;
		case SPINNER_CTL:
			fw = create_spinner(pw, i, ctl);
			break;
		case RADIO_CTL:
			fw = create_radio(pw, i, ctl);
			break;
		case CHECK_CTL:
			fw = create_check(pw, i, ctl);
			break;
		default:
			fprintf(stderr, "Control type %d is NYI\n",
				ctl[i].type);
			return;
		}
		if (lastw != None) {
			if (ctl[i].lw != None) {
				XtVaSetValues(ctl[i].lw,
					XtNfromVert, lastw,
					(char *)0);
			}
			XtVaSetValues(fw,
				XtNfromVert, lastw,
				(char *)0);
		}
		lastw = fw;
		create_items(ctl[i].cw, ctl[i].nitems, ctl[i].items);
	}
}

static void create_form(void)
{
	int i;

	printf("%d pages\n", npages);
	for (i = 0; i < npages; i++) {
		printf("Tab [%s]\n", pages[i].tab);
		pages[i].box = XtVaCreateManagedWidget("form_box",
			formWidgetClass, frame,
			(char *)0);
		XtSetMappedWhenManaged(pages[i].box, False);
		create_controls(pages[i].box, pages[i].nctl, pages[i].ctl);
	}
}

/* end of form reader */


/* Plugin stuff */
static void ewin_cmd(char *p)
{
	unsigned long w;
	if (sscanf(p, "%lx", &w) != 1) {
		printf("500 Where\n");
		return;
	}
   	ewin = w;
	printf("250 OK\n");
}

static void ph_cmd(char *p)
{
	if (sscanf(p, "%d", &ph) != 1) {
		printf("500 What\n");
		return;
	}
	printf("250 OK\n");
}

static void what_cmd(char *p)
{
	printf("(llpr \"Howdy!\")\n");
}

static void win_cmd(char *p)
{
	printf("250 %lx\n", (unsigned long) XtWindow(topLevel));
}

static void exec_cmd(char *p)
{
	XtVaGetValues(topbox,
		XtNwidth, &width,
		XtNheight, &height,
		(char *)0);
	fprintf(stderr, "layout = %dx%d\n", width, height);

	XtVaGetValues(notebook,
		XtNwidth, &width,
		XtNheight, &height,
		(char *)0);
	fprintf(stderr, "notebook = %dx%d\n", width, height);

	XtVaGetValues(command,
		XtNwidth, &width,
		XtNheight, &height,
		(char *)0);
	fprintf(stderr, "command = %dx%d\n", width, height);

	printf("250 OK\n");
}

static void quit_cmd(char *p)
{
	printf("221 Over and out\n");
	exit(0);
}

static struct {
	char *verb;
	void (*cb) (char *);
} plugin_cmds[] = {
	{"EWIN", ewin_cmd},
	{"PH", ph_cmd},
	{"WHAT", what_cmd},
	{"WIN", win_cmd},
	{"EXEC", exec_cmd},
	{"QUIT", quit_cmd},
	{ NULL, NULL }
};

static void read_plugin_cmd(XtPointer client_data, int *fid, XtInputId * id)
{
	char b[1024], *p;
	int i, n;

	if ((n = read(*fid, b, 1020)) == -1)
		return;

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
	if (plugin_cmds[i].verb) {
		(*plugin_cmds[i].cb) (b+strlen(plugin_cmds[i].verb)+1);
	} else {
		printf("500 What are you talking about\n");
	}
	fflush(stdout);
}

/* ---
*/
void mainloop(void)
{
	XtAppAddInput(XtWidgetToApplicationContext(topLevel),
		fileno(stdin), (XtPointer) XtInputReadMask,
		read_plugin_cmd, NULL);
	printf("220 Form plugin\n");
	fflush(stdout);

	XtAppMainLoop(XtWidgetToApplicationContext(topLevel));
}

static void select_tab(Widget w, int pos)
{
	int i;

	for (i = 0; i < npages; i++) {
		XtUnmapWidget(pages[i].box);
	}
	XtMapWidget(pages[pos].box);
	XtVaSetValues(w,
		XtNnotebookSelected, pos,
		(char *)0);
}

static char *find_radio_value(char *name)
{
	int i, j;
	char *value = "";
	Boolean state;

	for (i = 0; i < npages; i++) {
		for (j = 0; j < pages[i].nctl; j++) {
			if (pages[i].ctl[j].done
				|| pages[i].ctl[j].type != RADIO_CTL
				|| strcmp(pages[i].ctl[j].name, name))
					continue;
			XtVaGetValues(pages[i].ctl[j].cw,
				XtNstate, &state,
				(char *)0);
			if (state) value = pages[i].ctl[j].caption;
			pages[i].ctl[j].done = 1;
		}
	}
	return value;
}

static void print_controls(int nctl, control *ctl)
{
	int i;
	Boolean state;
	char *p;

	for (i = 0; i < nctl; i++) {
		if (ctl[i].done) continue;
		switch (ctl[i].type) {
		case TEXT_CTL:
			p = MwTextFieldGetString(ctl[i].cw);
			printf("%s=%s\n", ctl[i].name, p);
			break;
		case COMBO_CTL:
			printf("%s=%s\n", ctl[i].name, ctl[i].value);
			break;
		case CHECK_CTL:
			XtVaGetValues(ctl[i].cw,
				XtNstate, &state,
				(char *)0);
			if (state) printf("%s=%s\n", ctl[i].name,
					ctl[i].caption);
			else printf("%s=\n", ctl[i].name);
			break;
		case RADIO_CTL:
			printf("%s=%s\n", ctl[i].name,
				find_radio_value(ctl[i].name));
			break;
		case LIST_CTL:
			printf("%s=%s\n", ctl[i].name, ctl[i].value);
		default:
			break;
		}
	}
}

static void print_values(void)
{
	int i, j;

	for (i = 0; i < npages; i++) {
		for (j = 0; j < pages[i].nctl; j++)
			pages[i].ctl[j].done = 0;
	}

	for (i = 0; i < npages; i++)
		print_controls(pages[i].nctl, pages[i].ctl);
}

static void ok_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	print_values();
	exit(EXIT_SUCCESS);
}

static void cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
	exit(EXIT_FAILURE);
}

/* ---
*/
int main(int argc, char **argv)
{
	XtAppContext ac;
	Display *dpy;
	int i, n;
	Widget w;

	n = read_form(fopen("test.form", "r"));

	topLevel = XtVaOpenApplication(&ac, "FormPlugin",
		NULL, 0, &argc, argv,
		fallback_resources,
		mwApplicationShellWidgetClass,
		(char *)0);
	topbox = XtVaCreateManagedWidget("topbox",
		formWidgetClass, topLevel,
		(char *)0);
	notebook = XtVaCreateManagedWidget("notebook",
		mwNotebookWidgetClass, topbox,
		(char *)0);
	/* here we use a Frame widget because it lays out its children
	   in just the way we want: they all occupy the whole parent */
	frame = XtVaCreateManagedWidget("frame",
		mwFrameWidgetClass, topbox,
		(char *)0);
	create_form();
	dpy = XtDisplay(topLevel);
	plugin_protocol = XInternAtom(dpy, "_PLUGIN_PROTOCOL", False);

	w = XtVaCreateManagedWidget("ok",
		commandWidgetClass, topbox,
		(char *)0);
	XtAddCallback(w, XtNcallback, ok_cb, NULL);
	w = XtVaCreateManagedWidget("cancel",
		commandWidgetClass, topbox,
		(char *)0);
	XtAddCallback(w, XtNcallback, cancel_cb, NULL);

	XtRealizeWidget(topLevel);

	for (i = 0; i < npages; i++) {
		MwNotebookInsert(notebook, pages[i].tab, i);
	}
	XtVaSetValues(notebook,
		XtNnotebookSelect, select_tab,
		(char *)0);
	select_tab(notebook, 0);

	mainloop();
	return 0;
}
