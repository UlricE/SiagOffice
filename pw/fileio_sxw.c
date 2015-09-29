/*
   Pathetic Writer
   Copyright (C) 2003  Ulric Eriksson <ulric@siag.nu>

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

/*
 * fileio_sxw.c
 */

/*
How this stuff works:

The OpenOffice.org format is basically a zip archive. One of the
files in the archive is a file in XML format called content.xml.
This file is parsed into a tree by routines in common/o3read.c.

Being an XML file, all headers, paragraphs and so on are delimited
by start and end tags, unlike HTML where many tags are optional.

When we see a start tag, we call the newline function which
increments the line number and sets the style and format.
It also sets the "beginning of paragraph" flag on the line.
During this pass, the whole paragraph is put in a single line
with no regard to width or height.

Finally all lines are broken and line heights calculated.

Pretty simple really. The only thing that might be confusing
is that newline is called at the *beginning* of each paragraph.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pw.h"
#include "../common/common.h"
#include "../common/o3read.h"
#include <Mowitz/MwUtils.h>

enum {LIST_NONE=0, LIST_UL, LIST_OL, LIST_DL};

typedef struct o3style {
	char *name;
	char bold, italic, underline;
} o3style;

static int list;	/* list style */
static int ll;		/* item number */
static int indent;	/* amount of space at beginning of paragraph */

static o3style *styles = NULL;
static int nstyles = 0;
static int href;
static char **hrefs;

static int sty;
static int fmt;
static unsigned int row;
static buffer *buf;
static int sht;

static void tag_style_properties(hnode *);
static void tag_style_style(hnode *);
static void tag_table_table(hnode *);
static void tag_table_row(hnode *);
static void tag_table_cell(hnode *);
static void tag_text_a(hnode *);
static void tag_text_h(hnode *);
static void tag_text_list_item(hnode *);
static void tag_text_ordered_list(hnode *);
static void tag_text_p(hnode *);
static void tag_text_s(hnode *);
static void tag_text_span(hnode *);
static void tag_text_tab_stop(hnode *);
static void tag_text_unordered_list(hnode *);
static void tag_unknown(hnode *);
static void text(hnode *);

static struct {
	char *name;
	void (*action)(hnode *);
} tag[] = {
	{"STYLE:STYLE", tag_style_style},
	{"STYLE:PROPERTIES", tag_style_properties},
	{"TABLE:TABLE", tag_table_table},
	{"TABLE:TABLE-CELL", tag_table_cell},
	{"TABLE:TABLE-ROW", tag_table_row},
	{"TEXT:A", tag_text_a},
	{"TEXT:H", tag_text_h},
	{"TEXT:LIST-ITEM", tag_text_list_item},
	{"TEXT:ORDERED-LIST", tag_text_ordered_list},
	{"TEXT:P", tag_text_p},
	{"TEXT:S", tag_text_s},
	{"TEXT:SPAN", tag_text_span},
	{"TEXT:TAB-STOP", tag_text_tab_stop},
	{"TEXT:UNORDERED-LIST", tag_text_unordered_list},
	{NULL, tag_unknown}
};

static void emitchar(int c)
{
	if (c == '\n') return;
	ins_char(buf, sht, row, line_length(buf, sht, row), c, fmt);
}

static void emitstr(char *p)
{
	while (*p) emitchar(*p++);
}

static void newline()
{
	int i;
	char b[100];

	row++;
	set_bop(buf, sht, row, 1);
	set_style(buf, sht, row, sty);
	fmt = style_table[sty].format;
	switch (list) {
	case LIST_UL:
		emitstr("-");
		break;
	case LIST_OL:
		sprintf(b, "%d", ll);
		emitstr(b);
		break;
	default:
		break;
	}
	for (i = 0; i < indent; i++)
		emitchar(' ');
}

static void tree(hnode *h)
{
	void (*action)(hnode *);
	int i;

	if (h == NULL) return;
	if (h->tag == NULL) {
		text(h);
	} else {
		for (i = 0; tag[i].name; i++) {
			if (!strcmp(tag[i].name, h->tag)) break;
		}
		action = tag[i].action;
		(*action)(h);
	}
}

/* tag handlers are in alphabetical order so I can find them... */

static void tag_style_properties(hnode *h)
{
	char b[1024], *p;
	p = get_value("FO:FONT-STYLE", h->text, b);
	if (p && !strcmp(p, "italic")) styles[nstyles].italic = 1;
	p = get_value("FO:FONT-WEIGHT", h->text, b);
	if (p && !strcmp(p, "bold")) styles[nstyles].bold = 1;
	p = get_value("STYLE:TEXT-UNDERLINE", h->text, b);
	if (p && !strcmp(p, "single")) styles[nstyles].underline = 1;
	tree(h->child);
	tree(h->next);
}

/* Begin a new style definition */
static void tag_style_style(hnode *h)
{
	char b[1024], *p;
	p = get_value("STYLE:NAME", h->text, b);
	if (p) strcpy(b, p);
	else strcpy(b, "unknown style");
	styles = crealloc(styles, (nstyles+1)*sizeof *styles);
	styles[nstyles].name = cstrdup(b);
	styles[nstyles].bold = 0;
	styles[nstyles].italic = 0;
	styles[nstyles].underline = 0;
	tree(h->child);
	nstyles++;
	tree(h->next);
}

static void tag_table_table(hnode *h)
{
	char b[1024], *p;
	int oldsty = sty, oldfmt = fmt;
	p = get_value("TABLE:NAME", h->text, b);
	if (p) {
		sty = MW_STY_HEADER1;
		newline();
		emitstr(p);
	}
	sty = MW_STY_DEFAULT;
	fmt = style_table[sty].format;
	tree(h->child);
	sty = oldsty;
	fmt = oldfmt;
	tree(h->next);
}

static void tag_table_cell(hnode *h)
{
	tree(h->child);
	emitchar(' ');
	tree(h->next);
}

static void tag_table_row(hnode *h)
{
	newline();
	tree(h->child);
	tree(h->next);
}

static void tag_text_a(hnode *h)
{
	char b[1024], *p;
	int oldfmt = fmt;
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);

	p = get_value("XLINK:HREF", h->text, b);
	if (p) {
		ft.uline = 1;
		fmt = MwEncodeFormat(~0, &ft);
	}

	tree(h->child);
	fmt = oldfmt;
	tree(h->next);
}

/* Heading */
static void tag_text_h(hnode *h)
{
	char b[1024], *p;
	int level = 1;
	int oldsty = sty, oldfmt = fmt;

	p = get_value("TEXT:LEVEL", h->text, b);
	if (p) {
		level = atoi(p);
	}
	if (level < 1) level = 1;
	if (level > 6) level = 6;
	switch (level) {
	case 1: sty = MW_STY_HEADER1; break;
	case 2: sty = MW_STY_HEADER2; break;
	case 3: sty = MW_STY_HEADER3; break;
	case 4: sty = MW_STY_HEADER4; break;
	case 5: sty = MW_STY_HEADER5; break;
	default: sty = MW_STY_HEADER6; break;
	}
	newline();
	tree(h->child);
	sty = oldsty;
	fmt = oldfmt;
	tree(h->next);
}

static void tag_text_list_item(hnode *h)
{
	newline();
	tree(h->child);
	ll++;
	tree(h->next);
}

static void tag_text_ordered_list(hnode *h)
{
	int oldlist = list, oldll = ll, oldindent = indent;
	list = LIST_OL;
	ll = 1;
	indent += 2;
	tree(h->child);
	list = oldlist, ll = oldll, indent = oldindent;
	tree(h->next);
}

/* Paragraph break */
static void tag_text_p(hnode *h)
{
	char b[1024], *p;
	int oldsty = sty;
	int oldfmt = fmt;


	p = get_value("TEXT:STYLE-NAME", h->text, b);
	if (p) {
		if (!strcmp(p, "Heading")) {
			sty = MW_STY_HEADER1;
		}
	}
	newline();
	tree(h->child);
	sty = oldsty;
	fmt = oldfmt;
	tree(h->next);
}

static void tag_text_s(hnode *h)
{
	printf(" ");    /* what do I know */
	tree(h->child);
	tree(h->next);
}

static void tag_text_span(hnode *h)
{
	int i;
	int oldsty = sty, oldfmt = fmt;
	char b[1024], *p = get_value("TEXT:STYLE-NAME", h->text, b);
MwFmt ft;
MwDecodeFormat(fmt, ~0, &ft);
	if (p == NULL) p = "xxx";
	for (i = 0; i < nstyles; i++) if (!strcmp(p, styles[i].name)) break;
	if (i < nstyles) {
		if (styles[i].bold) ft.bold = 1;
		if (styles[i].italic) ft.italic = 1;
		if (styles[i].underline) ft.uline = 1;
	}
	fmt = MwEncodeFormat(~0, &ft);
	tree(h->child);
	sty = oldsty, fmt = oldfmt;
	tree(h->next);
}

static void tag_text_tab_stop(hnode *h)
{
	emitstr(" ");    /* for lack of better */
	tree(h->child);
	tree(h->next);
}

static void tag_text_unordered_list(hnode *h)
{
	tree(h->child);
	tree(h->next);
}

/* Handles tags we don't have handlers for. Ignore the tag, do children */
static void tag_unknown(hnode *h)
{
	/* ignore the tag */
	tree(h->child);
	tree(h->next);
}

/* Copy text onto screen. */
static void text(hnode *h)
{
	emitstr(h->text);
	tree(h->next);
}

static int nextc(void *closure)
{
	FILE *fp = closure;
	int c = get_utf8_char(fp);
	if (c == EOF) return '\0';
	return c;
}

static int load(char *fn, buffer *bu)
/* Returns: 0 if successful, otherwise 1 */
{
	FILE *fp;
	char cmd[1024];
	hnode *h;
	row = 0;
	buf = bu;
	sht = 0;
	list = LIST_NONE;
	ll = 0;
	indent = 0;
	sty = MW_STY_DEFAULT;
	fmt = style_table[sty].format;
	buf->height_interest = 0;

	href = 0;
	hrefs = NULL;

	MwSnprintf(cmd, sizeof cmd,
		"unzip -p '%s' content.xml", fn);
	fp = popen(cmd, "r");
	if (!fp) {
		return 1;
	}
	h = parse_html(nextc, fp);
	if (h == NULL) {
		pclose(fp);
		return 1;
	}
	pclose(fp);
	tree(h);

	free_html(h);

	buf->height_interest = 1;
	for (row = 1; row <= line_last_used(buf, sht); row++) {
		rebreak_line(buf, sht, row);
		check_line_height(buf, sht, row);
	}
	buf->change = 0;

	return 0;
}

/* ---
Accept any file with .sxw as extension
*/

static int myformat(char *fn)
{
	char *ext;

	ext = strrchr(fn, '.');
	if (!ext) return 0;     /* no extension */
	if (MwStrcasecmp(ext, ".sxw"))
		return 0;       /* wrong extension */
	return 1;
}

/* ---
*/
void fileio_sxw_init(void)
{
	register_format(load, NULL, myformat, "OpenOffice.org swriter (*.sxw)");
}

