/*
   Copyright (C) 2002  Ulric Eriksson <ulric@siag.nu>

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
#include <string.h>
#include <ctype.h>

#include "o3read.h"

enum {LIST_NONE=0, LIST_UL, LIST_OL, LIST_DL};

typedef struct style {
	char *name;
	char bold;
	char italic;
	char underline;
} style;

typedef struct hstate {
	int pre;	/* preformatted, default 0 = false */
	int list;	/* list mode, default LIST_NONE */
	int ll;		/* list level, starts with 1 */
	int indent;	/* indentation level, default 0 */
} hstate;

static style *styles = NULL;
static int nstyles = 0;

static int href;	/* links, default 0 */
static char **hrefs;

static void tag_style_properties(hnode *, hstate *);
static void tag_style_style(hnode *, hstate *);
static void tag_table_table(hnode *, hstate *);
static void tag_table_row(hnode *, hstate *);
static void tag_table_cell(hnode *, hstate *);
static void tag_text_a(hnode *, hstate *);
static void tag_text_h(hnode *, hstate *);
static void tag_text_list_item(hnode *, hstate *);
static void tag_text_ordered_list(hnode *, hstate *);
static void tag_text_p(hnode *, hstate *);
static void tag_text_s(hnode *, hstate *);
static void tag_text_span(hnode *, hstate *);
static void tag_text_tab_stop(hnode *, hstate *);
static void tag_text_unordered_list(hnode *, hstate *);
static void tag_unknown(hnode *, hstate *);
static void text(hnode *, hstate *);

static struct {
	char *name;
	void (*action)(hnode *, hstate *);
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

static hstate *new_hstate(void)
{
	hstate *s = cmalloc(sizeof *s);
	s->pre = 0;
	s->indent = 0;
	s->list = LIST_NONE;
	s->ll = 0;
	return s;
}

static void free_hstate(hstate *s)
{
	free(s);
}

static void newline(hstate *s)
{
	int i;

	putchar('\n');
	for (i = 0; i < s->indent; i++)
		putchar(' ');
}

static void tree(hnode *h, hstate *s)
{
	void (*action)(hnode *, hstate *);
	int i;

	if (h == NULL) return;
	if (h->tag == NULL) {
		text(h, s);
	} else {
		for (i = 0; tag[i].name; i++) {
			if (!strcmp(tag[i].name, h->tag)) break;
		}
		action = tag[i].action;
		(*action)(h, s);
	}
}

/* tag handlers are in alphabetical order so I can find them... */

static void tag_style_properties(hnode *h, hstate *s)
{
	char b[1024], *p;
	p = get_value("FO:FONT-STYLE", h->text, b);
	if (p && !strcmp(p, "italic")) styles[nstyles].italic = 1;
	p = get_value("FO:FONT-WEIGHT", h->text, b);
	if (p && !strcmp(p, "bold")) styles[nstyles].bold = 1;
	p = get_value("STYLE:TEXT-UNDERLINE", h->text, b);
	if (p && !strcmp(p, "single")) styles[nstyles].underline = 1;
	tree(h->child, s);
	tree(h->next, s);
}

/* Begin a new style definition */
static void tag_style_style(hnode *h, hstate *s)
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
	tree(h->child, s);
	nstyles++;
	tree(h->next, s);
}

static void tag_table_table(hnode *h, hstate *s)
{
	char b[1024], *p;
	p = get_value("TABLE:NAME", h->text, b);
	if (p) printf("<h1>%s</h1>\n", p);
	printf("<table>\n");
	tree(h->child, s);
	printf("</table>\n");
	tree(h->next, s);
}

static void tag_table_cell(hnode *h, hstate *s)
{
	printf("<td>");
	tree(h->child, s);
	printf("</td>");
	tree(h->next, s);
}

static void tag_table_row(hnode *h, hstate *s)
{
	printf("<tr>");
	tree(h->child, s);
	printf("</tr>");
	tree(h->next, s);
}

static void tag_text_a(hnode *h, hstate *s)
{
	char b[1024], *p;

	p = get_value("XLINK:HREF", h->text, b);
	if (p) printf("<a href=\"%s\">", p);
	tree(h->child, s);
	if (p) printf("</a>");
	tree(h->next, s);
}

/* Heading */
static void tag_text_h(hnode *h, hstate *s)
{
	char b[1024], *p;
	int level = 1;

	newline(s);

	p = get_value("TEXT:LEVEL", h->text, b);
	if (p) {
		level = atoi(p);
	}
	if (level < 1) level = 1;
	if (level > 6) level = 6;
	printf("<h%d>", level);
	tree(h->child, s);
	printf("</h%d>\n", level);
	tree(h->next, s);
}

static void tag_text_list_item(hnode *h, hstate *s)
{
	printf("<li>");
	tree(h->child, s);
	tree(h->next, s);
}

static void tag_text_ordered_list(hnode *h, hstate *s)
{
	printf("<ol>\n");
	tree(h->child, s);
	printf("</ol>\n");
	tree(h->next, s);
}

/* Paragraph break */
static void tag_text_p(hnode *h, hstate *s)
{
	char b[1024], *p, *t = "p";

	newline(s);

	p = get_value("TEXT:STYLE-NAME", h->text, b);
	if (p) {
		if (!strcmp(p, "Heading")) t = "h1";
	}
	printf("<%s>", t);
	tree(h->child, s);
	printf("</%s>\n", t);
	tree(h->next, s);
}

static void tag_text_s(hnode *h, hstate *s)
{
	printf(" ");	/* what do I know */
	tree(h->child, s);
	tree(h->next, s);
}

static void tag_text_span(hnode *h, hstate *s)
{
	int i;
	char b[1024], *p = get_value("TEXT:STYLE-NAME", h->text, b);
	if (p == NULL) p = "xxx";
	for (i = 0; i < nstyles; i++) if (!strcmp(p, styles[i].name)) break;
	if (i < nstyles) {
		if (styles[i].bold) printf("<b>");
		if (styles[i].italic) printf("<i>");
		if (styles[i].underline) printf("<u>");
	}
	tree(h->child, s);
	if (i < nstyles) {
		if (styles[i].underline) printf("</u>");
		if (styles[i].italic) printf("</i>");
		if (styles[i].bold) printf("</b>");
	}
	tree(h->next, s);
}

static void tag_text_tab_stop(hnode *h, hstate *s)
{
	printf(" ");	/* for lack of better */
	tree(h->child, s);
	tree(h->next, s);
}

static void tag_text_unordered_list(hnode *h, hstate *s)
{
	printf("<ul>\n");
	tree(h->child, s);
	printf("</ul>\n");
	tree(h->next, s);
}

/* Handles tags we don't have handlers for. Ignore the tag, do children */
static void tag_unknown(hnode *h, hstate *s)
{
	/* ignore the tag */
	tree(h->child, s);
	tree(h->next, s);
}

/* Copy text onto screen. Treats whitespace differently if this
   is "preformatted".
*/
static void text(hnode *h, hstate *s)
{
	int i;
	for (i = 0; h->text[i]; i++) {
		switch (h->text[i]) {
		case '<':
			printf("&lt;");
			break;
		case '>':
			printf("&gt;");
			break;
		default:
			putchar(h->text[i]);
		}
	}
	/* can't have children */
	tree(h->next, s);
}

static int nextc(void *closure)
{
	FILE *fp = closure;
	int c = getc(fp);
	if (c == EOF) return '\0';
	return c;
}

static void usage(void)
{
	printf("usage: lmb url\n");
	exit(0);
}

int main(int argc, char **argv)
{
	hnode *h;
	hstate *s;

	href = 0;
	hrefs = NULL;

	h = parse_html(nextc, stdin);
	if (h == NULL) usage();

	s = new_hstate();
	printf("<html><head><title>HTML by o3tohtml</title></head><body>\n");
	tree(h, s);
	newline(s);
	printf("</body></html>\n");

	free_html(h);
	free_hstate(s);
	return 0;
}

