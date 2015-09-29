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

#if 0
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
#else
static void tag_delete(hnode *, hstate *);
static void tag_emphasis(hnode *, hstate *);
static void tag_entry(hnode *, hstate *);
static void tag_informaltable(hnode *, hstate *);
static void tag_itemizedlist(hnode *, hstate *);
static void tag_listitem(hnode *, hstate *);
static void tag_orderedlist(hnode *, hstate *);
static void tag_para(hnode *, hstate *);
static void tag_row(hnode *, hstate *);
static void tag_unknown(hnode *, hstate *);
#endif

static void text(hnode *, hstate *);

static struct {
	char *name;
	void (*action)(hnode *, hstate *);
} tag[] = {
#if 0
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
#else
	{"BOOKINFO", tag_delete},
	{"EMPHASIS", tag_emphasis},
	{"ENTRY", tag_entry},
	{"INFORMALTABLE", tag_informaltable},
	{"ITEMIZEDLIST", tag_itemizedlist},
	{"LISTITEM", tag_listitem},
	{"ORDEREDLIST", tag_orderedlist},
	{"PARA", tag_para},
	{"ROW", tag_row},
	{"TITLE", tag_delete},
#endif
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

/* swallow this */
static void tag_delete(hnode *h, hstate *s)
{
	tree(h->next, s);
}

static void tag_emphasis(hnode *h, hstate *s)
{
	char b[1024], *t = "em", *p = get_value("ROLE", h->text, b);
	if (p && !strcmp(p, "bold")) t = "b";
	else if (p && !strcmp(p, "underline")) t = "u";
	printf("<%s>\n", t);
	tree(h->child, s);
	printf("</%s>\n", t);
	tree(h->next, s);
}

static void tag_entry(hnode *h, hstate *s)
{
	printf("<td>");
	tree(h->child, s);
	printf("</td>");
	tree(h->next, s);
}

/* treat this as unordered list */
static void tag_itemizedlist(hnode *h, hstate *s)
{
	char b[1024], *t = "ul", *p = get_value("MARK", h->text, b);
	if (p && !strcmp(p, "bullet")) t = "ul";
	printf("<%s>\n", t);
	tree(h->child, s);
	printf("</%s>\n", t);
	tree(h->next, s);
}

static void tag_informaltable(hnode *h, hstate *s)
{
	printf("<table>");
	tree(h->child, s);
	printf("</table>");
	tree(h->next, s);
}

static void tag_listitem(hnode *h, hstate *s)
{
	printf("<li>");
	tree(h->child, s);
	tree(h->next, s);
}

/* treat this as ordered list */
static void tag_orderedlist(hnode *h, hstate *s)
{
	char b[1024], *t = "ol", *p = get_value("NUMERATION", h->text, b);
	if (p && !strcmp(p, "fnork")) t = "ol";
	printf("<%s>\n", t);
	tree(h->child, s);
	printf("</%s>\n", t);
	tree(h->next, s);
}

static void tag_para(hnode *h, hstate *s)
{
	printf("<p>");
	tree(h->child, s);
	printf("</p>");
	tree(h->next, s);
}

static void tag_row(hnode *h, hstate *s)
{
	printf("<tr>");
	tree(h->child, s);
	printf("</tr>");
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
	printf("<html><head><title>HTML by dbtohtml</title></head><body>\n");
	tree(h, s);
	newline(s);
	printf("</body></html>\n");

	free_html(h);
	free_hstate(s);
	return 0;
}

