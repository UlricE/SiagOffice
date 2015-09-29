/*
   Copyright (C) 2002-2005  Ulric Eriksson <ulric@siag.nu>

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

enum {START, TAG, CHAR, COMMENT1, COMMENT2, COMMENT3, COMMENT4, COMMENT5, END};

static hnode *h, *ch;

static char *b = NULL;

static int n, nmax = 0;

static struct {
	char *name;
	int value;
} cchar[] = {
	{"quot", '"'}, {"amp", '&'}, {"lt", '<'}, {"gt", '>'},
	{"nbsp", 160}, {"iexcl", 161}, {"cent", 162}, {"pound", 163},
	{"curren", 164}, {"yen", 165}, {"brvbar", 166}, {"sect", 167},
	{"uml", 168}, {"copy", 169}, {"ordf", 170}, {"laquo", 171},
	{"not", 172}, {"shy", 173}, {"reg", 174}, {"macr", 175},
	{"deg", 176}, {"plusmn", 177}, {"sup2", 178}, {"sup3", 179},
	{"acute", 180}, {"micro", 181}, {"para", 182}, {"middot", 183},
	{"cedil", 184}, {"sup1", 185}, {"ordm", 186}, {"raquo", 187},
	{"frac14", 188}, {"frac12", 189}, {"frac34", 190}, {"iquest", 191},
	{"Agrave", 192}, {"Aacute", 193}, {"Acirc", 194}, {"Atilde", 195},
	{"Auml", 196}, {"Aring", 197}, {"AElig", 198}, {"Ccedil", 199},
	{"Egrave", 200}, {"Eacute", 201}, {"Ecirc", 202}, {"Euml", 203},
	{"Igrave", 204}, {"Iacute", 205}, {"Icirc", 206}, {"Euml", 207},
	{"ETH", 208}, {"Ntilde", 209}, {"Ograve", 210}, {"Oacute", 211},
	{"Ocirc", 212}, {"Otilde", 213}, {"Ouml", 214}, {"times", 215},
	{"Oslash", 216}, {"Ugrave", 217}, {"Uacute", 218}, {"Ucirc", 219},
	{"Uuml", 220}, {"Yacute", 221}, {"THORN", 222}, {"szlig", 223},
	{"agrave", 224}, {"aacute", 225}, {"acirc", 226}, {"atilde", 227},
	{"auml", 228}, {"aring", 229}, {"aelig", 230}, {"ccedil", 231},
	{"egrave", 232}, {"eacute", 233}, {"ecirc", 234}, {"euml", 235},
	{"igrave", 236}, {"iacute", 237}, {"icirc", 238}, {"iuml", 239},
	{"eth", 240}, {"ntilde", 241}, {"ograve", 242}, {"oacute", 243},
	{"ocirc", 244}, {"otilde", 245}, {"ouml", 246}, {"divide", 247},
	{"slash", 248}, {"ugrave", 249}, {"uacute", 250}, {"ucirc", 251},
	{"uuml", 252}, {"yacute", 253}, {"thorn", 254}, {"yuml", 255},
	{NULL, 0}
};

/* convert Auml to Ä and #33 to ! */
static int from_cchar(char *from)
{
	int i;

	if (from[0] == '#') {
		i = atoi(from+1);
		if (i >= ' ' && i <= 255) return i;
		return -1;
	}
	for (i = 0; cchar[i].name; i++) {
		if (!strcmp(cchar[i].name, from)) return cchar[i].value;
	}
	return -1;
}

static hnode *new_hnode(void)
{
	hnode *h = cmalloc(sizeof *h);
	h->tag = NULL;
	h->text = NULL;
	h->parent = NULL;
	h->next = NULL;
	h->child = NULL;
	return h;
}

static void indent(int i)
{
	int j;

	for (j = 0; j < i; j++) putchar(' ');
}

void free_html(hnode *h)
{
	if (h == NULL) return;
	if (h->tag) free(h->tag);
	if (h->text) free(h->text);
	free_html(h->next);
	free_html(h->child);
}

void dump_html(hnode *h, int i)
{
	if (h == NULL) return;
	if (h->tag == NULL) {
		indent(i);
		printf("Text: '%s'\n", h->text);
	} else {
		indent(i);
		printf("Tag: '%s'\n", h->tag);
		if (h->text) {
			indent(i+4);
			printf("Value: '%s'\n", h->text);
		}
	}
	dump_html(h->child, i+2);
	dump_html(h->next, i);
}

static void growb(int n)
{
	if (n >= nmax) {
		nmax = n+1000;
		b = crealloc(b, nmax+1);
	}
}

static void clearb(void)
{
	n = 0;
	growb(n+1);
	b[n] = '\0';
}

static hnode *last_node(hnode *h)
{
	if (h == NULL) return NULL;
	if (h->next == NULL) return h;
	return last_node(h->next);
}

/*
When we enter ins_node, ch points to the most recently inserted node.
The new node is inserted as a child to this node. When we are done,
ch points to the newly inserted node.

Special case: If tree is empty, ch is NULL. In that case, the new
node becomes the root of the tree.
*/

static void ins_node(hnode *n)
{
	hnode *ph;

	if (ch == NULL) {
		h = n;
	} else if ((ph = last_node(ch->child)) == NULL) {
		ch->child = n;
	} else {
		ph->next = n;
	}
	n->parent = ch;
	ch = n;
}

static hnode *match_tag(char *p)
{
	hnode *h;

	for (h = ch; h; h = h->parent) {
		if (h->tag && !strcmp(h->tag, p)) break;
	}
	return h;
}

static void tag_cb(char *p)
{
	hnode *h;
	char *q;
	char b[1024];
	int n, closer = 0;

	if (p == NULL) return;		/* should never happen */
	q = p;
	while (isspace(*q)) q++;
	if (q[0] == '\0') return;	/* ignore empty tag */

	n = strlen(q);
	if (n && q[n-1] == '/') {
		q[n-1] = '\0';
		closer = 1;
	}

	n = 0;
	while (*q && !isspace(*q)) b[n++] = toupper(*q++);
	b[n] = '\0';
	while (*q && isspace(*q)) q++;	/* skip past white space */
	if (p[0] == '\0') return;
	if (p[0] == '/') {
		/* closing tag */
		h = match_tag(b+1);
		if (h == NULL) {
			fprintf(stderr, "Warning: bogus closer '%s'\n", p);
		} else {
			ch = h->parent;
		}
	} else {
		h = new_hnode();
		h->tag = cstrdup(b);
		if (*q) h->text = cstrdup(q);
		ins_node(h);
		if (closer) ch = ch->parent;
	}
}

static int extchar(char *p)
{
	int c = from_cchar(p);
	if (c == -1) c = '?';
	return c;
}

static void text_cb(char *p)
{
	hnode *h;

	if (p == NULL || p[0] == '\0') return;
	h = new_hnode();
	h->text = cstrdup(p);
	ins_node(h);
	ch = ch->parent;
}

static void char_cb(int c)
{
	growb(n+1);
	b[n++] = c;
	b[n] = '\0';
}

hnode *parse_html(int (*getc_cb)(void *), void *closure)
{
	int state = 0, error = 0;
	int c;
	char t[1024];
	int m = 0;

	ch = h = NULL;
	n = 0;
	while (state != END) {
		c = getc_cb(closure);
		switch (state) {
		case START:
			if (c == '<') {
				text_cb(b);
				clearb();
				state = TAG;
			} else if (c == '&') {
				m = 0;
				state = CHAR;
			} else if (c == '\0') {
				text_cb(b);
				state = END;
			} else {
				char_cb(c);
			}
			break;
		case TAG:
			if (c == '>') {
				tag_cb(b);
				clearb();
				state = START;
			} else if (c == '!') {
				state = COMMENT1;
			} else if (c == '\0') {
				error = 1;
				state = END;
			} else {
				char_cb(c);
			}
			break;
		case COMMENT1:
			if (c == '-') {
				state = COMMENT2;
			} else if (c == '\0') {
				error = 1;
				state = END;
			} else {
				char_cb('!');
				char_cb(c);
				state = TAG;
			}
			break;
		case COMMENT2:
			if (c == '-') {
				state = COMMENT3;
			} else if (c == '\0') {
				error = 1;
				state = END;
			} else {
				char_cb('!');
				char_cb('-');
				char_cb(c);
				state = TAG;
			}
			break;
		case COMMENT3:
			if (c == '-') {
				state = COMMENT4;
			} else if (c == '\0') {
				error = 1;
				state = END;
			}
			break;
		case COMMENT4:
			if (c == '-') {
				state = COMMENT5;
			} else if (c == '\0') {
				error = 1;
				state = END;
			} else {
				state = COMMENT3;
			}
			break;
		case COMMENT5:
			if (c == '>') {
				clearb();
				state = START;
			} else if (c == '\0') {
				error = 1;
				state = END;
			} else {
				state = COMMENT3;
			}
			break;
		case CHAR:
			if (c == ';') {
				t[m] = '\0';
				char_cb(extchar(t));
				state = START;
			} else if (c == '\0') {
				error = 1;
				state = END;
			} else {
				if (m+1 < sizeof t) t[m++] = c;
			}
			break;
		default:
			error = 1;
			state = END;
		}
	}
	if (error) {
		fprintf(stderr, "Error\n");
	}
	return h;
}

/* p looks like this: key=value key2="value 2". Keys are not case
   sensitive. Everything not in quotes is converted to upper case.
   Returns value if found, otherwise NULL.
*/
char *get_value(char *k, char *p, char *b)
{
	char *q = p;
	int n = strlen(k);
	int c, i;

	if (q == NULL) return NULL;
Again:
	while (*q && isspace(*q)) q++;  /* skip leading space */
	if (!cstrncasecmp(k, q, n) && q[n] == '=') {     /* gotcha */
		n++;
		i = 0;
		c = q[n];
		if (c == '"' || c == '\'') {
			n++;
			while (q[n] && q[n] != c) b[i++] = q[n++];
		} else {
			while (q[n] && !isspace(q[n])) b[i++] = q[n++];
		}
		b[i] = '\0';
		return b;
	}
	while (*q && !isspace(*q)) q++;
	if (*q) goto Again;
	return NULL;
}

static void malloc_error(size_t n)
{
	fprintf(stderr, "Error allocating %ld bytes\n", (long)n);
	exit(EXIT_FAILURE);
}

void *cmalloc(size_t n)
{
	void *q = malloc(n);
	if (q == NULL) malloc_error(n);
	return q;
}

void *crealloc(void *p, size_t n)
{
	void *q = realloc(p, n);
	if (q == NULL) malloc_error(n);
	return q;
}

char *cstrdup(const char *p)
{
	int n = strlen(p);
	char *q = cmalloc(n+1);
	strcpy(q, p);
	return q;
}

int cstrncasecmp(const char *p, const char *q, size_t n)
{
	size_t i;
	int c = 0;

	for (i = 0; i < n && p[i]; i++) {
		if ((c = toupper(p[i])-toupper(q[i]))) break;
	}
	return c;
}

