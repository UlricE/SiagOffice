/*
   Siag, Scheme In A Grid
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
 * fileio_sxc.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <Mowitz/MwFormat.h>
#include "../common/common.h"
#include "../common/o3read.h"
#include <Mowitz/MwUtils.h>

#include "calc.h"

enum {LIST_NONE=0, LIST_UL, LIST_OL, LIST_DL};

typedef struct o3style {
	char *name;
	char bold, italic, underline;
} o3style;

typedef struct hstate {
	int pre;
	int list;
	int ll;
	int indent;
} hstate;

static o3style *styles = NULL;
static int nstyles = 0;
static int href;
static char **hrefs;

static int tbi, obi, mute, pre, sty;
static int fmt;

static unsigned int sht, row, col;
static buffer *buf;

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

static char emitbuf[1024];
static int emitn;

static void emitchar(int c)
{
	if (emitn >= (sizeof emitbuf)-1) return;
        if (c == '\n') c = ' ';
	emitbuf[emitn++] = c;
	emitbuf[emitn] = '\0';
putchar(c);
}

static void emitstr(char *p)
{
        while (*p) emitchar(*p++);
}

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

static void allocsheet(buffer *b)
{
	if (sht == b->nsht) buffer_add_sheet(b, sht);
}

static void sheetname(buffer *b, char *n)
{
	while (*n && isspace(*n)) n++;
	allocsheet(b);
	buffer_rename_sheet(b, sht, n);
}

static void tag_table_table(hnode *h, hstate *s)
{
        char b[1024], *p;
        p = get_value("TABLE:NAME", h->text, b);
	row = 1;
        if (p == NULL) {
		sprintf(b, _("Sheet %s"), sht);
		p = b;
        }
	sheetname(buf, p);
        tree(h->child, s);
	sht++;
        tree(h->next, s);
}

static void tag_table_cell(hnode *h, hstate *s)
{
	int type = EMPTY, repeat = 1, i;
	char *p, b[1024];
	cval value;
	value.number = 0;
	p = get_value("TABLE:NUMBER-COLUMNS-REPEATED", h->text, b);
	if (p) repeat = atoi(p);
	p = get_value("TABLE:VALUE-TYPE", h->text, b);
	if (p == NULL) type = EMPTY;
	else if (!strcmp(p, "float")) {
		type = CONSTANT;
		p = get_value("TABLE:VALUE", h->text, b);
		if (p) value.number = strtod(p, NULL);
		p = get_value("TABLE:FORMULA", h->text, b);
		if (p) {
		}
	}
	emitn = 0;
        tree(h->child, s);
	emitbuf[emitn] = '\0';
	if (type == EMPTY && emitn > 0) type = LABEL;
	for (i = 0; i < repeat; i++) {
		switch (type) {
		case EMPTY:
			break;
		case CONSTANT:
			ins_data(buf, C_interpreter, emitbuf,
				value, CONSTANT, sht, row, col);
			ins_format(buf, sht, row, col, fmt);
			break;
		default:	/* anything else is a label */
			ins_data(buf, C_interpreter, emitbuf,
				value, LABEL, sht, row, col);
			ins_format(buf, sht, row, col, fmt);
		}
		col++;
	}
        tree(h->next, s);
}

static void tag_table_row(hnode *h, hstate *s)
{
	col = 1;
        tree(h->child, s);
	row++;
        tree(h->next, s);
}

static void tag_text_a(hnode *h, hstate *s)
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

        tree(h->child, s);
        fmt = oldfmt;
        tree(h->next, s);
}

/* Heading */
static void tag_text_h(hnode *h, hstate *s)
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
#if 0
        fmt = style_table[sty].format;
#endif
        tree(h->child, s);
        sty = oldsty;
        fmt = oldfmt;
        tree(h->next, s);
}

static void tag_text_list_item(hnode *h, hstate *s)
{
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
        char b[1024], *p;
        int oldsty = sty;
        int oldfmt = fmt;


        p = get_value("TEXT:STYLE-NAME", h->text, b);
        if (p) {
                if (!strcmp(p, "Heading")) {
                        sty = MW_STY_HEADER1;
#if 0
                        fmt = style_table[sty].format;
#endif
                }
        }
        tree(h->child, s);
        sty = oldsty;
        fmt = oldfmt;
        tree(h->next, s);
}

static void tag_text_s(hnode *h, hstate *s)
{
        printf(" ");    /* what do I know */
        tree(h->child, s);
        tree(h->next, s);
}

static void tag_text_span(hnode *h, hstate *s)
{
        int i;
        int oldsty = sty, oldfmt = fmt;
        char b[1024], *p = get_value("TEXT:STYLE-NAME", h->text, b);
MwFmt ft;
MwDecodeFormat(fmt, ~0, &ft);
        if (p == NULL) p = "xxx";
        for (i = 0; i < nstyles; i++) if (!strcmp(p, styles[i].name)) break;
        if (i < nstyles) {
                if (styles[i].bold) ft.bold = 1/*printf("<b>")*/;
                if (styles[i].italic) ft.italic = 1/*printf("<i>")*/;
                if (styles[i].underline) ft.uline = 1/*printf("<u>")*/;
        }
        fmt = MwEncodeFormat(~0, &ft);
        tree(h->child, s);
        sty = oldsty, fmt = oldfmt;
        tree(h->next, s);
}

static void tag_text_tab_stop(hnode *h, hstate *s)
{
        printf(" ");    /* for lack of better */
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
#if 0
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
#else
        emitstr(h->text);
#endif
        tree(h->next, s);
}

static int nextc(void *closure)
{
        FILE *fp = closure;
        int c = get_utf8_char(fp);
        if (c == EOF) return '\0';
        return c;
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int load(char *fn, buffer *bu)
{
	hnode *h;
	char cmd[1024];
	hstate *s;
	FILE *fp;
	cval value;
	value.number = 0;
	mute = 0;
	obi = 0;
	tbi = 0;
	pre = 0;
	row = 0;
	buf = bu;
	sht = 0;
	sty = MW_STY_DEFAULT;
	fmt = 0;

	href = 0;
	hrefs = NULL;

	MwSnprintf(cmd, sizeof cmd,
		"unzip -p '%s' content.xml", fn);
	fp = popen(cmd, "r");
	if (fp == NULL) {
		return 1;
	}
	h = parse_html(nextc, fp);
	if (h == NULL) {
		pclose(fp);
		return 1;
	}
	pclose(fp);
	s = new_hstate();
	tree(h, s);
	free_html(h);
	free_hstate(s);

	return 0;
} /* load */

/* ---
Accept any file with .sxc as extension
*/

static int myformat(char *fn)
{
	char *ext;

	ext = strrchr(fn, '.');
	if (!ext) return 0;	/* no extension */
	if (MwStrcasecmp(ext, ".sxc"))
		return 0;	/* wrong extension */
	return 1;
}

/* ---
*/
void fileio_sxc_init(void)
{
	register_format(load, NULL, myformat, "OpenOffice.org scalc (*.sxc)");
}

