/*
   Egon Animator
   Copyright (C) 2000-2002  Ulric Eriksson <ulric@siag.nu>

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
 * fileio_html.c
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

static void save_line(FILE *fp, char *line)
{
	int c;
	char b[100];
	if (!line) return;
	while ((c = *line)) {
		MwToCchar(b, c);
		fprintf(fp, b);
		line++;
	}
}

static int save(char *fn, buffer *buf)
/* Returns: 0 if successful, otherwise 1 */
{
	FILE *fp;
	int s = 0;
	MwAniObject *o = NULL;

	if ((fp = fopen(fn, "w")) == NULL) return 1;

	/* do the header */
	fprintf(fp, "<HTML>\n<HEAD>\n<TITLE>\n");
	/* use buffer name as title */
	save_line(fp, buf->name);

	fprintf(fp, "\n</TITLE>\n</HEAD>\n\n<BODY>\n");

	for (s = 0; s < buf->nsht; s++) {
		if (s) fprintf(fp, "<BR><HR><BR>\n");
		for (o = buf->sht[s].cast; o; o = o->next) {
			if (o->type == MW_ANI_STRING) {
				save_line(fp, o->string);
				fprintf(fp, "<p>\n");
			}
		}
	}
	fprintf(fp, "</BODY>\n</HTML>\n");
	fclose(fp);
	return 0;
}


static enum {START, INATAG, INACHAR, END} state;
static int errflag;
static buffer *buf;
static char tagbuf[256];	/* for &chars; and <tags> */
static char outbuf[256];	/* for unwritten lines */
static int tbi, obi, mute, pre, sty;
static int fmt;
static unsigned int row;
static int sht;
static MwAniObject *ao;
static int y;
static int nobj;


static void emitchar(int c)
{
	char name[1024];
	MwAniScript *as;

	if (mute) return;

	if (c == '\f') {
		if (obi) emitchar('\n');	/* flush pending text */
		sht++;
		if (sht >= buf->nsht) {
			buffer_add_sheet(buf, sht);
		}
		y = 20;
	} else if (c == '\n') {	/* break the current line */
		outbuf[obi] = '\0';
		if (!only_space(outbuf)) {
			if (buf->sht[sht].cast == NULL) {
				ao = MwMalloc(sizeof *ao);
				buf->sht[sht].cast = ao;
			} else {
				ao->next = MwMalloc(sizeof *ao);
				ao = ao->next;
			}
			ao->next = NULL;
			ao->type = MW_ANI_STRING;
			ao->fmt = fmt;
			sprintf(name, "String %d", nobj++);
			ao->name = MwStrdup(name);
			ao->string = MwStrdup(outbuf);
			as = MwMalloc(sizeof *as);
			ao->script = as;
			as->next = NULL;
			as->time = 0;
			as->x = 10;
			as->y = y;
			as->width = 500;
			as->height = 20;
			as->visible = TRUE;
		}
		y += 20;
		obi = 0;
	} else {
		if (obi < 255) outbuf[obi++] = c;
	}
}


/* &#xxx; => emitchar(atoi(xxx)) */
static void emitcchar(char *p)
{
	int i;
	i = MwFromCchar(p);
	if (i != -1) emitchar(i);
}

static void tag_ignore(void)
{
	;
}

static void tag_newline(void)
{
	emitchar('\n');
}

static void tag_listitem(void)
{
	emitchar('\n');
	emitchar(' ');
	emitchar('-');
	emitchar(' ');
}

static void tag_descdesc(void)
{
	emitchar('\n');
	emitchar('\t');
}

static void tag_mute(void)
{
	mute = 1;
}

static void tag_unmute(void)
{
	mute = 0;
}

static void tag_pre(void)
{
	emitchar('\n');
	pre = 1;
}

static void tag_pre_(void)
{
	emitchar('\n');
	pre = 0;
}

static void tag_h1(void)
{
	emitchar('\n');
}

static void tag_h2(void)
{
	emitchar('\n');
}

static void tag_h3(void)
{
	emitchar('\n');
}

static void tag_h4(void)
{
	emitchar('\n');
}

static void tag_h5(void)
{
	emitchar('\n');
}

static void tag_h6(void)
{
	emitchar('\n');
}

static void tag_address(void)
{
	emitchar('\n');
}

static void tag_plain(void)
{
	emitchar('\n');
}

static void tag_i(void)
{
	;
}

static void tag_i_(void)
{
	;
}

static void tag_b(void)
{
	;
}

static void tag_b_(void)
{
	;
}

static struct {
	char *name;
	void (*action)(void);
} tag[] = {
	{"br", tag_newline},
	{"p", tag_newline},
	{"ol", tag_newline},
	{"/ol", tag_newline},
	{"ul", tag_newline},
	{"li", tag_listitem},
	{"dl", tag_newline},
	{"dt", tag_newline},
	{"dd", tag_descdesc},
	{"table", tag_newline},
	{"/table", tag_newline},
	{"tr", tag_newline},
	{"th", tag_newline},
	{"/p", tag_ignore},
	{"h1", tag_h1},
	{"/h1", tag_plain},
	{"h2", tag_h2},
	{"/h2", tag_plain},
	{"h3", tag_h3},
	{"/h3", tag_plain},
	{"h4", tag_h4},
	{"/h4", tag_plain},
	{"h5", tag_h5},
	{"/h5", tag_plain},
	{"h6", tag_h6},
	{"/h6", tag_plain},
	{"hr", tag_newline},
	{"li", tag_newline},
	{"pre", tag_pre},
	{"/pre", tag_pre_},
	{"address", tag_address},
	{"/address", tag_plain},
	{"i", tag_i},
	{"/i", tag_i_},
	{"b", tag_b},
	{"/b", tag_b_},
	{"head", tag_mute},
	{"/head", tag_unmute},
	{NULL, NULL}
};

/* any unrecognized tag is ignored */
static void emittag(char *p)
{
	int i;
	for (i = 0; p[i]; i++)
		if (isspace(p[i])) break;
	p[i] = '\0';
	for (i = 0; tag[i].name; i++)
		if (!MwStrcasecmp(p, tag[i].name)) break;
	if (tag[i].name) (*tag[i].action)();
}

static void html_char(int c)
{
	switch (state) {
	case START:
		if (isspace(c)) {
			if (pre) emitchar(c);
			else emitchar(' ');
		} else if (c == '<') {
			tbi = 0;
			state = INATAG;
		} else if (c == '&') {
			tbi = 0;
			state = INACHAR;
		} else if (c == EOF) {
			state = END;
		} else {
			emitchar(c);
		}
		break;
	case INATAG:
		if (c == '>') {
			tagbuf[tbi] = '\0';
			emittag(tagbuf);
			state = START;
		} else if (c == EOF) {
			errflag = 1;
			state = END;
		} else {
			if (tbi+10 < sizeof tagbuf)
				tagbuf[tbi++] = c;
		}
		break;
	case INACHAR:
		if (c == ';') {
			tagbuf[tbi] = '\0';
			emitcchar(tagbuf);
			state = START;
		} else if (c == EOF) {
			errflag = 1;
			state = END;
		} else {
			if (tbi+10 < sizeof tagbuf)
				tagbuf[tbi++] = c;
		}
		break;
	default:
		fprintf(stderr, "In html_char(): shouldn't be here!\n");
		errflag = 1;
		state = END;
		break;
	}
}

static int load(char *fn, buffer *b)
{
	FILE *fp;
	MwFmt ft;
	char instr[1024];
	char *p;

	fp = fopen(fn, "r");
	if (!fp) return 1;

	sht = 0;
	nobj = 0;
	y = 20;
	buf = b;
	MwDecodeFormat(0, ~0, &ft);
	ft.size = 140;
	ft.fg = "white";
	fmt = MwEncodeFormat(~0, &ft);
	buf->sht[sht].delta = 100;
	buf->sht[sht].duration = 4000;
	buf->sht[sht].now = 0;
	buf->width = 600;
	buf->height = 400;
	buf->sht[sht].cast = NULL;
	buf->state = MW_ANI_STOP;
	buf->sht[sht].bg = NULL;
	buf->change = FALSE;
	buf->sht[sht].plugin = NULL;
	buf->sht[sht].nplugin = 0;

	state = START;
	errflag = 0;
	mute = 0;
	obi = 0;
	tbi = 0;
	pre = 0;
	row = 1;
	sty = MW_STY_DEFAULT;
	/* some trickery required to figure out powerpoint slides */
	while (fgets(instr, 1024, fp)) {
		if (state == END) {
			break;
		} else if (!MwStrcasecmp(instr, "<BR><HR><BR>\n")) {
			/* pptHtml uses this as page separator */
			emitchar('\f');
		} else {
			for (p = instr; *p; p++)
				html_char(*p);
		}
	}

	buf->change = 0;
	fclose(fp);
	return 0;
}

/* ---
format guessing:
   1. extension .htm or .html
   2. not empty
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp = NULL;
	char b[250];
	int result;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrncasecmp(ext, ".htm", 4) &&
		(fp = fopen(fn, "r")) &&
		fgets(b, sizeof b, fp));
	if (fp) fclose(fp);
	return result;
}

/* ---
*/
void fileio_html_init(void)
{
	register_format(load, save, myformat,
			"Hypertext Markup Language (*.html)");
}

