/*
   fileio_ppt.c - Read a PowerPoint presentation into Egon Animator.
   Copyright 2002  Ulric Eriksson <ulric@siag.nu>

   Based on:
   pptHtml - Format a PowerPoint Presentation into Html
   Copyright 1999  Steve Grubb  <linux_4ever@yahoo.com>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published  by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

#include "config.h"			/* Created by ./configure script */
#include "../cole/support.h"		/* Needs to be before internal.h */
#include "../cole/internal.h"		/* Needs to be before cole */
#include "../cole/cole.h"

#include <stdio.h>
#include <string.h>	/* for strcpy() */
#include <ctype.h>	/* For isprint */
#include <stdlib.h> /* For exitt() */
#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

static enum {START, INATAG, INACHAR, END} state;
static int errflag;
static buffer *buf;
static char outbuf[256];
static int tbi, obi, mute, pre, sty;
static int fmt;
static unsigned int row;
static int sht;
static MwAniObject *ao;
static int y;
static int nobj;

#define STDSIZE (140)

static char FileName[2][32] =			/* The section of the PowerPoint File we read */
{
	"/PowerPoint Document",	/* Power Point 97 & 2000 */
	"/PP40"				/* Everything else ? */
};


/* Function Prototypes */
static void print_unicode(unsigned char *, int);

/* Global data */
static char filename[128];
static int output_this_container = 0;
static int past_first_slide = 0;
static int last_container = 0;

/* Go over the objects in a sheet and remember the widest one.
   Adjust font size to make sure that everything will fit.
   This is better than the code currently used in fileio_txt.c.
*/
static void fixup_sizes(MwAniObject *o, int w, int h)
{
	MwAniObject *p;
	MwAniScript *s;
	float f, g;
	MwFmt fmt;
	int x, y, n;
	int mx = 0, my = 0;
	MwRichchar *r;

	/* We know that the existing strings have size 140 decipoints.
	We also know that they are 20 pixels apart. Calculate a suitable
	scaling factor by dividing h (window height in pixels) with
	y (height we have used). */
	for (p = o; p; p = p->next) {
		if (p->type != MW_ANI_STRING) continue;
		n = strlen(p->string);
		r = MwRcMakerich(p->string, p->fmt);
		x = 10+MwRcStrwidth(r, n);
		MwFree(r);
		if (x > mx) mx = x;
		s = p->script;
		y = s->y+10;
		if (y > my) my = y;
	}
	f = (float)h/my;
	g = (float)w/mx;
	if (g < f) f = g;
	my = 0;
	for (p = o; p; p = p->next) {
		if (p->type != MW_ANI_STRING) continue;
		MwDecodeFormat(p->fmt, ~0, &fmt);
		fmt.size *= f;
		p->fmt = MwEncodeFormat(~0, &fmt);
		s = p->script;  /* we know that there is only one tick */
		s->y *= f;
		if (s->y > my) my = s->y;
	}
	/* at this point, we have used no more than my pixels */
	/* if there is space at the bottom, move some up */
	y = (h-my)/2-10;
	if (y > 0) {
		for (p = o; p; p = p->next) {
			s = p->script;
			s->y += y;
		}
	}
}

static void emitchar(int c)
{
	char name[1024];
	MwAniScript *as;
       
	if (mute) return;
       
	if (c == '\f') {

/* hack to prevent empty pages */
if (buf->sht[sht].cast == NULL) return;

		if (obi) emitchar('\n');	/* flush pending text */
		sht++;
		if (sht >= buf->nsht) {
			buffer_add_sheet(buf, sht);
		}
		y = 20;
	} else if (c == '\n') { /* break the current line */
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
		if (obi > 60) {
			int i;
			char b[1024];
			/* try to split line */
			for (i = 60; i > 0 && outbuf[i] != ' '; i--);
			if (i) {
				strcpy(b, outbuf+i);
				outbuf[i] = '\0';
				emitchar('\n');
				for (i = 0; b[i]; i++) emitchar(b[i]);
			}
		}
		if (obi < 255) outbuf[obi++] = c;
	}
}


static void container_processor(int type)
{
/*printf("container_processor(%04x)\n", type);
*/
	if (type == 0x03EE) {
		if (past_first_slide)
			emitchar('\f');
		else
			past_first_slide = 1;
	}
	switch (type) {
	case 0x000D:
		if (last_container == 0x11)		/* suppress notes info */
			output_this_container = 0;
		else
			output_this_container = 1;
		break;
	case 0x0FF0:
		output_this_container = 1;
		break;
	default:
/*		printf("Cont:%x|\n", type);	*/
		output_this_container = 0;
		break;
	}
	last_container = type;
}

static void atom_processor(int type, int count, int buf_last, unsigned char *working_buffer, int buf_idx)
{
	int i;
/*printf("atom_processor(%04x, %d, %d, %d)\n", type, count, buf_last, buf_idx);
*/
	if (output_this_container == 0) return;

	switch (type) {
	case 0x0FA0:	/* Text String in unicode */
/*printf("text string in unicode\n");
*/
		print_unicode(working_buffer, buf_idx);
		emitchar('\n');
		break;
	case 0x0FA8:	/* Text String in ASCII */
		for (i=0;i<buf_idx; i++) {
			if (working_buffer[i] == 0x0D) {
				emitchar('\n');
			} else {
				emitchar(working_buffer[i]);
			}
		}
		emitchar('\n');
		break;
	case 0x0FBA:	/* CString - unicode... */
/*printf("cstring - unicode\n");
*/
		print_unicode(working_buffer, buf_idx);
		emitchar('\n');
		break;
	case 0x03F3:	/* new sheet? */
		emitchar('\f');
		break;
	default:
/*printf("none of the above\n");
*/
		break;
	}
}

/* I must admit that I don't fully understand what this function does...

   It clears all but the 6 LSB, then sets the MSB. Ho hum.
*/
static void put_utf8(unsigned short c)
{
	emitchar(0x0080 | ((short)c & 0x003F));
}

static void OutputCharCorrected(unsigned char c)
{
	switch (c) {	/* Special char handlers here... */
	case '\r':
		emitchar('\n');
		break;
	case 0x3C:
		emitchar('<');
		break;
	case 0x3E:
		emitchar('>');
		break;
	case 0x26:
		emitchar('&');
		break;
	case 0x22:
		emitchar('\'');
		break;
#if 0
	/* Also need to cover 128-159 since MS uses this area... */
	case 0x80:		/* Euro Symbol */
		html_string("&#8364;");
		break;
	case 0x82:		/* baseline single quote */
		html_string("&#8218;");
		break;
	case 0x83:		/* florin */
		html_string("&#402;");
		break;
	case 0x84:		/* baseline double quote */
		html_string("&#8222;");
		break;
	case 0x85:		/* ellipsis */
		html_string("&#8230;");
		break;
	case 0x86:		/* dagger */
		html_string("&#8224;");
		break;
	case 0x87:		/* double dagger */
		html_string("&#8225;");
		break;
	case 0x88:		/* circumflex accent */
		html_string("&#710;");
		break;
	case 0x89:		/* permile */
		html_string("&#8240;");
		break;
	case 0x8A:		/* S Hacek */
		html_string("&#352;");
		break;
	case 0x8B:		/* left single guillemet */
		html_string("&#8249;");
		break;
	case 0x8C:		/* OE ligature */
		html_string("&#338;");
		break;
	case 0x8E:		/*  #LATIN CAPITAL LETTER Z WITH CARON */
		html_string("&#381;");
		break;
	case 0x91:		/* left single quote ? */
		html_string("&#8216;");
		break;
	case 0x92:		/* right single quote ? */
		html_string("&#8217;");
		break;
	case 0x93:		/* left double quote */
		html_string("&#8220;");
		break;
	case 0x94:		/* right double quote */
		html_string("&#8221;");
		break;
	case 0x95:		/* bullet */
		html_string("&#8226;");
		break;
	case 0x96:		/* endash */
		html_string("&#8211;");
		break;
	case 0x97:		/* emdash */
		html_string("&#8212;");
		break;
	case 0x98:		/* tilde accent */
		html_string("&#732;");
		break;
	case 0x99:		/* trademark ligature */
		html_string("&#8482;");
		break;
	case 0x9A:		/* s Haceks Hacek */
		html_string("&#353;");
		break;
	case 0x9B:		/* right single guillemet */
		html_string("&#8250;");
		break;
	case 0x9C:		/* oe ligature */
		html_string("&#339;");
		break;
	case 0x9F:		/* Y Dieresis */
		html_string("&#376;");
		break;
#endif
	default:
		emitchar(c);
		break;
	}
}

/* What? This looks really weird, but that's just because I don't get it */
static void print_utf8(unsigned short c)
{
	if (c == 0) return;
		
	if (c < 0x80) OutputCharCorrected(c);
	else if (c < 0x800) {
		emitchar(0xC0 | (c >>  6));
		put_utf8(c);
	} else {
		emitchar(0xE0 | (c >> 12));
		put_utf8(c >>  6);
		put_utf8(c);
	}
}

static void print_unicode(unsigned char *ucs, int len)
{
	int i;
	for (i = 0; i < len; i += 2)
		print_utf8(ucs[i] | (ucs[i+1] << 8));
}

static void dump_file(COLEDIRENT *cde, void *_info)
{
	unsigned long version=0, instance=0, type=0, length=0;
	unsigned char buf[16];
	COLEFILE *cf;
	COLERRNO err;

	cf = cole_fopen_direntry(cde, &err);

/* Ouput Header */

/*
This code apparently reads the file one byte at a time.

The file seems to be organized in blocks consisting of an 8 byte header
followed by a variable amount of data.

It would seem somewhat more logical to read the whole buffer at once,
and then read all the associated data in a single call.

The header is:

 - two bytes instance & version
 - two bytes type
 - four bytes record length

*/

/* Output body */
	while (cole_fread(cf, buf, 8, &err)) {
		instance = buf[0] | (buf[1]<<8);
		version = instance & 0x000FL;
		instance >>= 4;
		type = buf[2] | ((buf[3]<<8) & 0x0FFFL);
		length = buf[4] | (buf[5]<<8) | (buf[6]<<16) | (buf[7]<<24);

/*printf("Record instance=%ld, version=%ld, type=%ld, length=%ld\n", instance, version, type, length);
*/

		if (version == 0x0F) {
			container_processor(type);
		} else {
			unsigned char *b = MwMalloc(length);
			if (cole_fread(cf, b, length, &err)) {
				atom_processor(type, length, length, b, length);
			}
			MwFree(b);
		}
	}

	if (past_first_slide)
		emitchar('\f');

	cole_fclose(cf, &err);
}

static int load(char *fn, buffer *b)
{
	int i;
	MwFmt ft;
	int f_ptr = 0;
	COLEFS *cfs;
	COLERRNO colerrno;

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
	buf->width = 700;
	buf->height = 500;
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

	strncpy(filename, fn, 1000);
	cfs = cole_mount(filename, &colerrno);
	if (cfs == NULL) {
		cole_perror("Egon", colerrno);
		return 1;
	}
	while (cole_locate_filename(cfs, FileName[f_ptr], NULL, dump_file, &colerrno)) {
		if (f_ptr) {
			cole_perror("Egon", colerrno);
			if (colerrno == COLE_EFILENOTFOUND)
				fprintf(stderr, "Section: PowerPoint Document\n");
			break;
		} else {
			f_ptr++;
		}
	}
	cole_umount(cfs, &colerrno);
	for (i = 0; i < buf->nsht; i++) {
		fixup_sizes(buf->sht[i].cast, buf->width, buf->height);
	}
	return 0;
}

/* Extension .ppt, must exist and be mountable by libcole */
static int myformat(char *fn)
{
	char *ext;
	int result;
	COLEFS *cfs = NULL;
	COLERRNO colerrno;

	result = ((ext = strrchr(fn, '.')) &&
		!MwStrcasecmp(ext, ".ppt") &&
		(cfs = cole_mount(fn, &colerrno)));
	if (cfs) cole_umount(cfs, &colerrno);
	return result;
}

void fileio_ppt_init(void)
{
	register_format(load, NULL, myformat, "PowerPoint Document (*.ppt)");
}

