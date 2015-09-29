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

/*
 * fileio.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../siod/siod.h"
#include "egon.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "user_interface.h"

#define MAX_FORMATS (20)

static int format_count = 0;
static int nloader = 0;
static int nsaver = 0;
static char *fmt_name;

struct {
	int (*load)(char *, buffer *);
	int (*save)(char *, buffer *);
	int (*myformat)(char *);
	char *pattern;
} fileformats[MAX_FORMATS];

/* These are not declared in egon.h because they will only be called here */
extern void fileio_egon_init(void);
extern void fileio_ppt_init(void);
extern void fileio_c_init(void);
extern void fileio_gif_init(void);
extern void fileio_ps_init(void);
extern void fileio_scm_init(void);
extern void fileio_mgp_init(void);
extern void fileio_txt_init(void);
extern void fileio_ps_init(void);
extern void fileio_html_init(void);
extern void fileio_extconv_init(void);

static int siag_fmt;

static int fmt_index(char *fmt)
{
	int i;

	if (fmt == NULL) return siag_fmt;

	for (i = 0; i < format_count; i++)
		if (!strcmp(_(fileformats[i].pattern), fmt)) return i;
	return -1;
}

static int save_all(char *fn, buffer *buf)
{
	char *fmt = guess_file_format(fn);
	int n = fmt_index(fmt);
	if (fmt && n >= 0 && fileformats[n].save) {
		return savematrix(fn, buf, fmt);
	}
	n = select_from_list("Which format?", saver_patterns, nsaver);
	if (n < 0) return -1;
	return savematrix(fn, buf, saver_patterns[n]);
}

static int load_all(char *fn, buffer *buf)
{
	char *fmt = guess_file_format(fn);
	int n = fmt_index(fmt);
	if (fmt && n >= 0 && fileformats[n].load) {
		return loadmatrix(fn, buf, fmt);
	}
	n = select_from_list("Which format?", loader_patterns, nloader);
	if (n < 0) return -1;
	return loadmatrix(fn, buf, loader_patterns[n]);
}

static int myformat(char *fn)
{
	return 0;	/* this doesn't handle any format at all */
}

/* Need this in a separate structure for fsel_input */
char *loader_patterns[MAX_FORMATS];
char *saver_patterns[MAX_FORMATS];

/* ---
Register a file format handler.
*/

void register_format(int (*load)(char *, buffer *),
		int (*save)(char *, buffer *),
		int (*myformat)(char *), char *p)
{
	char *pattern = MwStrdup(p);
	fileformats[format_count].load = load;
	fileformats[format_count].save = save;
	fileformats[format_count].myformat = myformat;
	fileformats[format_count].pattern = pattern;
	format_count++;
	if (load) {
		loader_patterns[nloader++] = pattern;
		loader_patterns[nloader] = NULL;
	}
	if (save) {
		saver_patterns[nsaver++] = pattern;
		saver_patterns[nsaver] = NULL;
	}
}

/* ---
*/
void fileio_init(void)
{
	/* Register Egon format first */
	siag_fmt = format_count;
	fileio_egon_init();
	fileio_ppt_init();
	fileio_mgp_init();
	fileio_txt_init();
	fileio_html_init();
	fileio_c_init();
	fileio_gif_init();
	fileio_ps_init();
	fileio_scm_init();
	fileio_extconv_init();

	/* Put this catch-all last */
	register_format(load_all, save_all, myformat, "All files (*)");
}

/* ---
Save a file of unknown format.
*/

int savematrix(char *fn, buffer *buf, char *format)
{
	int (*fun)(char *, buffer *);
	int i;

	fmt_name = format;

	if (format) {
		i = fmt_index(format);
		if (i < 0) return 1;
	} else i = format_count-1;	/* Ask for format */

	fun = fileformats[i].save;
	if (fun) return fun(fn, buf);
	else return 1;
}

/* ---
Load a file of unknown format.
*/

int loadmatrix(char *fn, buffer *buf, char *format)
{
	int (*fun)(char *, buffer *);
	int i;

	fmt_name = format;

	if (format) {
		i = fmt_index(format);
		if (i < 0) return 1;
	} else i = format_count-1;	/* Ask for format */

	fun = fileformats[i].load;
	if (fun) return fun(fn, buf);
	else return 1;
}

/* ---
Guess the format of a file.
*/

char *guess_file_format(char *fn)
{
        int i;
        int (*my_format)(char *);

        for (i = 0; i < format_count; i++) {
                my_format = fileformats[i].myformat;
                if (my_format && my_format(fn))
                        return fileformats[i].pattern;
        }
        return NULL;
}

void get_format_handlers(char *ext, int *loader, int *saver)
{
	int i;
	char e[1024];

	sprintf(e, "(%s)", ext);
	*loader = *saver = -1;

	for (i = 0; i < nloader; i++) {
		if (strstr(loader_patterns[i], e)) {
			*loader = fmt_index(loader_patterns[i]);
			break;
		}
	}
	for (i = 0; i < nsaver; i++) {
		if (strstr(saver_patterns[i], e)) {
			*saver = fmt_index(saver_patterns[i]);
			break;
		}
	}
}

static struct {
	char *name;
	char *lext;
	char *lcmd;
	char *sext;
	char *scmd;
} *conv = NULL;
static int nconv = 0;

static char *tempname;
static char *maketempname(void)
{
	char p[1024];
	if (tempname == NULL) {
		sprintf(p, "%s/conv_ext.tmp", siag_tmpdir);
		tempname = MwStrdup(p);
	}
	return tempname;
}

#define TMPNAME (maketempname())

#define BLACKLIST "\\\"\'$?*|&;()<>[]{}! \t\n\r"

static int save_ext(char *fn, buffer *buf)
{
	int loader, saver;
	int i, n;
	char cmd[1024];
	char qn[1024];

	for (i = 0; i < nconv; i++) {
		if (!strcmp(fmt_name, conv[i].name)) break;
	}
	if (i == nconv) return 1;
	get_format_handlers(conv[i].sext, &loader, &saver);
	if (saver < 0) return 1;
	n = savematrix(TMPNAME, buf, fileformats[saver].pattern);
	MwQuotecpy(qn, fn, BLACKLIST);
	sprintf(cmd, conv[i].scmd, TMPNAME, qn);
	system(cmd);
	remove(TMPNAME);
	return n;
}

static int load_ext(char *fn, buffer *buf)
{
	int loader, saver;
	int i, n;
	char cmd[1024];
	char qn[1024];

	for (i = 0; i < nconv; i++) {
		if (!strcmp(fmt_name, conv[i].name)) break;
	}
	if (i == nconv) return 1;
	get_format_handlers(conv[i].lext, &loader, &saver);
	if (loader < 0) return 1;
	MwQuotecpy(qn, fn ,BLACKLIST);
	sprintf(cmd, conv[i].lcmd, qn, TMPNAME);
	system(cmd);
	n = loadmatrix(TMPNAME, buf, fileformats[loader].pattern);
	remove(TMPNAME);
	return n;
}

static int myformat_ext(char *fn)
{
	return 0;
}

static void register_converter(char *fmt, char *lext, char *lcmd,
				char *sext, char *scmd)
{
	int (*saver)(char *, buffer *);
	int (*loader)(char *, buffer *);

	conv = MwRealloc(conv, (nconv+1)*sizeof *conv);
	conv[nconv].name = MwStrdup(fmt);
	if (lext) {
		loader = load_ext;
		conv[nconv].lext = MwStrdup(lext);
		conv[nconv].lcmd = MwStrdup(lcmd);
	} else {
		loader = NULL;
		conv[nconv].lext = conv[nconv].lcmd = NULL;
	}
	if (sext) {
		saver = save_ext;
		conv[nconv].sext = MwStrdup(sext);
		conv[nconv].scmd = MwStrdup(scmd);
	} else {
		saver = NULL;
		conv[nconv].sext = conv[nconv].scmd = NULL;
	}
	nconv++;
	register_format(loader, saver, myformat_ext, fmt);
}

static LISP lregister_converter(LISP fmt, LISP lext, LISP lcmd,
				LISP sext, LISP scmd)
{
	register_converter(get_c_string(fmt),
			NULLP(lext)?NULL:get_c_string(lext),
			NULLP(lcmd)?NULL:get_c_string(lcmd),
			NULLP(sext)?NULL:get_c_string(sext),
			NULLP(scmd)?NULL:get_c_string(scmd));
	return NIL;
}

void fileio_extconv_init(void)
{
	init_subr_5("register-converter", lregister_converter);
	execute("(register-converters)");
}

