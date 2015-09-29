/*
   Pathetic Writer
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

/* ---
fileio_pw.c

970706 File format snatched from Siag
970707 New tag .sty to distinguish Pw styles from Siag styles
970714 New tag .ad for adjustment. But this sucks!
980720 .plugin added, structured file format
981020 New font code. See xcommon/fonts.c
981113 Multiple sheets.
990422 Margins, paper type, size and orientation, header and footer,
	first page number, tab distance
990518 .bop sets a bop on next line

Brief description of the file format:

#comment                     	A comment; ignored
.sw width                    	Standard column width
.sh height                   	Standard row height
.sf format                   	Standard format code
.cw col width                   Column width in pixels
.rh row height                  Column height in pixels
.st row style			Style
.ad				Adjust
.fmtN				Format N
.styN				Style N
row col format       #          Empty cell
row col format       "label     Cell containing a label

Leading or trailing white space is not allowed
Empty lines are ignored
All widths and heights are in pixels
Format codes are in decimal

This is the same format as Siag with only labels and with cells
replaced by segments.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include "../common/plugin.h"

#include "pw.h"

int make_backups = 1;

/* ---
returns the name of a component in a structured file. Caller must free
*/

static char *file_name(buffer *buf, char *n)
{
        char b[1024];
        plugin_basedir(b, buf->name);
        strcat(b, "/");
        strcat(b, n);
        return MwStrdup(b);
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static MwRichchar empty_line[] = {{0, 0}};

static int save_flat(char *fn, buffer * buf)
{
	int i, lr;
	int s;
	FILE *fp;
	int sh = style_height(MW_STY_DEFAULT);
	int sf = style_table[MW_STY_DEFAULT].format;
	char *used_fmt = MwCalloc(MwFormatCount+1, 1);
	char *used_style = MwCalloc(nstyle+1, 1);
	int f, oldfmt;
	MwFmt fmt;

	if ((fp = fopen(fn, "w")) == (FILE *) 0)
		return 1;

	/* start by saving standard values */
	fprintf(fp, "# Creator: %s\n", version);
	fprintf(fp, ".sh %d\n", sh);
	fprintf(fp, ".sf %d\n", sf);

	fprintf(fp, ".margins %d %d %d %d %d %d\n",
		buf->top_margin, buf->bottom_margin,
		buf->left_margin, buf->right_margin,
		buf->header_margin, buf->footer_margin);
	fprintf(fp, ".paper %s %d %d %d\n",
		buf->paper_name, buf->paper_width,
		buf->paper_height, buf->orientation);
	fprintf(fp, ".header %s\n", buf->header);
	fprintf(fp, ".footer %s\n", buf->footer);
	fprintf(fp, ".header_on_first %d\n", buf->header_on_first);
	fprintf(fp, ".first_page %d\n", buf->first_page_number);

	for (s = 0; s < buf->nsht; s++) {
		fprintf(fp, ".sheet %d %s\n", s, buf->sht[s].name);
		lr = line_last_used(buf, s);
		fprintf(fp, "# %s\n# %d lines\n#\n", fn, lr);
		fprintf(fp, ".tabs %s\n", buf->sht[s].tabs);
	
		for (i = 1; i <= lr; i++) {
			int height = cell_height(buf, s, i);
			if (height != sh)
				fprintf(fp, ".rh %d %d\n", i, height);
			if (buf->sht[s].text[i].sty != MW_STY_DEFAULT)
				fprintf(fp, ".st %d %d\n",
					i, buf->sht[s].text[i].sty);
			if (buf->sht[s].text[i].adj != MW_HADJ_LEFT)
				fprintf(fp, ".ad %d %d\n",
					i, buf->sht[s].text[i].adj);
		}
		for (i = 0; i < buf->sht[s].nplugin; i++) {
			int width, height;
			int n = plugin_size_get(buf->sht[s].plugin[i].ph,
					&width, &height);
			if (n != 0) continue;
			fprintf(fp, ".plugin %d %d %s\n",
				buf->sht[s].plugin[i].row,
				buf->sht[s].plugin[i].col,
				buf->sht[s].plugin[i].name);
			fprintf(fp, ".plugin-geometry %d %d\n",
				width, height);
		}

		for (i = 1; i <= lr; i++) {
			int j = 0, col = 1;
			MwRichchar *line = buf->sht[s].text[i].p;
			if (ret_bop(buf, s, i))
				fprintf(fp, ".bop\n");
			if (MwRcStrlen(line) == 0) line = empty_line;
			fprintf(fp, "# Line %d", i);
	
			oldfmt = -1;
			while (line[j].c) {
				f = line[j].fmt;
				if (f != oldfmt) {
					MwDecodeFormat(f, ~0, &fmt);
					putc('\n', fp);
					if (!used_style[fmt.style]) {
						save_styles(fp, fmt.style);
						used_style[fmt.style] = 1;
					}
					if (!used_fmt[f]) {
						MwSaveFormats(fp, f);
						used_fmt[f] = 1;
					}
					fprintf(fp, "%d %d %d\t\"",
						i, col++, f);
				}
				oldfmt = f;
				putc(line[j++].c, fp);
			}
			putc('\n', fp);
		}
	}
	fprintf(fp, "# End of file %s\n", fn);
	fclose(fp);
	MwFree(used_fmt);
	MwFree(used_style);
	return 0;
}				/* save_flat */

/* ---
Returns: 0 if successful, otherwise 1
*/

static int load_flat(char *fn, buffer * buf)
{
	int i, j;
	long f;
	FILE *fp;
	char instring[256], *p, *contents;
	int sf = style_table[MW_STY_DEFAULT].format;
	int *fmts = NULL;
	int nfmt = -1;
	int *styles = NULL;
	int nsty = 0;
	int s = 0;
	int lastph = -1;
	int forcebop = 1;	/* no bop set; must be old file */
	int willbop = 0;	/* set bop on next line */

	if ((fp = fopen(fn, "r")) == NULL)
		return 1;

	s = 0;

	while (fgets(instring, 250, fp) != NULL) {
		char b[1024];
		b[0] = '\0';
		switch (instring[0]) {
		case '\0':
		case '#':
			break;
		case '.':
			if (!strncmp(instring, ".margins ", 9)) {
				sscanf(instring, ".margins %d %d %d %d %d %d",
				       &(buf->top_margin),
				       &(buf->bottom_margin),
				       &(buf->left_margin),
				       &(buf->right_margin),
				       &(buf->header_margin),
				       &(buf->footer_margin));
			} else if (!strncmp(instring, ".paper ", 7)) {
				sscanf(instring, ".paper %s %d %d %d",
				       b,
				       &(buf->paper_width),
				       &(buf->paper_height),
				       &(buf->orientation));
			   	buf->paper_name = MwStrdup(b);
			} else if (!strncmp(instring, ".header ", 8)) {
			   	sscanf(instring, ".header %[^\n]", b);
				buf->header = MwStrdup(b);
			} else if (!strncmp(instring, ".footer ", 8)) {
				sscanf(instring, ".footer %[^\n]", b);
				buf->footer = MwStrdup(b);
			} else if (!strncmp(instring, ".header_on_first ", 17)) {
				sscanf(instring, ".header_on_first %d",
				       &(buf->header_on_first));
			} else if (!strncmp(instring, ".first_page ", 12)) {
				sscanf(instring, ".first_page %d",
				       &(buf->first_page_number));
			} else if (!strncmp(instring, ".tabs ", 6)) {
				sscanf(instring, ".tabs %[^\n]", b);
				buf->sht[s].tabs = MwStrdup(b);
			} else if (!strcmp(instring, ".bop\n")) {
				willbop = 1;
				forcebop = 0;
			} else if (!strncmp(instring, ".sf", 3)) {
				sscanf(instring, ".sf %d", &sf);
			} else if (!strncmp(instring, ".rh", 3)) {
				int h;
				sscanf(instring, ".rh %d %d", &i, &h);
				alloc_line(buf, s, i);
				buf->sht[s].text[i].height = h;
			} else if (!strncmp(instring, ".ft ", 4)) {
				int n;
				MwFmt fmt;
				sscanf(instring, ".ft%d", &n);
				if (n > nfmt) {
					nfmt = n;
					fmts = MwRealloc(fmts,
							(nfmt+1)*sizeof *fmts);
				}
				fmts[n] = MwLoadFormats(fp);
				/* now replace style with the real one */
				MwDecodeFormat(fmts[n], ~0, &fmt);
				/* look out for bogus styles */
				if (fmt.style < nsty
					&& styles[fmt.style] < nstyle) {
					fmt.style = styles[fmt.style];
					fmts[n] = MwEncodeFormat(~0, &fmt);
				} else {
					fmt.style = 0;
				}
			} else if (!strncmp(instring, ".style ", 7)) {
				int n;
				sscanf(instring, ".style %d", &n);
				if (n >= nsty) {
					nsty = n;
					styles = MwRealloc(styles,
							(nsty+1)*sizeof *styles);
				}
				styles[n] = load_styles(fp);
			} else if (!strncmp(instring, ".sheet ", 7)) {
				char name[1000];
				sscanf(instring, ".sheet %d %[^\n]", &s, name);
				if (s >= buf->nsht)
					buffer_add_sheet(buf, s);
				buffer_rename_sheet(buf, s, name);
			} else if (!strncmp(instring, ".st ", 4)) {
				int sty;
				sscanf(instring, ".st %d %d", &i, &sty);
				alloc_line(buf, s, i);
				buf->sht[s].text[i].sty = sty;
			} else if (!strncmp(instring, ".ad ", 4)) {
				int adj;
				sscanf(instring, ".ad %d %d", &i, &adj);
				alloc_line(buf, s, i);
				buf->sht[s].text[i].adj = adj;
			} else if (!strncmp(instring, ".sty", 4)) {
				int h;
				char b[256];
				sscanf(instring, ".sty%d %[^\n]", &h, b);
				h >>= MW_FMT_SHIFT;
			} else if (!strncmp(instring, ".plugin-geometry ", 17)) {
				int n, width, height;
				n = sscanf(instring, ".plugin-geometry %d %d",
						&width, &height);
				if (lastph >= 0 && n >= 2) {
					plugin_size_set(lastph,
						width, height);
				}
			} else if (!strncmp(instring, ".plugin", 7)) {
				int n;
				int row, col;
				char name[1024];
				plugin_t plugin;
				sscanf(instring, ".plugin %d %d %s",
					&row, &col, name);
				plugin.row = row;
				plugin.col = col;
				plugin.name = MwStrdup(name);
				p = file_name(buf, name);
				lastph = plugin.ph = plugin_start(p);
				MwFree(p);
				plugin.displayed = 0;
				if (plugin.ph != -1) {
					n = buf->sht[s].nplugin++;
					buf->sht[s].plugin = MwRealloc(buf->sht[s].plugin,
                                                buf->sht[s].nplugin*sizeof(plugin_t));
                                        buf->sht[s].plugin[n] = plugin;
				}
			}
			break;
		default:	/* cell definition */
			i = j = 0;
			i = strtol(instring, &p, 10);
			if (i < 1 || i > 1000000) break;
			j = strtol(p, &p, 10);
			if (j < 1 || j > 1000000) break;
			if (j == 1) {	/* first cell of line */
				set_bop(buf, s, i, willbop||forcebop);
				willbop = 0;
			}
			f = strtol(p, &p, 10);
			if (nfmt == -1 || f > nfmt) {	/* old */
				f = MwFmtOldToNew(f);
			} else {
				f = fmts[f];
			}
			if (p[0] != '\t' || p[1] != '"') break;
			contents = p+2;
			if ((p = strchr(contents, '\n'))) *p = '\0';
			ins_text(buf, s,
				make_position(i, line_length(buf, s, i)),
				(unsigned char *)contents, f);
			break;
		}
	}

	/* For old documents, try to guess where the bops should be.
	   Criteria: two lines with the same style, where both lines
	   contain at least one character and the first character
	   of the second line is not white space.
	*/
	if (forcebop) {
		for (s = 0; s < buf->nsht; s++) {
			int r;
			for (r = 2; r < line_last_used(buf, s); r++) {
				if ((ret_style(buf, s, r)
				      == ret_style(buf, s, r-1))
				    && (line_length(buf, s, r) > 0)
				    && (line_length(buf, s, r-1) > 0)
				    && (!isspace(peek_char(buf, s, r, 0)))) {
					set_bop(buf, s, r, 0);
				}
			}
		}
	}

	fclose(fp);
	return 0;
} /* load_flat */

#define PW_MAGIC "# Creator: Pathetic Writer"

static int flatfile(char *fn)
{
	char b[100];
	FILE *fp = fopen(fn, "r");
	if (!fp) return 0;
	if (fgets(b, sizeof b, fp) &&
			!strncmp(b, PW_MAGIC, strlen(PW_MAGIC))) {
		fclose(fp);
		return 1;
	}
	return 0;
}

static int save(char *fn, buffer *buf)
{
        char cmd[1024], bak[1024];
        char *dir, *b, *p;
        int i, result;
	int has_plugins = 0;
	int s;

	if (make_backups) {
		/* make backup copy */
		sprintf(bak, "%s.BAK", fn);
		rename(fn, bak);
	}

	for (s = 0; s < buf->nsht; s++) {
	        if (buf->sht[s].nplugin) {
			has_plugins = 1;
			break;
	        }
	}
        if (!has_plugins)	/* no plugins, save as usual */
	        return save_flat(fn, buf);

        /* save all the plugins */
        dir = file_name(buf, "");
        p = strrchr(dir, '/');
        if (p) *p = '\0';
        mkdir(dir, 0700);
	for (s = 0; s < buf->nsht; s++) {
	        for (i = 0; i < buf->sht[s].nplugin; i++) {
	                b = file_name(buf, buf->sht[s].plugin[i].name);
	                plugin_save(buf->sht[s].plugin[i].ph, b);
	                MwFree(b);
	        }
	}
        /* save the main file */
        b = file_name(buf, "INDEX.pw");
        result = save_flat(b, buf);
        MwFree(b);

        /* tar up the lot */
        sprintf(cmd, "(cd %s;tar cf - *)>%s", dir, fn);
        system(cmd);
        MwFree(dir);
        return result;
}

/* ---
this is flawed: the archive isn't untarred in the right directory,
   so a single Siag process can hose itself by unpacking archives on
   top of each other. Furthermore, the file is loaded under the name
   INDEX.pw, which will get stored into the buffer name. FIX IT!

old files don't have the MAGIC but we still want to load them.
So first try flat, then tar, then flat again.
*/

static int load(char *fn, buffer *buf)
{
        char b[1024], cmd[1024];
        char *p;
        int n;

        if (flatfile(fn) || !tryuntar(fn, "INDEX.pw"))
                return load_flat(fn, buf);

        p = plugin_basedir(b, buf->name);
        sprintf(cmd,
		"mkdir -p %s;"
		"cat %s|(cd %s;tar xf -)",
		p, fn, p);
        system(cmd);
        p = file_name(buf, "INDEX.pw");
        n = load_flat(p, buf);
        MwFree(p);
        return n;
}
 

/* ---
Conservative file format guessing:
   1. extension ".pw"
   2. starts with the string "# Creator: Pathetic Writer"
*/

static int myformat(char *fn)
{
        char *ext;
        FILE *fp;
        char b[100];

        ext = strrchr(fn, '.');
        if (!ext) return 0;     /* no extension */
        if (MwStrcasecmp(ext, ".pw"))
                return 0;       /* wrong extension */
	if (flatfile(fn))
		return 1;	/* old format */
        if ((fp = fopen(fn, "r")) == NULL)
                return 1;       /* new file */
        if (fgets(b, sizeof b, fp) &&
			!strncmp(b, PW_MAGIC, strlen(PW_MAGIC))) {
                fclose(fp);
                return 1;
        }
        fclose(fp);
	if (tryuntar(fn, "INDEX.pw")) return 1;
        return 0;
}

/* ---
*/

void fileio_pw_init(void)
{
	register_format(load, save, myformat, "Pathetic Writer (*.pw)");
}

