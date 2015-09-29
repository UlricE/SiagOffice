/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2002  Ulric Eriksson <ulric@siag.nu>

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
 fileio_siag.c
 960209 New file format
 970206 Added string support
 970215 Broken out in a separate module
 970223 Added support for different numeric styles
 970630 Support for different interpreters
 970701 Removed explicit string format (now same as expression)
 970719 New tag m for embedding
 980717 Structured file format
 981008 New font and format code
 981028 Multiple sheets
 990422 Make backups
 990510	Margins, paper type, size, orientation, header, footer,
	first page number

   Brief description of the file format:

   #comment                     	A comment; ignored
   .sw width                    	Standard column width
   .sh height                   	Standard row height
   .nsf format                   	Standard format code
   .cw col width                	Column width in pixels
   .rh row height               	Column height in pixels
   .fmtN				Format N
   .pN key				Property: length N bytes,
						value on following lines
   .ft					Format definition follows
   .sheet N				New sheet follows
   row col format       #               Empty cell
   row col format       "label          Cell containing a label
   row col format       =expression     Cell containing a SIOD expression
   row col format	==expression	Cell containing a C expression
   row col format	+int,expr	Expression with named interpreter
   row col format	$expression	String expression
   row col format	mObject		Embedded object

   Leading or trailing white space is not allowed
   Empty lines are ignored
   All widths and heights are in pixels
   Format codes are in decimal


	980717: A structured file format!

	The file name will still end in .siag, so some magic is
	necessary to identify it. First fileio_siag will test if the file
	starts with "Created by..." as usual. If this succeeds, it is
	a flat file, i.e. of the old format. If it fails, an attempt
	is made to untar the file in $HOME/.siag/$PID/filename.
	If this fails, fileio_siag again tries it as a flat file (really
	old versions of Siag didn't put the "Created by..." in the files).
		Otherwise, it will check for a file called INDEX.siag.
	This is the main file, and the presence of the file will be
	taken as an indication that the archive is a structured Siag file.
		Loading is done by unpacking the file in the same place as
	before, then loading the file INDEX.siag. The index file is of
	the old, flat format with extensions added to handle plugin
	modules. The plugins are specified with lines of the following format:

	.plugin x y filename
	.plugin-geometry width height [x y]

	The geometry specification is used to create the child window
	where the plugin will be displayed. The filename is the name of
	the file to be loaded by the plugin. The filename extension is
	used to select a plugin from a preconfigured list; for example,
	files ending in .gif or .jpg can be handled by a plugin able to
	display images. Eventually, I will write plugins for my own
	applications so that e.g. Pathetic Writer can be used as a plugin.
	It will even be possible to embed Siag documents inside Siag
	documents, and so on.
		Saving the file is done in the reverse order, first telling
	each plugin to save its data, then saving INDEX.siag and finally 
	tarring up the archive again.


	981008: New format code. See xcommon/fonts.c for details.
--- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>
#include "../common/common.h"
#include "../common/plugin.h"
#include "calc.h"

extern char *embed_load(char *);


char *siag_fileformat;
int make_backups = 1;

static int col_last_changed(buffer *b, int s, int row, int sf)
{
	int i;

	if (row > b->sht[s].alloc_lines) return 0;
	for (i = b->sht[s].alloc_cols[row]; i > 1; i--)
		if (ret_type(b, s, row, i) != EMPTY ||
			ret_format(b, s, row, i) != sf)
			break;
	return i;
}

/* ---
returns the name of a component in a structured file. Caller must free
*/

static char *file_name(buffer *buf, char *n)
{
	char b[1024];
	if (n[0] == '/') {		/* link */
		return MwStrdup(n);
	}
	plugin_basedir(b, buf->name);
	strcat(b, "/");
	strcat(b, n);
	return MwStrdup(b);
}


/* ---
Returns: 0 if successful, otherwise 1
*/

static int save_flat(char *fn, buffer * buf)
{
	int i, j, lastcell, lr;
	int s;
	FILE *fp;
	int sw = buf->sw, sh = buf->sh, sf = std_fmt_get(buf);
	int intp, f;
	MwFmt fmt;
	char *taddr;
	property_list *p;
	char *used_fmt = MwCalloc(MwFormatCount+1, 1);
	char *used_style = MwCalloc(nstyle+1, 1);

	if ((fp = fopen(fn, "w")) == (FILE *) 0)
		return 1;

	/* start by saving standard values */
	fprintf(fp, "# Creator: %s\n", version);
	fprintf(fp, ".sw %d\n", sw);
	fprintf(fp, ".sh %d\n", sh);

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
	fprintf(fp, ".a1_refs %d\n", buf->a1_refs);

	MwDecodeFormat(sf, ~0, &fmt);
	if (!used_style[fmt.style]) {
		/* haven't saved style before */
		save_styles(fp, fmt.style);
		used_style[fmt.style] = 1;
	}
	if (!used_fmt[sf]) {
		/* we haven't saved this format before */
		MwSaveFormats(fp, sf);
		used_fmt[sf] = 1;
	}
	fprintf(fp, ".nsf %d\n", sf);

	/* properties */
	for (p = buf->p_list; p; p = p->next) {
		fprintf(fp, ".p%d %s\n", strlen(p->value), p->key);
		fwrite(p->value, strlen(p->value), 1, fp);
		fprintf(fp, "\n");	/* terminate last line */
	}

	for (s = 0; s < buf->nsht; s++) {
		fprintf(fp, ".sheet %d %s\n", s, buf->sht[s].name);
		lr = line_last_used(buf, s);
		fprintf(fp, "# %s\n# %d lines\n#\n", fn, lr);

		for (i = 1; i <= buf->sht[s].used_lines; i++) {
			int height = cell_height(buf, s, i);
			if (height != sh)
				fprintf(fp, ".rh %d %d\n", i, height);
		}
		for (i = 1; i <= buf->sht[s].used_cols; i++) {
			int width = cell_width(buf, s, i);
			if (width != sw)
				fprintf(fp, ".cw %d %d\n", i, width);
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
			fprintf(fp, "# Line %d\n", i);
			lastcell = col_last_changed(buf, s, i, sf);
			for (j = 1; j <= lastcell; j++) {
				f = ret_format(buf, s, i, j);
				MwDecodeFormat(f, ~0, &fmt);
				if (!used_style[fmt.style]) {
					/* haven't saved style before */
					save_styles(fp, fmt.style);
					used_style[fmt.style] = 1;
				}
				if (!used_fmt[f]) {
					/* we haven't saved this format before */
					MwSaveFormats(fp, f);
					used_fmt[f] = 1;
				}
	
				switch (ret_type(buf, s, i, j)) {
				case EMPTY:
					if (f != sf)
						fprintf(fp, "%d %d %d\t#\n",
							i, j, f);
					break;
				case LABEL:
					fprintf(fp, "%d %d %d\t\"%s\n", i, j,
						f, ret_text(buf, s, i, j));
					break;
				case EMBED:
					fprintf(fp, "%d %d %d\tm%s\n", i, j,
						f, ret_text(buf, s, i, j));
					break;
				case MNUMBER:
				case MTEXT:
					break;	/* not stored at all */
			/* ERRORs and CONSTANTs are stored as EXPRESSION */
				default:	/* EXPRESSION */
		/* Special case SIOD and C for backward compatibility. */
					intp = ret_interpreter(buf, s, i, j);
					taddr = ret_text(buf, s, i, j);
					if (intp == siod_interpreter &&
							taddr[0] == '=') {
						intp = C_interpreter;
						taddr++;
					}
					if (intp == siod_interpreter) {
						fprintf(fp, "%d %d %d\t=%s\n",
							i, j, f, taddr);
					} else if (intp == C_interpreter) {
						fprintf(fp, "%d %d %d\t==%s\n",
							i, j, f, taddr);
					} else {
						fprintf(fp, "%d %d %d\t+%s,%s\n",
							i, j, f,
							interpreter2name(intp),
							taddr);
					}
				}
			}
		}
	}
	fprintf(fp, "# End of file %s\n", fn);
	fclose(fp);
	MwFree(used_fmt);
	MwFree(used_style);
	return 0;
}				/* save_flat */

/* ---
load the old flat format
Returns: 0 if successful, otherwise 1
*/

static int load_flat(char *fn, buffer *buf)
{
	int i, j, intp;
	char *texti;
	FILE *fp;
	int *fmts = NULL;
	int nfmt = -1;
	char instring[256], *p, *coords, *contents, *comma;
	int sw = 80, sh = 20, sf = 0, f;
	int s = 0;
	int *styles = NULL;
	int nsty = 0;
	int n;
	MwFmt fmt;
	int lastph = -1;
	cval value;
	char *endp;
	int type;
	value.number = 0;

	if ((fp = fopen(fn, "r")) == NULL)
		return 1;

	for (s = 0; s < buf->nsht; s++) {
		for (i = line_last_used(buf, s); i > 0; i--) {
			for (j = col_last_changed(buf, s, i, sf); j > 0; j--) {
				ins_data(buf, siod_interpreter, NULL,
					value, EMPTY, s, i, j);
				ins_format(buf, s, i, j, sf);
			}
		}
	}

	buf->a1_refs = a1_refs;

	s = 0;	/* old files have no .sheet tags, so start at first sheet */
	while (fgets(instring, 250, fp) != NULL) {
		char b[1024];
		b[0] = '\0';	/* make sure we don't keep stuff between loops */

		/* strip off \r\n */
/*
   if ((p = strpbrk(instring, "\r\n")) != NULL) *p = '\0';
 */

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
			} else if (!strncmp(instring, ".a1_refs ", 9)) {
				sscanf(instring, ".a1_refs %d",
					&(buf->a1_refs));
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
                        } else if (!strncmp(instring, ".sw ", 4)) {
				sscanf(instring, ".sw %d", &sw);
				buf->sw = sw;
			} else if (!strncmp(instring, ".sh ", 4)) {
				sscanf(instring, ".sh %d", &sh);
				buf->sh = sh;
/* The old "standard format" used the .sf tag, which was bitmapped and
must no longer be used. Thus .nsf (new standard format)
*/
			} else if (!strncmp(instring, ".nsf ", 5)) {
				sscanf(instring, ".nsf %d", &sf);
				std_fmt_set(buf, sf);
			} else if (!strncmp(instring, ".cw ", 4)) {
				int w;
				sscanf(instring, ".cw %d %d", &i, &w);
				set_width(buf, s, i, w);
			} else if (!strncmp(instring, ".rh ", 4)) {
				int h;
				sscanf(instring, ".rh %d %d", &i, &h);
				set_height(buf, s, i, h);
			} else if (!strncmp(instring, ".ft", 3)) {
				sscanf(instring, ".ft%d", &n);
				if (n > nfmt) {
					nfmt = n;
					fmts = MwRealloc(fmts,
							(nfmt+1)*sizeof *fmts);
				}
				fmts[n] = MwLoadFormats(fp);
				/* now replace style with the real one */
				MwDecodeFormat(fmts[n], ~0, &fmt);
				/* Careful now. Some versions of Siag
				   produced bogus styles
				*/
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
			} else if (!strncmp(instring, ".fmt", 4)) {
				int h;
				char b[256];
				sscanf(instring, ".fmt%d %[^\n]", &h, b);
				h >>= MW_FMT_SHIFT;	/* 0 <= h <= 15 */
				lookup_style(style2name(h), b,
						style_table[h].type);
			} else if (!strncmp(instring, ".plugin-geometry ", 17)) {
				int n, width, height;
				n = sscanf(instring, ".plugin-geometry %d %d",
						&width, &height);
				if (lastph >= 0 && n >= 2) {
					plugin_size_set(lastph,
						width, height);
				}
			} else if (!strncmp(instring, ".plugin ", 8)) {
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
			} else if (!strncmp(instring, ".p", 2)) {
				int n;
				char key[256], *value;
				sscanf(instring, ".p%d %s", &n, key);
				value = MwMalloc(n+1);
				fread(value, n, 1, fp);
				value[n] = '\0';
				put_property(buf, key, value);
				MwFree(value);
			}
			break;
		default:	/* cell definition */
			if ((coords = strtok(instring, "\t")) == NULL)
				break;
			if ((contents = strtok((char *) 0, "\t\n\r")) == NULL)
				break;
			if ((p = strtok(coords, " ")) == NULL)
				break;
			i = atoi(p);
			if ((p = strtok(NULL, " ")) == NULL)
				break;
			j = atoi(p);
			if ((p = strtok(NULL, " ")) == NULL)
				break;
			f = atoi(p);
			if (nfmt == -1 || f > nfmt) {	/* old */
				f = MwFmtOldToNew(f);
			} else {
				f = fmts[f];
			}
			switch (contents[0]) {
			case '$':	/* string expression */
				if ((texti = (contents+1)) == 0)
					return 1;
				ins_data(buf, siod_interpreter, texti,
					value, STRING, s, i, j);
				ins_format(buf, s, i, j, f);
				break;
			case '=':	/* old style expression */
	/* Special case SIOD and C for backward compatibility. */
				if (contents[1] == '=') {	/* C */
					intp = C_interpreter;
					texti = (contents+2);
				} else {	/* SIOD */
					intp = siod_interpreter;
					texti = (contents+1);
				}
				value.number = strtod(texti, &endp);
				if (texti != endp && only_space(endp)) {
					type = CONSTANT;
				} else {
					type = EXPRESSION;
				}
				if (texti == 0) return 1;
				ins_data(buf, intp, texti,
					value, type, s, i, j);
				ins_format(buf, s, i, j, f);
				break;
			case '+':	/* new style expression */
				if ((comma = strchr(contents, ',')) == NULL)
					return 1;
				*comma = '\0';
				intp = name2interpreter(contents+1);
				if (intp < 0) return 1;
				if (intp == siod_interpreter &&
						comma[1] == '=') {
			/* C expression masquerading as SIOD */
					intp = C_interpreter;
					texti = (comma+2);
				} else {
					texti = (comma+1);
				}
				if (texti == 0) return 1;
				ins_data(buf, intp,
					texti, value, EXPRESSION, s, i, j);
				ins_format(buf, s, i, j, f);
				break;
			case '\"':	/* label */
				if ((texti = (contents + 1)) == 0)
					return 1;
				ins_data(buf, siod_interpreter, texti,
					value, LABEL, s, i, j);
				ins_format(buf, s, i, j, f);
				break;
			case 'm':	/* embedded */
				if ((texti = (contents + 1)) == 0)
					return 1;
				embed_load(contents+1);
				ins_data(buf, siod_interpreter, texti,
					value, EMBED, s, i, j);
				ins_format(buf, s, i, j, f);
				break;
			default:	/* anything else means empty */
				ins_format(buf, s, i, j, f);
				break;
			}
			break;
		}
	}

	fclose(fp);
	MwFree(fmts);
	MwFree(styles);
	std_fmt_set(buf, sf);
	return 0;
} /* load_flat */

#define SIAG_MAGIC "# Creator: Siag"

static int flatfile(char *fn)
{
	char b[100];
	FILE *fp = fopen(fn, "r");
	if (!fp) return 0;
	if (fgets(b, sizeof b, fp) &&
			!strncmp(b, SIAG_MAGIC, strlen(SIAG_MAGIC))) {
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
	if (!has_plugins) /* no plugins, save as usual */
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
	b = file_name(buf, "INDEX.siag");
	result = save_flat(b, buf);
	MwFree(b);
	/* tar up the lot */
	sprintf(cmd, "(cd %s;tar cf - *)>%s", dir, fn);
	system(cmd);
	MwFree(dir);
	return result;
}

static int load(char *fn, buffer *buf)
{
	char cmd[1024], b[1024];
	char *p;
	int n;

	/* old files don't have the MAGIC but we still want to load them.
	   So first try flat, then tar, then flat again. */
	if (flatfile(fn) || !tryuntar(fn, "INDEX.siag"))
		return load_flat(fn, buf);

	p = plugin_basedir(b, buf->name);
	sprintf(cmd,
		"mkdir -p %s;"
		"cat %s 2> /dev/null |(cd %s;tar xf -) 2> /dev/null",
		p, fn, p);
	system(cmd);
	p = file_name(buf, "INDEX.siag");
	n = load_flat(p, buf);
	MwFree(p);
	return n;
}
 
/* ---
conservative file format guessing:
   1. extension .siag
   2. starts with the string "# Creator: Siag"
   3. (added for the structured format) If the above fails, try to
	untar the file in $HOME/.siag/$PID/filename and look for
	the file INDEX.siag.
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp;
	char b[100];

	ext = strrchr(fn, '.');
	if (!ext) return 0;	/* no extension */
	if (MwStrcasecmp(ext, ".siag"))
		return 0;	/* wrong extension */
	if (flatfile(fn))
		return 1;	/* old format */
	if ((fp = fopen(fn, "r")) == NULL)
		return 1;	/* new file */
	if (fgets(b, sizeof b, fp) &&
			!strncmp(b, SIAG_MAGIC, strlen(SIAG_MAGIC))) {
		fclose(fp);
		return 1;
	}
	fclose(fp);
	if (tryuntar(fn, "INDEX.siag")) return 1;
	return 0;
}

/* ---
*/
void fileio_siag_init(void)
{
	register_format(load, save, myformat, siag_fileformat = "Siag (*.siag)");
}

