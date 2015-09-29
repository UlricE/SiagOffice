/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2001  Ulric Eriksson <ulric@siag.nu>

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
 * fileio_latex.c
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <Mowitz/MwUtils.h>

#include "../common/common.h"

#include "calc.h"

static int col_last_changed(buffer *b, int s, int row)
{
	int i;

	if (row > b->sht[s].alloc_lines) return 0;
	for (i = b->sht[s].alloc_cols[row]; i > 1; i--)
		if (ret_type(b, s, row, i) != EMPTY)
			break;
	return i;
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int save(char *fn, buffer * buf)
{
	int i, j, lastcell, lr, lc;
	FILE *fp;
	int s = 0;
	char label[1024];

	/* save current sheet */
	if (w_list->buf == buf) s = w_list->sht;
	sprintf(label, "%s:%s", buf->name, buf->sht[s].name);

	if ((fp = fopen(fn, "w")) == (FILE *) 0)
		return 1;

	lr = line_last_used(buf, s);
	lc = 0;
	for (i = 1; i <= lr; i++) {
		lastcell = col_last_changed(buf, s, i);
		if (lastcell > lc) lc = lastcell;
	}
	fprintf(fp, "\\begin{tabular}{");
	for (j = 1; j <= lc; j++) {
		fprintf(fp, "|l");
	}
	fprintf(fp, "|}\n");
	fprintf(fp, "\\hline\n");
	for (i = 1; i <= lr; i++) {
		for (j = 1; j <= lc; j++) {
			if (ret_type(buf, s, i, j) != ERROR) {
				fprintf(fp, "%s",
					ret_pvalue(NULL, buf, s, i, j, -1));
			}
			if (j < lc) fprintf(fp, "&");
		}
		fprintf(fp, " \\\\\n\\hline\n");
	}
/*	fprintf(fp, "\\label{%s}\n", label);
*/
	fprintf(fp, "\\end{tabular}\n");

	fclose(fp);
	return 0;
}				/* save */

#define LATEX_MAGIC "\\begin{tabular}"

/* ---
conservative file format guessing:
   1. extension .tex
   2. contains a line starting with "\begin{tabular}"
*/

static int myformat(char *fn)
{
	char *ext;
	FILE *fp;
	char b[250];

	ext = strrchr(fn, '.');
	if (!ext) return 0;	/* no extension */
	if (MwStrcasecmp(ext, ".tex"))
		return 0;	/* wrong extension */
	if ((fp = fopen(fn, "r")) == NULL) return 0;	/* can't open */
	while (fgets(b, sizeof b, fp)) {
		if (!strncmp(b, LATEX_MAGIC, strlen(LATEX_MAGIC))) {
			fclose(fp);
			return 1;
		}
	}
	fclose(fp);
	return 0;
}

/* ---
*/
void fileio_latex_init(void)
{
	register_format(NULL, save, myformat, "Latex table (*.tex)");
}

