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

/*
 * fileio_txt.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pw.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

static int save(char *fn, buffer *buf)
/* Returns: 0 if successful, otherwise 1 */
{
	FILE *fp = fopen(fn, "w");
	int s = 0;	/* save first only */
	int row = 1, to = line_last_used(buf, s);

	if (!fp) return 1;

	for (row = 1; row <= to; row++) {
		char *p = (char *)MwRcMakeplain(buf->sht[s].text[row].p);
		fprintf(fp, "%s\n", p);
		MwFree(p);
	}
	fclose(fp);
	return 0;
}

static int load(char *fn, buffer *buf)
/* Returns: 0 if successful, otherwise 1 */
{
	FILE *fp = fopen(fn, "r");
	char b[250];
	int row = 1;
	int s = 0;

	if (!fp) return 1;

	while (fgets(b, sizeof b, fp)) {
		char *p = strchr(b, '\n');
		if (p) *p = '\0';
		ins_text(buf, s, make_position(row, 0), (unsigned char *)b,
			style_table[0].format);
		row++;
	}
	fclose(fp);
	return 0;
}

/* ---
Not very conservative file format guessing:
Load any non-empty file with .txt as extension
*/

static int myformat(char *fn)
{
        char *ext;
        FILE *fp;
        char b[100];

        ext = strrchr(fn, '.');
        if (!ext) return 0;     /* no extension */
        if (MwStrcasecmp(ext, ".txt"))
                return 0;       /* wrong extension */
        if ((fp = fopen(fn, "r")) == NULL)
                return 0;       /* can't open */
        if (fgets(b, sizeof b, fp)) {
                fclose(fp);
                return 1;
        }
        fclose(fp);
        return 0;
}

/* ---
*/
void fileio_txt_init(void)
{
	register_format(load, save, myformat, "Text (*.txt)");
}

