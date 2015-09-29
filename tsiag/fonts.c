/*
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
 * fonts.c
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../common/common.h"
#include <Mowitz/MwUtils.h>
#include <Mowitz/MwFormat.h>

char **font_list(int *n)
{
	return NULL;
}

char **color_list(int *n)
{
	return NULL;
}


static MwFmt default_format =
	{"Helvetica", 100, 0, 0, 0, 0, "black", "white", 0, 0, 0, 0};

void MwDecodeFormat(int n, long mask, MwFmt *fmt)
{
	*fmt = default_format;
}

int MwEncodeFormat(long mask, MwFmt *fmt)
{
	return 0;
}


void ps_set_color(FILE *fp, int red, int green, int blue)
{
	;
}

/* and now a bunch of stubs so tsiag can compile */
int MwFmtAttrToMask(char *attr){return 0;}

char **MwFontList(int *n){*n = 0; return NULL;}

char **MwColorList(int *n){*n = 0; return NULL;}

int MwFormatCount = 0;

void MwSaveFormats(FILE *fp, int n){;}

int MwLoadFormats(FILE *fp){return 0;}

int MwFmtOldToNew(long n){return 0;}

void MwPsSetColor(FILE *fp, int a, int b, int c){;}

void MwPsSetFont(FILE *fp, int a){;}

MwRichchar *MwRcMakerich(unsigned char *p, int n){return NULL;}

float MwRcStrwidth(MwRichchar *p, int n){return 0.0;}

int MwRcStrheight(MwRichchar *p, int n){return 0;}

void MwPsMakeFonts(FILE *fp){;}

void MwInitColors(void){;}

void MwInitFonts(void){;}

long MwFmtNewToOld(int n){return 0;}

