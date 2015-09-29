/*
    Copyright (C) 1996-1998  Ulric Eriksson <ulric@siag.nu>

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

static int ncolor;
static struct colors {
	int r, g, b;
	char *n;
} *color;

/* ---
Prettify the colour name by initcapping it and put spaces between words.
*/

static void scan_name(char *from, char *to)
{
	int c = ' ';
	while (*from) {
		if (isspace(c) && islower(*from)) {
			*to++ = c = toupper(*from++);
		} else if (isupper(*from)) {
			if (!isspace(c)) *to++ = ' ';
			*to++ = c = *from++;
		} else {
			*to++ = c = *from++;
		}
	}
	*to = '\0';
}

/* ---
Store a colour definition in the colour table.
*/

static void insert_color(int r, int g, int b, char *n)
{
	int i;
	char n2[1024];
	scan_name(n, n2);
	for (i = 0; i < ncolor; i++) {
		if (color[i].r == r && color[i].g == g && color[i].b == b)
			break;
	}
	if (i == ncolor) {
		ncolor++;
		color = realloc(color, ncolor*sizeof *color);
	} else {
		free(color[i].n);
	}
	color[i].r = r;
	color[i].g = g;
	color[i].b = b;
	color[i].n = malloc(strlen(n2)+1);
	strcpy(color[i].n, n2);
}

static int compar(const void *p, const void *q)
{
	const struct colors *c1 = p;
	const struct colors *c2 = q;
	return strcmp(c1->n, c2->n);
}

/* ---
we can't really scale just by multiplying with 256, because we want
   0 to correspond to 0 but 255 to correspond to 65535
*/

static int sc(int n)
{
	return n*65535/255;
}

int main(void)
{
	char s[1024], n[1024];
	int r, g, b, i;

	while (fgets(s, sizeof s, stdin)) {
		if (s[0] != '!') {
			i = sscanf(s, "%d %d %d %[^\n]", &r, &g, &b, n);
			if (i == 4) {
				insert_color(r, g, b, n);
			} else {
				fprintf(stderr,
					"Couldn't read 4 items (read %d)\n", i);
			}
		} else {
			fprintf(stderr, "Skipping comment\n");
		}
	}
	qsort(color, ncolor, sizeof *color, compar);
	for (i = 0; i < ncolor; i++) {
		printf("(register-color \"%s\" %d %d %d)\n", color[i].n,
			sc(color[i].r), sc(color[i].g), sc(color[i].b));
	}
	return 0;
}

