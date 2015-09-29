/*
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include <locale.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include "../config.h"
#include <Mowitz/MwUtils.h>
#include "common.h"
#include <Mowitz/MwUtils.h>

/*#define DEBUG
*/

/* ---
common.c

Device independent functions for all three programs.
--- */

/* ---
Figure out the names of directories
*/

char *libdir; /* where plugins are; default /usr/local/lib/siag */
char *datadir;	  /* where Scheme files are; default /usr/local/share/siag */
char *docdir;	  /* where online docs are; default /usr/local/doc/siag */
char *siag_basedir;	/* $HOME/.siag */
char *siag_tmpdir;	/* $HOME/.siag/tmp */
char *version;	  /* version string */

void common_init(char *fmt)
{
	char *home = getenv("SIAGHOME");
	char *docs = getenv("SIAGDOCS");
	char *lang;
	char p[1024];

	if (home) {
		libdir = datadir = MwStrdup(home);
	} else {
		libdir = LIBDIR;
		datadir = DATADIR;
	}
	if (docs) {
		docdir = MwStrdup(docs);
	} else {
		docdir = DOCDIR;
	}
	if (fmt) {
		sprintf(p, fmt, VERSION_NO);
	} else {
		sprintf(p, "Siag Office %s", VERSION_NO);
	}
	version = MwStrdup(p);
	home = getenv("HOME");
	if (home == NULL) home = "";
	sprintf(p, "%s/.siag", home);
	siag_basedir = MwStrdup(p);
	mkdir(siag_basedir, 0700);
	strcat(p, "/tmp");
	siag_tmpdir = MwStrdup(p);
	mkdir(siag_tmpdir, 0700);

	/* i18n */
	lang = getenv("LC_ALL");
	if (lang == NULL) lang = getenv("LC_MESSAGES");
	if (lang == NULL) lang = getenv("LANG");
	if (lang) {
		sprintf(p, "%s/common/dictionary.%s", datadir, lang);
		MwLoadDictionary(p);
	}
}

/* ---
Check if a string should be interpreted as a positive answer.
*/

int itsayes(char *p)
{
	return !MwStrcasecmp(p, "yes")
		|| !MwStrcasecmp(p, "1")
		|| !MwStrcasecmp(p, "on")
		|| !MwStrcasecmp(p, _("yes"))
		|| !MwStrcasecmp(p, _("on"));
}

/* a little bookkeeping to keep track of temp files and make sure they
   are deleted when child processes terminate */

static int ndeletia;
static struct {
	long p;
	char *fn;
} *deletia;

/* ---
associate a temp file with a process
*/

void deletia_add(long p, char *fn)
{
	int i;

	for (i = 0; i < ndeletia; i++)
		if (deletia[i].fn == NULL) break;
	if (i == ndeletia) {
		ndeletia++;
		deletia = MwRealloc(deletia, ndeletia*sizeof *deletia);
	}
	deletia[i].p = p;
	deletia[i].fn = MwStrdup(fn);
}

/* ---
mark all files related to a process to be deleted later
p == 0 marks all files
*/

void deletia_mark(long p)
{
	int i;

	for (i = 0; i < ndeletia; i++) {
		if (p == 0 || p == deletia[i].p)
			deletia[i].p = 0;
	}
}

/* ---
remove files that have previously been marked
*/

void deletia_reap(void)
{
	int i;

	for (i = 0; i < ndeletia; i++) {
		if (deletia[i].p == 0 && deletia[i].fn) {
			remove(deletia[i].fn);
			MwFree(deletia[i].fn);
			deletia[i].fn = NULL;
		}
	}
}

/* ---
Signal handler for dead children.
*/

void waitforchild(int i)
{
	pid_t p;
	long lp;

	p = waitpid(-1, NULL, WNOHANG);
	lp = (long)p;
	if (p > 0) deletia_mark(lp);
	signal(SIGCHLD, waitforchild);
}

#define TAR_MAGIC "ustar"

/* ---
Check if a file is a tar archive and if an index file is present.
Returns 1 for success.
*/

int tryuntar(char *fn, char *index)
{
	FILE *fp;
	char b[1024];
	int result, n;
	struct stat statbuf;

	fp = fopen(fn, "r");
	if (!fp) {
		return 0;
	}
	n = fread(b, 1, 300, fp);
	if (n != 300) {
		fclose(fp);
		return 0;
	}
	if (strncmp(b+257, TAR_MAGIC, strlen(TAR_MAGIC))) {
		fclose(fp);
		return 0;
	}
	fclose(fp);
	sprintf(b,
		"mkdir -p %s/untar;"
		"cat %s 2> /dev/null |(cd %s/untar;tar xf - %s) 2> /dev/null",
		siag_basedir, fn, siag_basedir, index);
	system(b);
	sprintf(b, "%s/untar/%s", siag_basedir, index);
	result = !stat(b, &statbuf);
	sprintf(b, "rm -rf %s/untar", siag_basedir);
	system(b);
	return result;
}

int only_space(char *p)
{
	while (*p && isspace(*p)) p++;
	return (*p == '\0');
}

char *plugin_basedir(char *dir, char *bufname)
{
	int i;
	pid_t pid = getpid();

	sprintf(dir, "%s/%ld/%s", siag_basedir, (long)pid, bufname);
	for (i = strlen(dir)-strlen(bufname); dir[i]; i++)
		if (!isalnum(dir[i])) dir[i] = '_';
	return dir;
}


/* global variables */

int pr_scr_flag;                /* if the display needs updating */
int input_warp_pointer;         /* move cursor to input field */
