/*
   dummy.c
   Copyright (C) 1997-2001  Ulric Eriksson <ulric@siag.nu>

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
This plugin handles applications that don't understand the communication
protocol. It does this by taking the file name on the command line,
looking up the appropriate application in its configuration file,
forking and executing the command.
--- */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <X11/Xlib.h>
#include "../siod/siod.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

extern void execute(char *);
extern int init_parser(int, char **);

static char *cmd, *window_name;
static pid_t pid;

static Display *display;

static struct {
	char *desc, *ext, *cmd, *name;
} *handler = NULL;

int nhandler = 0;

static Window victim = None;

static int reused_victim(Display *display, Window w)
{
        Window root, parent, *child;
        unsigned int n;
        XQueryTree(display, w, &root, &parent, &child, &n);
        return (parent == root);
}

static Window find_window_by_name(Display *display, Window w,
                                char *name, int depth)
{
        Window root, parent, *child, w1 = None;
        int i, d;
        unsigned int n;
        char *found;

        XSync(display, False);
        if (XFetchName(display, w, &found)) {
                d = strncmp(found, name, strlen(name));
                XFree(found);
                if (!d && !reused_victim(display, w)) return w;
        }
        if (!depth) return None;
        XSync(display, False);
        XQueryTree(display, w, &root, &parent, &child, &n);
        for (i = 0; i < n; i++) {
                w1 = find_window_by_name(display, child[i], name, depth-1);
                if (w1 != None) break;
        }
        XFree(child);
        return w1;
}

static int keep_searching;

static void wakeme(int i)
{
        keep_searching = 0;
}

/* ---
returns window id of victim or None if not found
*/

static Window find_victim(Display *display, char *name)
{
        Window victim = None;

        XSync(display, False);
        keep_searching = 1;
        signal(SIGALRM, wakeme);
        alarm(60);      /* give up after one minute */
        while (keep_searching && victim == None) {
                victim = find_window_by_name(display,
                        RootWindowOfScreen(DefaultScreenOfDisplay(display)),
                        name, 3);
        }
        alarm(0);       /* turn off the alarm */

        return victim;
}

static int plugin_handler_find(char *ext)
{
        int i;

        for (i = 0; i < nhandler; i++)
                if (!strcmp(ext, handler[i].ext)) return i;

        return -1;
}

/* ---
*/
int plugin_register(char *desc, char *ext, char *cmd, char *name)
{
        char b[1024];
        int n = plugin_handler_find(ext);

        if (n == -1) {  /* allocate room for new plugin */
                n = nhandler++;

                handler = MwRealloc(handler, nhandler*sizeof *handler);
        } else {        /* reuse old position */
                MwFree(handler[n].desc);
                MwFree(handler[n].ext);
                MwFree(handler[n].cmd);
		MwFree(handler[n].name);
        }

        handler[n].desc = MwStrdup(desc);
        handler[n].ext = MwStrdup(ext);
        handler[n].cmd = MwStrdup(cmd);
	handler[n].name = MwStrdup(name);
        sprintf(b, "%s (*.%s)", desc, ext);
        return 0;
}

static LISP lplugin_register(LISP desc, LISP ext, LISP cmd, LISP name)
{
	plugin_register(get_c_string(desc),
			get_c_string(ext),
			get_c_string(cmd),
			get_c_string(name));
	return NIL;
}

static void save(char *p)
{
        if (*p++ != ' ' || *p == '\0') printf("501 File name missing\n");
        else {
		printf("250 Saving %s\n", p);
		if (!fork()) execlp("touch", "touch", p, (char *)0);
	}
}

static void load_(char *p)
{
        if (*p++ != ' ' || *p == '\0') printf("501 File name missing\n");
        else printf("250 Loading %s\n", p);
}

static void exec_(char *p)
{
        if (*p++ != ' ' || *p == '\0') {
		execute(p);
		printf("250 OK\n");
	} else {
		printf("502 Can't execute %s\n", p);
	}
}

static void help(char *p)
{
        printf("214 SAVE LOAD EXEC HELP NOOP QUIT PRNT\n");
}

static void noop(char *p)
{
        printf("250 OK\n");
}

static void win(char *p)
{
	printf("250 %lx\n", (unsigned long)victim);
}

static void quit_(char *p)
{
	int i;

	kill(pid, SIGINT);
	sleep(1);
	waitpid(pid, NULL, WNOHANG);
	errno = 0;
	i = kill(pid, SIGHUP);
	if (!i || errno != ESRCH) {
		sleep(1);
		waitpid(pid, NULL, WNOHANG);
		kill(pid, SIGKILL);
	}
        printf("221 Over and out\n");
	XCloseDisplay(display);
        exit(0);
}

static void prnt(char *p)
{
        printf("502 Can't print yet\n");
}

static struct {
        char *verb;
        void (*cb)(char *);
} cmds[] = {
        {"SAVE", save},
        {"LOAD", load_},
        {"EXEC", exec_},
        {"HELP", help},
	{"NOOP", noop},
	{"WIN", win},
        {"QUIT", quit_},
        {"PRNT", prnt},
        {NULL, NULL}
};

/* ---
*/
void mainloop(void)
{
	char b[1024];
	int i;

        while (fgets(b, sizeof b, stdin)) {
                MwChomp(b);
                for (i = 0; cmds[i].verb; i++) {
                        if (!strncmp(b, cmds[i].verb, strlen(cmds[i].verb)))
                                break;
                }
                if (cmds[i].verb)
                        (*cmds[i].cb)(b+strlen(cmds[i].verb));
                else
                        printf("500 What are you talking about?\n");
                fflush(stdout);
        }
}

static char **parse_cmd(char *cmd, char *fn, char *name)
{
        int i = 0;
        char **argv = (char **)malloc(10*sizeof(char *));
        char *cmd_copy = strdup(cmd);
        char *p, *q;

        for (p = strtok(cmd_copy, " \n"); p && i < 9; p = strtok(NULL, " \n")) {
                if (p[0] == '%') {
                        switch (p[1]) {
                        case 's':
                                argv[i++] = strdup(fn);
                                break;
			case 'B':
				argv[i++] = strdup(fn);
				if ((q = strrchr(argv[i-1], '.'))) *q = '\0';
				if ((q = strrchr(argv[i-1], '/')))
					memmove(argv[i-1], q+1,
						strlen(q));
				break;
                        case 'W':
                                argv[i++] = strdup(name);
                                break;
                        case '%':
                                argv[i++] = p+1;
                                break;
                        default:
                                ;
                        }
                } else {
                        argv[i++] = p;
                }
        }
        argv[i] = NULL;
        return argv;
}

/* ---
*/
int main(int argc, char **argv)
{
        char *fn, *ext;
        int i;
	char b[1024];

	if (argc < 2) {
		printf("501 Nonsense command line\n");
		return 0;
	}

	common_init(NULL);

	init_parser(argc, argv);
	init_subr_4("dummy-handler", lplugin_register);
	sprintf(b, "(define datadir \"%s\")", datadir);
	execute(b);
	sprintf(b, "(load \"%s/plugins/dummy.scm\")", datadir);
	execute(b);
	sprintf(b, "(load \"%s/dummy.scm\")", siag_basedir);
	execute(b);

	/* file name is last arg, ignore the rest */
	fn = argv[argc-1];
	ext = strrchr(fn, '.');
	if (!ext) ext = fn;
	else ext++;
	for (i = 0; i < nhandler; i++) {
		if (!strcmp(ext, handler[i].ext)) break;
	}
	if (i == nhandler) {
		printf("501 Can't handle %s\n", ext);
		return 0;
	}

	window_name = handler[i].name;
	cmd = handler[i].cmd;
	pid = fork();

	if (pid == -1) {
		printf("501 Can't fork\n");
		return 0;
	} else if (pid == 0) {
		char **argv = parse_cmd(cmd, fn, window_name);
		execvp(argv[0], argv);
		exit(0);
	} else {
		display = XOpenDisplay(NULL);
		victim = find_victim(display, window_name);
		if (victim == None) {
			printf("500 Cannot start\n");
			XCloseDisplay(display);
			return 0;
		}
        	printf("220 Dummy plugin loaded\n");
        /* fflush necessary because we're not connected to a terminal */
        	fflush(stdout);
		mainloop();
	}
        return 0;
}

int ok2print = 0;
int errorflag = 0;

/* ---
*/
void llpr(char *p)
{
	;	/* nothing, really */
}

