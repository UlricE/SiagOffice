/*
   Egon Animator
   Copyright (C) 1997-2002  Ulric Eriksson <ulric@siag.nue>

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
 * egon.h
 */

#include <Mowitz/MwFormat.h>
#include <Mowitz/MwAnimatorTypes.h>

typedef struct plugin_t {
        int row, col;                  /* geometry */
        int ph;                         /* handle */
        char *name;                     /* file name */
        int displayed;                  /* started yet? */
} plugin_t;

typedef struct sheet {
	char *name;
	char *bgrad;		/* background gradient */
	char *bg;		/* background pixmap */
	MwAniObject *cast;	/* objects in animation sequence */
	unsigned int now;	/* time in ms */
	unsigned int delta;	/* ms between frames */
	unsigned int duration;	/* of animation */
	plugin_t *plugin;	/* array of plugins */
	int nplugin;		/* number of plugins */
} sheet;

typedef struct s_buffer {
	char name[1024];	/* identifies the buffer */
	char path[1024];	/* file name */
	sheet *sht;		/* the sheet array */
	int nsht;		/* number of sheets */
	unsigned int width, height;
	int state;		/* ANI_STOP et al */
	int change;		/* if there are pending changes */
	char *paper_name;	/* A4, letter... */
	int paper_width, paper_height;	/* in points */
	int top_margin, bottom_margin, left_margin, right_margin; /* 72 */
	int header_margin, footer_margin;
	char *header, *footer;
	int header_on_first;	/* default True */
	int first_page_number;	/* default 1 */
	int orientation;	/* default portrait */
	struct s_buffer *next;	/* next buffer in buffer ring */
} buffer;

typedef struct s_window {
	int sht;		/* sheet number */
	MwAniObject *object;	/* selected object */
	MwAniScript *script;	/* selected tick */
	buffer *buf;		/* buffer */
	struct egon_ui *ui;	/* widgets */
	struct s_window *next;	/* next window in window ring */
} window;

enum {PORTRAIT = 0, LANDSCAPE};

/* from window.c */

extern window *w_list;

extern void activate_window(window *);
extern void llpr(char *);
extern void free_window(window *);
extern window *new_window(buffer *, window *);
extern int remove_window(window *);
extern int split_window(window *);
extern void init_windows(buffer *, int *, char **);
extern void exit_windows(void);
extern void show_cur(window *);
extern void hide_cur(window *);

extern void mainloop(void);


/* from matrix.c */
extern void free_matrix(MwAniObject *);
extern MwAniObject *last_object(sheet *);
extern MwAniScript *last_script(MwAniObject *);
extern int ins_format(buffer *, MwAniObject *, MwAniScript *, int);
extern int ret_format(buffer *, MwAniObject *, MwAniScript *);
extern char *type2name(int);
extern int name2type(char *);

/* from cmds.c */
extern void init_cmds(void);

/* from buffer.c */
extern buffer *b_list;
extern char *paper_name;
extern int paper_width;
extern int paper_height;
extern int left_margin, right_margin, top_margin, bottom_margin;
extern int header_margin, footer_margin;
extern char *header, *footer;
extern int header_on_first;
extern int first_page_number;
extern int orientation;
extern char *buffer_name(char *);
extern int buffer_add_sheet(buffer *, int);
extern int buffer_remove_sheet(buffer *, int);
extern void buffer_rename_sheet(buffer *, int, char *);
extern buffer *new_buffer(char *, char *);
extern buffer *free_buffer(buffer *);
extern void plugin_unique_name(char *, char *);
extern buffer *find_sheet_by_name(char *, buffer *, int *);
extern buffer *find_buffer_by_name(char *);
extern int buffer_plugin2index(buffer *, int);


extern int ok2print;
extern int siod_interpreter;

/* from print.c */
extern int paper_width;
extern int paper_height;
extern int margin;
extern void preview(void);
extern void printer(void);

/* from railway.c */
extern int errorflag;
extern char *interpreter2name(int);
extern int name2interpreter(char *);
extern int register_interpreter(char *, double (*)(char *), void (*)(char *));
extern int is_constant(char *);
extern double parse_expr(MwAniObject *, char *, int, int);
extern int calc_matrix(MwAniObject *);
#define execute(p) exec_expr(siod_interpreter,(p))
extern void exec_expr(int, char *);
extern void va_execute(char *, ...);

/* from fileio.c */
extern char *loader_patterns[];
extern char *saver_patterns[];
extern void register_format(int (*)(char *, buffer *),
			int (*)(char *, buffer *),
			int (*)(char *), char *);
extern void fileio_init(void);
extern int savematrix(char *, buffer *, char *);
extern int loadmatrix(char *, buffer *, char *);
extern char *guess_file_format(char *);

extern int make_backups;

/* from position.c */

extern void init_position(void);

