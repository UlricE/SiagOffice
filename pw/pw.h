/*
   Pathetic Writer
   Copyright (C) 1997-2003  Ulric Eriksson <ulric@siag.nu>

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
 * pw.h
 */

#include <Mowitz/MwFormat.h>

#define BUFFER_COLS 100000
#define BUFFER_ROWS 100000

#define MAKE_POSITION(p)\
	(cons(flocons((p).row),cons(flocons((p).col),NIL)))

#define POSITION_ROW(p) (get_c_long(car(p)))
#define POSITION_COL(p) (get_c_long(cadr(p)))

typedef struct {
	int row, col;
} position;

typedef struct s_style {
        char *name;             /* Default aso */
        unsigned int format;   /* bitwise OR of formats above */
	MwFmt fmt;		/* format description */
        int follower;           /* what style a new line gets */
} style;

/* one line */
typedef struct rich_text {
        int height;		/* in decipoints */
        int sty;
        int adj;		/* left, right or center */
	char bop;		/* beginning of paragraph */
        MwRichchar *p;
} rich_text;

typedef struct plugin_t {
	int row, col;			/* geometry */
	int ph;				/* handle */
	char *name;			/* file name */
	int displayed;			/* started yet? */
} plugin_t;

typedef struct sheet {
	char *name;
	position point_pos;	/* point */
	position mark_pos;	/* mark */
	position blku, blkl;	/* upper left & lower right block corners */

	int alloc_lines;	/* number of allocated lines */
	int used_lines;		/* number of used lines */
	rich_text *text;	/* one pointer for each line */
	plugin_t *plugin;	/* array of plugins */
	int nplugin;		/* number of plugins */
	char *tabs;		/* tab string */
} sheet;

typedef struct s_buffer {
	char name[1024];	/* identifies the buffer */
	char path[1024];	/* file name */
	sheet *sht;		/* the sheet array */
	int nsht;		/* number of sheets */
	int change;		/* if there are unsaved changes */
	char *paper_name;	/* A4, letter... */
	int paper_width, paper_height;	/* in points, default NxM */
	int top_margin, bottom_margin, left_margin, right_margin; /* 72 */
	int header_margin, footer_margin;
	char *header, *footer;	/* default foo */
	int header_on_first;	/* default False */
	int first_page_number;	/* default 1 */
	int orientation;	/* default portrait */
	int height_interest;	/* whether to calculate row height */
	struct s_buffer *next;	/* next buffer in buffer ring */
} buffer;

typedef struct s_window {
	buffer *buf;		/* buffer */
	int sht;		/* sheet number */
	int bsht;		/* which sheet has the block? */
	int current_fmt;	/* currently used format */
	struct pw_ui *ui;	/* user interface specifics (Widgets) */
	struct s_window *next;	/* next window in window ring */
} window;

typedef struct {
  int size;             /* the number of keys pressed */
  int maxsize;          /* size of the buffer */
  char *text;           /* the textual contents */
} textbuf;

typedef enum {SPELL_WAIT = 0, SPELL_REPLACE, SPELL_ACCEPT, SPELL_INSERT,
                SPELL_SKIP, SPELL_CANCEL, SPELL_HELP} spell_state;

enum {PORTRAIT = 0, LANDSCAPE};

#include "user_interface.h"

#define block_upper(w) ((w)->buf->sht[(w)->bsht].blku)
#define block_lower(w) ((w)->buf->sht[(w)->bsht].blkl)


/* from window.c */
extern window *w_list;
extern int pr_line_flag;	/* updating the current line is enough */

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

extern void get_char_coords(window *, int, int, int, int, int *, int *);
extern void get_coords_cell(window *, int, int, int *, int *, int, int);

/* from matrix.c */
extern int nstyle;
extern style *style_table;
extern void do_bp_styles(void);
extern char *style2name(int);
extern int name2style(char *);
extern int lookup_style(char *, char *, char *,
			int, int, int, int, int, char *);
extern char **style_list(int *);
extern int style_height(int);
extern void save_styles(FILE *fp, int);
extern int load_styles(FILE *fp);
extern int max_columns;
extern int max_lines;
extern rich_text *new_text(void);
extern void free_text(rich_text *);
extern void alloc_line(buffer *, int, int);
extern int split_line(buffer *, int, int, int);
extern int join_lines(buffer *, int, int);
extern int ins_text(buffer *, int, position, unsigned char *, int);
extern int ins_char(buffer *, int, int, int, int, int);
extern unsigned char *peek_line(buffer *, int, int);
extern int peek_char(buffer *, int, int, int);
extern int rebreak_line(buffer *, int, int);
extern int del_char(buffer *, int, int, int);
extern int del_text(buffer *, int, position, int);
extern int del_lines(buffer *, int, int, int);
extern int ins_format(buffer *, int, int, int, int, int);
extern void set_style(buffer *, int, int, int);
extern int ret_style(buffer *, int, int);
extern void set_bop(buffer *, int, int, int);
extern int ret_bop(buffer *, int, int);
extern int ret_format(buffer *, int, int, int);
extern int ret_vadj(buffer *, int, int);
extern int ret_hadj(buffer *, int, int);
extern int line_last_used(buffer *, int);
extern int col_last_used(buffer *, int, int);
extern void downshift_text(buffer *, int, int);
extern void upshift_text(buffer *, int, int);
extern void position_kludge2(void);
extern int row2page(buffer *, int, int);
extern int page2row(buffer *, int, int);
extern char *pack_area(buffer *, int, int, int, int, int, unsigned int *);
extern char *pack_string_area(buffer *, int, int, int, int, int, unsigned int *);
extern char *pack_string(buffer *, int, int, int, int, int);
extern void unpack_area(buffer *, char *, int, int, int);
extern void unpack_string_area(buffer *, char *, int, int, int);

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
extern int tab_distance;
extern char *buffer_name(char *);
extern int buffer_add_sheet(buffer *, int);
extern int buffer_remove_sheet(buffer *, int);
extern void buffer_rename_sheet(buffer *, int, char *);
extern buffer *new_buffer(char *, char *);
extern buffer *free_buffer(buffer *);
extern void plugin_unique_name(char *, char *);
extern buffer *find_buffer_by_name(char *);
extern buffer *find_sheet_by_name(char *, buffer *, int *);
extern int char_width(buffer *, int, int, int);
extern int check_line_height(buffer *, int, int);
extern int line_height(buffer *, int, int);
extern int line_width(buffer *, int, int, int);
extern int line_length(buffer *, int, int);
extern int cell_height(buffer *, int, int);
extern void buffer_global_coords(buffer *, int, int, int, int *, int *);
extern int buffer_plugin2index(buffer *, int, int);
extern void buffer_cleanup(buffer *);

/* from input.c */
extern void init_input(void);
extern int ask_for_str_comp(char *, char *, int (*)(char *));
extern int ask_for_str(char *, char *);

/* from main.c */
extern int ok2print;
extern int plugin;
extern int siod_interpreter;

/* from fileio_ps.c */
extern void preview(void);
extern void printer(void);

/* from railway.c */
extern int errorflag;
extern char *interpreter2name(int);
extern int name2interpreter(char *);
extern int register_interpreter(char *, double (*)(void), void (*)(char *));
extern int is_constant(char *);
#define execute(p) exec_expr(siod_interpreter,(p))
extern void exec_expr(int, char *);
extern void va_execute(char *, ...);

/* from fileio.c */
extern char *loader_patterns[];
extern char *saver_patterns[];
extern void register_format(int (*)(char *, buffer *),
		int (*)(char *, buffer *), int (*)(char *), char *);
extern void fileio_init(void);
extern int savematrix(char *, buffer *, char *);
extern int loadmatrix(char *, buffer *, char *);
extern char *guess_file_format(char *);
extern void get_format_handlers(char *, int *, int *);

extern int make_backups;

/* from position.c */
extern position make_position(int, int);
extern position set_point(window *, position);
extern position get_point(window *);
extern position set_mark(window *, position);
extern position get_mark(window *);
extern position set_top(window *, position);
extern position get_top(window *);
extern position find_beginning_of_buffer(window *);
extern position find_end_of_buffer(window *);
extern int at_end_of_line(window *, position);
extern int at_beginning_of_line(window *, position);
extern position find_beginning_of_line(window *, position);
extern position find_end_of_line(window *, position);
extern position line_forward(window *, position);
extern position line_backward(window *, position);
extern int at_end_of_buffer(window *, position);
extern position cell_forward(window *, position);
extern position cell_backward(window *, position);

extern void init_position(void);

/* from mathwrap.c */
extern void init_mathwrap(void);

/* from fontsel.c */
extern int font_input(int *, int *);

