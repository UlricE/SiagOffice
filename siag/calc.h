/*
   Siag, Scheme In A Grid
   Copyright (C) 1996-2003  Ulric Eriksson <ulric@siag.nu>

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
 * calc.h
 */

#ifndef CALC_H
#define CALC_H

#define BUFFER_COLS 100000
#define BUFFER_ROWS 100000

#define MAKE_POSITION(p)\
	(cons(flocons((p).row),cons(flocons((p).col),NIL)))

#define POSITION_ROW(p) (get_c_long(car(p)))
#define POSITION_COL(p) (get_c_long(cadr(p)))

#define EMPTY 0
#define LABEL 1
#define ERROR 2
#define CONSTANT 3
#define EXPRESSION 4
#define STRING 5
#define EMBED 6
#define MNUMBER 7	/* transient number in matrix or such */
#define MTEXT 8		/* string in matrix or table */

#define IS_NUMBER(x) ((x)==EXPRESSION||(x)==CONSTANT||(x)==MNUMBER)

enum {PORTRAIT = 0, LANDSCAPE};

typedef struct style {
	char *name;
	char *fmt;
	int type;
} style;

typedef struct {
	int row, col;
} position;

typedef union {
	char *text;
	double number;
} cval;

typedef struct s_spread {
	char *text;
	int interpreter;
	cval value;
	short type;
	int format;
} spread;

typedef struct s_property_list {
	char *key;
	char *value;
	struct s_property_list *next;
} property_list;

typedef struct plugin_t {
#if 1	/* cell based coords */
	int row, col;		/* geometry */
#else	/* pixel based coords */
	int x, y;		/* geometry */
#endif
	int ph;			/* handle */
	char *name;		/* file name or whatever */
	int displayed;		/* has it been put up on the screen? */
} plugin_t;

typedef struct sheet {
	char *name;		/* identifies the sheet */
	int alloc_lines;	/* number of allocated lines */
	int longest_line;	/* last column of longest line */
	int *alloc_cols;	/* array of column sizes */
	int used_lines;	/* this is for the height array */
	int used_cols;		/* this is for the width array */
	int *height;		/* array of ints, (longest_line+1) in size */
	int *width;		/* array of ints, (alloc_lines+1) in size */
	position point_pos;	/* point */
	position mark_pos;	/* mark */
	position top;		/* left top of visible part of buffer */
	position prot;		/* protected lines/cols */
	position blku, blkl;	/* upper left & lower right block corners */
	spread **matrix;	/* the cell array */
	plugin_t *plugin;	/* array of plugins */
	int nplugin;		/* number of plugins */
} sheet;

typedef struct s_buffer {
	char name[1024];	/* identifies the buffer */
	char path[1024];	/* file name */

	int sh;			/* standard height */
	int sw;			/* standard width */
	int sf;			/* standard format */
	int a1_refs;		/* A1 or R1C1 references */
	sheet *sht;		/* the sheet array */
	int nsht;		/* number of sheets */
	int recalc;		/* if buffers need recalculating */
	int change;		/* if there are pending changes */
	char *paper_name;	/* Letter, A4... */
	int paper_width, paper_height;
	int top_margin, bottom_margin, left_margin, right_margin;
	int header_margin, footer_margin;
	char *header, *footer;
	int header_on_first;
	int first_page_number;
	int respect_protection;
	int orientation;
	property_list *p_list;	/* arbitrary data */
	struct s_buffer *next;	/* next buffer in buffer ring */
} buffer;

typedef struct s_window {
	buffer *buf;		/* buffer */
	int sht;		/* sheet number */
	int bsht;		/* where is the selection? */
	position sel;		/* ditto */
	struct siag_ui *ui;	/* user interface specifics (Widgets) */
	struct s_window *next;	/* next window in window ring */
} window;

typedef struct s_eval_struct {
	int errorflag;
	int siag_type;
	int siag_row;
	int siag_col;
	buffer *siag_buffer;
	struct s_eval_struct *next;
} eval_struct;

typedef struct {
  int size;             /* the number of keys pressed */
  int maxsize;          /* size of the buffer */
  char *text;           /* the textual contents */
} textbuf;

#include "user_interface.h"

#define set_window_buffer(w,b) ((w)->buf=(b))
#define next_window(w) ((w)->next)
#define buffer_of_window(w) ((w)->buf)
#define block_upper(w) ((w)->buf->sht[(w)->bsht].blku)
#define block_lower(w) ((w)->buf->sht[(w)->bsht].blkl)
#define set_point_row(w,r) ((w)->buf->sht[(w)->sht].point_pos.row=(r))
#define set_point_col(w,c) ((w)->buf->sht[(w)->sht].point_pos.col=(c))
#define set_mark_row(w,r) ((w)->buf->sht[(w)->sht].mark_pos.row=(r))
#define set_mark_col(w,c) ((w)->buf->sht[(w)->sht].mark_pos.col=(c))
#define set_top_row(w,r) ((w)->buf->sht[(w)->sht].top.row = (r))
#define set_top_col(w,r) ((w)->buf->sht[(w)->sht].top.col = (r))
#define set_prot_row(w,r) ((w)->buf->sht[(w)->sht].prot.row = (r))
#define set_prot_col(w,r) ((w)->buf->sht[(w)->sht].prot.col = (r))
#define set_prot(w,p) ((w)->buf->sht[(w)->sht].prot=(p))
#define get_prot(w) ((w)->buf->sht[(w)->sht].prot)
#define set_blku(w,p) ((w)->buf->sht[(w)->bsht].blku=(p))
#define set_blkl(w,p) ((w)->buf->sht[(w)->bsht].blkl=(p))
#define set_sel(w,p) ((w)->sel=(p))

/* from interface-specific window.c */
extern void activate_window(window * );
extern void llpr(char *);
extern void free_window(window * );
extern window *new_window(buffer *, window * );
extern void init_windows(buffer *, int *, char **);
extern void exit_windows(void);
extern void show_cur(window * );

/* from shared window.c */
extern int remove_window(window * );
extern int split_window(window * );
extern void hide_cur(window * );
extern void get_cell_coords(window *, int, int, int, int, int *, int *);
extern void get_coords_cell(window *w, int, int, int *, int *, int, int);
extern unsigned int rowcol_format(void);
extern int ask_for_str(char *, char *);
extern int cell_next_row(window *, int);
extern int cell_next_col(window *, int);
extern int cell_prev_row(window *, int);
extern int cell_prev_col(window *, int);

/* from matrix.c */
#define STY_DEFAULT 0
#define STY_INTEGER 1
#define STY_FLOAT 2
#define STY_TIME 3
#define STY_INVISIBLE 4
#define STY_PERCENT 5
extern int nstyle;
extern style *style_table;
extern char *style2name(int);
extern int name2style(char *);
extern int lookup_style(char *name, char *fmt, int type);
extern char **style_list(int *);
extern void save_styles(FILE *fp, int);
extern int load_styles(FILE *);
extern void std_fmt_set(buffer *, int);
extern int std_fmt_get(buffer *);
extern void free_rows(buffer *, int, int);
extern void free_matrix(spread **);
extern void swap_cells(buffer *, int, int, int, int, int);
extern int ins_data(buffer *, int, char *, cval, short, int, int, int);
extern int ins_format(buffer *, int, int, int, int);
extern char *ret_text(buffer *, int, int, int);
extern int ret_interpreter(buffer *, int, int, int);
extern char *ret_string(buffer *, int, int, int);
extern cval ret_val(buffer *, int, int, int);
extern short ret_type(buffer *, int, int, int);
extern int ret_format(buffer *, int, int, int);
extern int ret_font(buffer *, int, int, int);
extern char *ret_pvalue(char *, buffer *, int, int, int, int);
extern int line_last_used(buffer *, int);
extern int col_last_used(buffer *, int, int);
extern void downshift_matrix(buffer *, int, int);
extern void upshift_matrix(buffer *, int, int);
extern void rightshift_matrix(buffer *, int, int);
extern void leftshift_matrix(buffer *, int, int);
extern void fill_area(buffer *, int, int, int, int, int);
extern char *pack_string(buffer *, int, int, int, int, int);
extern void unpack_string(buffer *, char *, int, int, int);
extern char *pack_area(buffer *, int, int, int, int, int, unsigned int *);
extern void unpack_area(buffer *, char *, int, int, int);
extern int undo_save(buffer *, int, int, int, int, int);
extern int undo_restore(buffer *);

/* from cmds.c */
extern void init_cmds(void);
extern void do_cmd(int);
extern void init_calc_cmds(void);


/* from buffer.c */
extern buffer *b_list;
extern char *paper_name;
extern int paper_height, paper_width;
extern int top_margin, bottom_margin, left_margin, right_margin;
extern int header_margin, footer_margin;
extern char *header, *footer;
extern int header_on_first, first_page_number, orientation, respect_protection;
extern char *buffer_name(char *);
extern char *put_property(buffer *, char *, char *);
extern char *get_property(buffer *, char *);
extern int buffer_add_sheet(buffer *, int);
extern int buffer_remove_sheet(buffer *, int);
extern void buffer_rename_sheet(buffer *, int, char *);
extern buffer *new_buffer(char *, char *);
extern buffer *free_buffer(buffer *);
extern void plugin_unique_name(char *, char *);
extern buffer *find_buffer_by_name(char *);
extern buffer *find_sheet_by_name(char *, buffer *, int *);
extern int cell_width(buffer *, int, int);
extern int cell_height(buffer *, int, int);
extern void set_width(buffer *, int, int, int);
extern void set_height(buffer *, int, int, int);
extern void buffer_global_coords(buffer *, int, int, int, int *, int *);
extern int buffer_plugin2index(buffer *, int, int);
extern void buffer_cleanup(buffer *);

/* from main.c */
extern int ok2print, grid_only, plugin;
extern int C_interpreter, siod_interpreter;

/* from print.c */
extern void plot(int);
extern void preview(void);
extern void printer(void);

/* from railway.c */
extern int errorflag;
extern int siag_type;
extern int a1_refs;
extern char *interpreter2name(int);
extern int name2interpreter(char *);
extern int register_interpreter(char *,
	cval (*)(buffer *, char *, int, int, int),
	void (*)(char *),
	char *(*)(buffer *, char *, int, int, int, int, int, int));
extern cval parse_expr(buffer *, int, char *, int, int, int);
extern int calc_matrix(buffer *);
extern int ref_counter(buffer *, char *);
extern void ref_expander(buffer *, char *, char *, char *, char *, char *);
extern char *a1coord(int);
extern int ref_updater(buffer *, char *, char *, char *,
			int, int, int, int, int, int);
extern char *update_references(buffer *, int, char *,
			int, int, int, int, int, int);
extern void update_all_references(buffer *, int, int, int, int, int, int, int);
extern char *colnum_text(buffer *, int);
/* execute is no longer siod only */
#define execute(p) exec_unknown(p)
extern void va_execute(char *, ...);
extern void exec_expr(int, char *);
extern void exec_unknown(char *);

/* from fileio.c */
extern char *loader_patterns[];
extern char *saver_patterns[];
extern void register_format(int (*)(char *, buffer *),
		int (*)(char *, buffer*),
		int (*)(char *), char *);
extern void fileio_init(void);
extern int savematrix(char *, buffer *, char *);
extern int loadmatrix(char *, buffer *, char *);
extern char *guess_file_format(char *);

extern int make_backups;

/* from position.c */
extern position make_position(int, int);
extern position add_position(position, position);
extern position sub_position(position, position);
extern int same_position(position, position); /* test for equality */
extern position set_point(window * , position);
extern position get_point(window * );
extern position set_mark(window * , position);
extern position get_mark(window * );
extern position set_top(window * , position);
extern position get_top(window *);
extern void get_siod_coords(int *, int *, int *, buffer **);
extern void init_position(void);
extern int inblock(window * , position);
extern void prot_fixup(window *);
extern void set_blku_row(window *, int);
extern void set_blkl_row(window *, int);
extern void set_blku_col(window *, int);
extern void set_blkl_col(window *, int);

/* from mathwrap.c */
extern void init_mathwrap(void);

/* from fontsel.c */
extern int font_input(int *, int *);

/* global variables */
extern int recalc;
extern int macro_flag;
extern position P_MIN;		/* upper-left coordinate; set in position.c */
extern window * w_list;
extern textbuf kbd_macro;
extern int grid_lines;

#endif /* CALC_H */
