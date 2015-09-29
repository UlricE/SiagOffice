/*

   Copyright (c) 1994  Randolf Werner

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the X Consortium shall
   not be used in advertising or otherwise to promote the sale, use or
   other dealings in this Software without prior written authorization
   from the X Consortium.

 */

/*******************************************************************
 * Global header "xedit.h" 					   *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *                                                                 *
 * Because some stuff is taken from the "xedit" programm here      *
 * comes the copyright message of "xedit"                          *
 *******************************************************************/
/*
 *                      COPYRIGHT 1987
 *                 DIGITAL EQUIPMENT CORPORATION
 *                     MAYNARD, MASSACHUSETTS
 *                      ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Digital Equipment Corporation not be 
 * used in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 */

#include <stdio.h>
#ifndef SVR4
#include <strings.h>
#else
#include <string.h>
#define rindex(s,c)	(strrchr(s,c))
#define index(s,c)	(strchr(s,c))
#endif
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Cardinals.h>

#include <Mowitz/Mowitz.h>

#include <X11/Xaw/Toggle.h>

#ifndef L_tmpnam
#define L_tmpnam	1024
extern char *tmpnam();
#endif

#define NORESULT -1
#define XED_ABORT 0
#define CONTINUE 1
#define OK 0l
#define CANCEL 1l
#define ALL 2l
#define NO  3l
#define YES 4l
#define REPLACE 0
#define REPLACE_VETO 1
#define REPLACE_ALL 2
#define L_xedtmpnam 256

extern struct _app_resources {
	Boolean enableBackups;
	char *backupNameSuffix;
	char *printCommand;
	Boolean autoIndent;
	int tabsize;
	int maxScrollbreak;
	char *commands;
	char *pipes;
	int textwidth;
	int textheight;
	char *overwritetranslations;
	char *inserttranslations;
} app_resources;

typedef enum {
	NO_READ, READ_OK, WRITE_OK, CREATE_OK
} FileAccess;

/*      externals in xedit.c    */
extern void Feep();
extern void set_wm_delete();

/* externals in jump.c */
extern void DoJumpLine();
extern void DoJumpBegin();
extern void DoJumpEnd();
extern void DoJumpSelStart();
extern void DoJumpSelEnd();
extern void goto_line();
extern void goto_line2();
extern void show_position();
extern void jumpline_close();

/* externals in file.c */
extern void load_file();
extern void DoFileLoad();
extern void DoFileInsert();
extern void DoFileSave();
extern void DoFileSaveAs();
extern void DoFileSaveSelection();
extern void DoFilePrint();
extern void DoFilePrintSelection();
extern void DoFileQuit();
extern void ResetSourceChanged();
extern void SourceChanged();
extern Boolean IsSourceChanged();
extern FileAccess CheckFilePermissions();
extern char *xed_tmpnam();

/* externals in edit.c */
#ifdef SCROLLBREAK
extern void cursor_up();
extern void cursor_down();
#endif
extern void DoEditCut();
extern void DoEditCopy();
extern void DoEditPaste();
extern void DoEditShiftSelRight();
extern void DoEditShiftSelLeft();

/* externals in warning.c */
extern void error();
extern void error_ready();
extern int warning();
extern void warn_ready();

/* externals in search.c */
extern void DoSearchSearch();
extern void DoSearchSearchselection();
extern void DoSearchReplace();
extern void DoSearchReplaceselection();
extern void DoSearchFindbracket();
extern void DoSearchCheckbrackets();
extern void search();
extern void replace();
extern void veto_ready();
extern void search_ready();

/* externals in special.c */
extern void set_tabsize();
extern void DoSpezialOptions();
extern void DoSpezialCallSed();
extern void DoFileNew();
extern void DoSpezialHelp();
extern void DoSpezialAbout();
extern void DoSpezialAboutXedplus();
extern void DoSpezialAboutSiagOffice();
extern void option_ready();
extern void autoindent();
extern void xedskiplineend();
extern void xedtoggleoverwrite();
extern void sed_do();
extern void sed_do_sel();
extern void sed_undo();
extern void sed_close();
extern void help_close();
extern void about_close();

/* externals in commands.c */
extern void DoCommand0();
extern void DoPipe0();
extern void command_do();
extern void command_close();
extern void command_exec();
extern void pipe_do();
extern void pipe_close();
extern void pipe_exec();

/* externals in file_sel.c */
extern char *file_select();
extern Widget init_file_select();
extern void centerpopup();
extern void getdirectory();

/* externals in dnd.c */
extern void FileDragHandler(Widget, XtPointer, XEvent *, Boolean *);
extern void FileDropHandler(Widget, XtPointer, XEvent *, Boolean *);
