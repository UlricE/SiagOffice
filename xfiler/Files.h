/*-----------------------------------------------------------------------------
  Fm.h
  
  (c) Simon Marlow 1990-1993
  (c) Albert Graef 1994

  modified 1-29-95 by rodgers@lvs-emh.lvs.loral.com (Kevin M. Rodgers)
  to add filtering of icon/text directory displays by a filename filter.

-----------------------------------------------------------------------------*/

#ifndef FM_H
#define FM_H

#include "../config.h"

/* This code badly needs to be rewritten in C */

/* FIXME */
#ifndef HAVE_GETWD
#define getwd(s) getcwd(s,MAXPATHLEN)
#endif

/* FIXME */
/* ULTRIX apparently doesn't define these */
#ifdef ultrix
#define S_ISLNK(mode) (mode & S_IFMT) == S_IFLNK
#define S_ISSOCK(mode) (mode & S_IFMT) == S_IFSOCK
#endif

/* FIXME */
#ifdef __GNUC__
#ifndef alloca
#define alloca __builtin_alloca
#endif
#else
#if HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifdef _AIX
#pragma alloca
#else
#ifndef alloca
#define alloca malloc  /* I know we are wasting memory! :-) */
#endif
#endif
#endif
#endif

#include <stdio.h>
#include <sys/types.h> /* just in case */
#include <sys/stat.h>
#include <dirent.h>
#include <sys/param.h>

#include <Mowitz/Mowitz.h>

/* FIXME */
/* for compatibility with BSDI */
#define fnmatch xfnmatch

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024
#endif

#ifdef MAGIC_HEADERS
#ifndef MAGICTYPE_MAX
#define MAGICTYPE_MAX 128
#endif
#endif

/*--FmDirs-------------------------------------------------------------------*/

#define MAXCFGSTRINGLEN MAXPATHLEN

/* structure representing user-defined file types */
typedef struct {
  char *pattern;
#ifdef MAGIC_HEADERS
  char *magic_type;
#endif
  char *icon;
  char *push_action;
  char *drop_action;
  int len, dir;
  Pixmap icon_bm;
} TypeRec, *TypeList;

/* structure representing configured devices */
typedef struct {
  char *name;
  char *mount_action;
  char *umount_action;
  int mounted;
} DevRec, *DevList;

/* structure containing the widget ids of an icon */
typedef struct {
	Widget form, toggle, label, arrow;
} IconRec;

/* structure into which the directory information is read */
typedef struct {
	char name[FILENAME_MAX];
#ifdef MAGIC_HEADERS
	char magic_type[MAGICTYPE_MAX];
#endif
	Boolean sym_link;
	struct stat stats;
	IconRec icon;
	Boolean selected;
	TypeRec *type;
} FileRec, **FileList;

/* enumerated arguments passed to functions */
typedef enum { Files, Directories, All } FilterType;
typedef enum { SortByName, SortBySize, SortByMTime } SortType;
typedef enum { Tree, Icons, Text } DisplayType;

typedef struct _FileWindowRec {
  struct _FileWindowRec *next;
  DisplayType display_type;
  Boolean show_dirs, dirs_first, show_hidden;
  Boolean update;
  SortType sort_type;
  Widget shell, form, viewport, status, icon_box;
  Widget menubar, menubox, toolbar, toolbox, cmdform, cmdframe, cmdtext;
  Widget menuhandle, toolhandle;
  int bars;
  char directory[MAXPATHLEN];
  int dev;
  struct stat stats;
  FileList files;
  int n_files;
  int n_selections;
  long n_bytes, n_bytes_selected;
  Widget *file_items, *folder_items, *view_items, *help_items;
  Widget unreadable;
  Boolean do_filter;             /* KMR */
  char dirFilter[MAXPATHLEN];    /* KMR */
  Position x, y;
  Boolean ready;
} FileWindowRec, *FileWindowList;

#define P_READ 0x1
#define P_WRITE 0x2
#define P_EXECUTE 0x4

/* public functions */
Boolean readDirectory(FileWindowRec *fw);
void filterDirectory(FileWindowRec *fw, FilterType type);
void sortDirectory(FileList fl, int n, SortType type, Boolean dirs_first);
int permission(struct stat *stats, int perms);
void makePermissionsString(char *s, int perms);
void freeFileList(FileWindowRec *fw);
#ifdef MAGIC_HEADERS
void magic_parse_file(char *name);
void magic_get_type(char *name, char *buf);
#endif

/*--FmFw---------------------------------------------------------------------*/

#define USERS_CFG_FILE "~/.Filesrc"
#ifdef MAGIC_HEADERS
#define USERS_MAGIC_FILE "~/.FilesMagic"
#endif

extern FileWindowList file_windows;
extern Widget file_popup_widget, *file_popup_items;
extern Widget dir_popup_widget, *dir_popup_items;
extern FileWindowRec *popup_fw;

extern int n_types;
extern TypeList types;

extern int n_devices;
extern DevList devs;

int findDev(char *path);
void mountDev(int d);
void umountDev(int d);

void initFileWindows();
void createFileDisplay(FileWindowRec *fw);
void newFileWindow(String path, DisplayType format, 
			     Boolean by_cursor, Boolean iconic);
void updateFileDisplay(FileWindowRec *fw);
void reSortFileDisplay(FileWindowRec *fw);
void reDisplayFileWindow(FileWindowRec *fw);

void clearUpdateMarks();
void markForUpdate(String path);
void intUpdate();

void updateStatus(FileWindowRec *fw);

/* Braindamaged Intrinsic.h gives no way to declare a callback properly */
typedef void FmCallbackProc(Widget w, FileWindowRec *fw,
			    XtPointer call_data);

/*--FmFwCb-------------------------------------------------------------------*/

FmCallbackProc
  fileOpenCb, fileSelectAllCb, fileDeselectCb, fileTreeCb, fileIconsCb,
  fileTextCb, fileSortNameCb, fileSortSizeCb, fileSortMTimeCb, fileShowDirsCb,
  fileDirsFirstCb, fileCloseCb, mainArrowCb, fileHomeCb, fileUpCb, 
  floatingMenuCb, fileShowHiddenCb, fileEditCb, fileViewCb, fileComHereCb;
FmCallbackProc toplevelCloseCb, editorCb;
void timeoutCb(XtPointer data, XtIntervalId *id);

FmCallbackProc fileViewModeCb, fileSortModeCb, fileFindCb, updateIconsCb,
	helpContentsCb, helpAboutXfilerCb, helpAboutSiagCb;

extern void forget_icons(Display *);

/*---FmFwActions-------------------------------------------------------------*/

typedef enum { SingleFile, MultipleFiles, Executable, Directory } FileType;


typedef void FmActionProc(Widget w, XEvent *event, String *params, 
		    Cardinal *num_params);
int findWidget(Widget w, FileWindowRec **fw_ret);
int findWindow(Window w, FileWindowRec **fw_ret);
FmActionProc fileToggle, fileSelect, fileRefresh, fileOpenDir, treeOpenDir, 
			fileExecAction, fileExecFile, filePopup, dirPopup,
			fileFakeSelect, fileRestoreSelect;

void doEdit(char *directory, char *fname);
void doView(char *directory, char *fname);
extern void doImage(char *directory, char *fname);
extern void doSiaghelp(char *directory, char *fname);
void doOpenWith(char *directory, char *fname, char *with);
void doComHere(char *directory, char *fname);

/*---FmBitmaps---------------------------------------------------------------*/
/* This is so stupid! There is no reason whatsoever to hardcode these
 * constants here. Instead, the icons should be read as needed when
 * they are needed, referenced by filename.
 */

/* Cursor bitmaps */

#define FILE_CBM 0
#define FILEMSK_CBM 1
#define FILES_CBM 2
#define FILESMSK_CBM 3
#define NOENTRY_CBM 4
#define NOENTRYMSK_CBM 5
#define DIR_CBM 6
#define DIRMSK_CBM 7
#define EXEC_CBM 8
#define EXECMSK_CBM 9
#define WATCH_CBM 10
#define WATCHMSK_CBM 11

/* Tree view bitmaps */

#define LLINE_BM 12
#define TLINE_BM 13
#define FLINE_BM 14
#define CLINE_BM 15
#define LARROW_BM 16
#define RARROW_BM 17
#define WAVY_BM 18

/* Misc */

#define TICK_BM 19
#define NOTICK_BM 20
#define EXCL_BM 21
#define FILES_BM 22

/* File pixmaps */

#define DIR_BM 23
#define UPDIR_BM 24
#define FILE_BM 25
#define EXEC_BM 26
#define SYMLNK_BM 27
#define DIRLNK_BM 28
#define EXECLNK_BM 29
#define BLACKHOLE_BM 30

/* Application icons: */

#define ICON_BM 31
#define ICONMSK_BM 32
#define APPMGR_BM 33
#define APPMGRMSK_BM 34

/* Hardcoded bitmaps stop here: */

#define END_BM 35

/* Cursors */

#define FILE_CUR 0
#define FILES_CUR 1
#define NOENTRY_CUR 2
#define DIR_CUR 3
#define EXEC_CUR 4
#define WATCH_CUR 5

extern Cursor *curs;
extern Pixmap *bm;

void readBitmaps();
Pixmap readIcon(char *name);

/*--FmChmod------------------------------------------------------------------*/

void createChmodPopup();
FmCallbackProc chmodPopup;

/*--FmConfirm----------------------------------------------------------------*/

extern Boolean aborted;
void createConfirmPopup();
int confirm(String s1, String s2, String s3);

/*--FmDelete-----------------------------------------------------------------*/

FmCallbackProc deleteItems, emptyDir;

/*--FmErrors-----------------------------------------------------------------*/

void createErrorPopup();
void error(String label1, String label2);
void sysError(String label);
void abortXfm(String message);

/*--FmExec-------------------------------------------------------------------*/

typedef struct {
  String pattern, command;
} ExecMapRec;

extern ExecMapRec *exec_map;
extern int n_exec_maps;

char **makeArgv(char *action);
void freeArgv(char **argv);
void executeApplication(char *path, char *directory, char **argv);

/*--FmInfo-------------------------------------------------------------------*/

void createInfoPopup();
FmCallbackProc infoPopup;

/*--FmMain-------------------------------------------------------------------*/

/* Structure containing information about the user */
typedef struct {
  int uid, gid;
  char home[MAXPATHLEN];
  char shell[MAXPATHLEN];
  int arg0flag;
  mode_t umask;
} UserInfo;

typedef struct {
  Boolean appmgr, filemgr, version;
  String init_geometry;
	XFontStruct *icon_font, *label_font, *bold_font, *cell_font;
  int file_icon_width, file_icon_height,
    tree_icon_width, tree_icon_height;
  String cfg_file_r;
#ifdef MAGIC_HEADERS
  String magic_file_r;
#endif
  Boolean confirm_deletes, confirm_delete_folder, confirm_moves,
    confirm_copies, confirm_overwrite, confirm_quit;
  Boolean echo_actions;
  SortType default_sort_type;
  DisplayType default_display_type, initial_display_type;
  Boolean show_owner, show_perms, show_date, show_length;
  char cfg_file[MAXPATHLEN];
#ifdef MAGIC_HEADERS
  char magic_file[MAXPATHLEN];
#endif
  int double_click_time, update_interval;
  String default_editor, default_viewer, default_term, default_browser, sh_list;
  String bitmap_path, pixmap_path;
  Boolean iconic;
} Resources;

extern char *progname;
extern Resources resources;
extern XtAppContext app_context;
extern UserInfo user;
extern Widget toplevel, tooltip;

/* The following implements a semaphor for preventing the processing of update
   events in the refresh timer and the client message handler while a complex
   operation is in progress. This is a dreadful kludge, but we have to keep
   these events from modifying global data structures of the file manager
   while an operation is running. Update events which happen to get dispatched
   while the semaphor is set (for instance when an operation like file-copy
   is processing a popup form) will be simply ignored. -ag */

extern int freeze;

void quit();

/*---FmPopup-----------------------------------------------------------------*/

void createMainPopups();

FmCallbackProc selectPopup, mkdirPopup, createFilePopup, goToPopup, movePopup,
    copyPopup, linkPopup, filterPopup, openWithPopup;

/*--FmUtils------------------------------------------------------------------*/

/* structures containing information required to set up a menu */
typedef struct {
  String item_name;
  String item_label;
  FmCallbackProc *callback;
} MenuItemRec, *MenuItemList;


/* structures containing information required to set up a button */
typedef struct {
  String button_name;
  String button_label;
  FmCallbackProc *callback;
} ButtonRec, *ButtonList;


/* structure for creating a popup questionaire */
typedef struct {
  String label;
  String value;
  Cardinal length;
  Widget widget;
} QuestionRec, *QuestionList;

/* functions */

void initUtils();

Widget *createMenu(String menu_name, String menu_label, MenuItemList items,
		   Cardinal n_items, Dimension left_margin, Widget parent,
		   XtPointer client_data);
Widget *createButtons(ButtonList buttons, Cardinal n_buttons, Widget parent,
		      XtPointer client_data);
Widget createPopupQuestions(String name, String title, Pixmap bitmap, 
			    QuestionList questions, Cardinal n_questions,
			    ButtonList buttons, Cardinal n_buttons);
void fillIn(Widget w);
void grayOut(Widget w);
void tick(Widget w);
void noTick(Widget w);
void popupByCursor(Widget shell, XtGrabKind grab_kind);
void zzz(void), wakeUp(void);
Widget *createFloatingMenu(String menu_name,
			   MenuItemRec *items, Cardinal n_items, 
			   Dimension left_margin, Widget parent, 
			   XtPointer client_data,
			   Widget *menu_widget);
char *varPopup(Pixmap icon_bm, char *action);

/*--FmComms------------------------------------------------------------------*/

extern Atom xfm_open_window, xfm_update_window, wm_delete_window,
    wm_protocols;
Window FindFilesWin (Display *dpy, Window win);
void openRemoteDirs(Display *dpy, Window files, int argc, char **argv);
void clientMessageHandler(Widget w, XtPointer closure, XEvent *e);
void initComms(void);

/*--FmOps--------------------------------------------------------------------*/

char *split(char *s, char c);
char *expand(char *s, char *t, char *c);
char *strparse(char *s, char *t, char *c);
int fnmatch(String pattern, String name);
char *fnexpand(char *fn);
int prefix(char *s, char *t);
int exists(char *path);
char *searchPath(char *s1, char *p, char *s2);

int create(char *path, mode_t mode), rcopy(char *oldpath, char *newpath),
  rdel(char *path);

/*--FmDnd--------------------------------------------------------------------*/
void RootDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b);
void DragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b);
void DropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b);
void IconBoxDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b);


/*---------------------------------------------------------------------------*/

/* Horrible kludge to avoid warnings, as XtFree is designed to take a (char *)*/
#define XTFREE(p) XtFree((void *)(p))
#define XTREALLOC(p,n) XtRealloc((void *)(p),(n))

#endif
