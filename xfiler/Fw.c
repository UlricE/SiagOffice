/*-----------------------------------------------------------------------------
  Module FmFw.c                                                             

  (c) Simon Marlow 1991
  (c) Albert Graef 1994
                                                                           
  functions & data for creating a file window, and various functions        
  related to file windows                                                   

  modified 1-29-95 by rodgers@lvs-emh.lvs.loral.com (Kevin M. Rodgers)
  to add filtering of icon/text directory displays by a filename filter.

-----------------------------------------------------------------------------*/

#include <pwd.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#ifdef _AIX
#include <sys/resource.h>
#endif

#include <sys/wait.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>
#include <Mowitz/Mowitz.h>

#include <X11/xpm.h>

#include "Files.h"

#include "../common/common.h"
#include <Mowitz/Mowitz.h>
#include "../xcommon/xcommon.h"

#define FW_WIDTH 400
#define FW_HEIGHT 300
#define TEXT_PADDING 10
#define MAXCFGLINELEN 1024

#define MENUBAR (1)
#define TOOLBAR (2)
#define FORMATBAR (4)

/*-----------------------------------------------------------------------------
  PUBLIC DATA                                       
-----------------------------------------------------------------------------*/

FileWindowList file_windows = NULL;
FileWindowRec *popup_fw;
Widget file_popup_widget, *file_popup_items;
Widget dir_popup_widget, *dir_popup_items;
Widget tooltip;

int n_types;
TypeList types;

int n_devices;
DevList devs;

/*-----------------------------------------------------------------------------
  STATIC DATA                                       
-----------------------------------------------------------------------------*/

static MenuItemRec file_popup_menu[] = {
  { "edit", "Edit", fileEditCb },
  { "view", "View", fileViewCb },
  { "open_with", "Open With...", openWithPopup },
  { "line1", NULL, NULL },
  { "move", "Move...", movePopup },
  { "copy", "Copy...", copyPopup },
  { "link", "Link...", linkPopup },
  { "line2", NULL, NULL },
  { "delete", "Delete", deleteItems },
  { "line3", NULL, NULL },
  { "info", "Information...", infoPopup },
  { "chmod", "Permissions...", chmodPopup }
};

static MenuItemRec dir_popup_menu[] = {
  { "open", "Open", fileOpenCb },
  { "line1", NULL, NULL },
  { "move", "Move...", movePopup },
  { "copy", "Copy...", copyPopup },
  { "link", "Link...", linkPopup },
  { "line2", NULL, NULL },
  { "delete", "Delete", deleteItems },
  { "line3", NULL, NULL },
  { "comHere", "Command Window", fileComHereCb },
  { "line4", NULL, NULL },
  { "info", "Information...", infoPopup },
  { "chmod", "Permissions...", chmodPopup }
};

 static MenuItemRec file_menu[] = {
  { "new", "New...", createFilePopup },
  { "line1", NULL, NULL },
  { "move", "Move...", movePopup },
  { "copy", "Copy...", copyPopup },
  { "link", "Link...", linkPopup },
  { "line2", NULL, NULL },
  { "delete", "Delete",  deleteItems },
  { "line3", NULL, NULL },
  { "select", "Select...", selectPopup },
  { "select all", "Select all", fileSelectAllCb },
  { "deselect all", "Deselect all", fileDeselectCb },
  { "line4", NULL, NULL },
  { "quit", "Quit", toplevelCloseCb },
};

static MenuItemRec folder_menu[] = {
  { "new", "New...", mkdirPopup },
  { "line1", NULL, NULL },
  { "goto", "Go to...", goToPopup },
  { "home", "Home", fileHomeCb },
  { "up", "Up", fileUpCb },
  { "line2", NULL, NULL },
  { "empty", "Empty", emptyDir },
  { "line3", NULL, NULL },
  { "comHere", "Command", fileComHereCb },
  { "line4", NULL, NULL },
  { "close", "Close", fileCloseCb },
};

static MenuItemRec view_menu[] = {
  { "tree", "Tree",  fileTreeCb },
  { "icons", "Icons",  fileIconsCb },
  { "text", "Text",  fileTextCb },
  { "line1", NULL,  NULL },
  { "sort by name", "Sort by name",  fileSortNameCb },
  { "sort by size", "Sort by size",  fileSortSizeCb },
  { "sort by mtime", "Sort by date",  fileSortMTimeCb },
  { "line2", NULL,  NULL },
  { "filter", "Filter...", filterPopup },          /* KMR */
  { "line3", NULL,  NULL },
  { "hide folders", "Hide folders",  fileShowDirsCb },
  { "mix folders/files", "Mix folders/files",
       fileDirsFirstCb },
  { "show hidden files", "Show hidden files", fileShowHiddenCb },
};

static MenuItemRec help_menu[] = {
	{ "contents", "Contents", helpContentsCb },
	{ "line1", NULL, NULL },
	{ "aboutxfiler", "About Xfiler...", helpAboutXfilerCb },
	{ "aboutsiag", "About Siag Office...", helpAboutSiagCb },
};

/*-----------------------------------------------------------------------------
  Widget Argument Lists
-----------------------------------------------------------------------------*/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) NULL },
  { XtNiconPixmap, (XtArgVal) NULL },
  { XtNiconMask, (XtArgVal) NULL }
};

static Arg label_args[] = {
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) FW_WIDTH },
  { XtNfont, (XtArgVal) NULL },
  { XtNresize, (XtArgVal) False },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight },
  { XtNtranslations, (XtArgVal) NULL },
};

static Arg icon_box_args[] = {
  { XtNwidth, (XtArgVal) 0 },
  { XtNtranslations, (XtArgVal) NULL }
};

static Arg tree_box_args[] = {
  { XtNwidth, (XtArgVal) 1 },
  { XtNdefaultDistance, (XtArgVal) 0 },
  { XtNheight, (XtArgVal) 1 }
};

static Arg icon_form_args[] = {
  { XtNdefaultDistance, (XtArgVal) 0 },
  { XtNwidth, (XtArgVal) 0 }
};

static Arg tree_form_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNdefaultDistance, (XtArgVal) 0 },
  { XtNwidth, (XtArgVal) 0 },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

static Arg icon_toggle_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNheight, (XtArgVal) 0 }
};

static Arg icon_label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 },
};

static Arg text_label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNjustify, XtJustifyLeft },
  { XtNhorizDistance, (XtArgVal) TEXT_PADDING },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

static Arg text_toggle_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNfont, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) 0 },
  { XtNtranslations, (XtArgVal) NULL },
  { XtNjustify, XtJustifyLeft },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

static Arg arrow_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNsensitive, (XtArgVal) True },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 },
  { XtNhighlightThickness, (XtArgVal) 0 }
};

static Arg line_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNbitmap, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft },
  { XtNinternalWidth, (XtArgVal) 0 },
  { XtNinternalHeight, (XtArgVal) 0 }
};

/*-----------------------------------------------------------------------------
  Translation tables
-----------------------------------------------------------------------------*/

static char label_translations_s[] = "\
  :<Btn1Up>(2)         : fileRefresh()\n";

static char tree_translations_s[] = "\
  Shift<Btn1Down>,<Btn1Up>: fileToggle()\n\
  :<Btn1Down>,<Btn1Up> : fileSelect()\n\
  <Btn2Down>,<Btn2Up> : fileToggle()\n\
  <Btn3Down>          : dirPopup()\n";


static char dir_translations_s[] = "\
  :<Btn1Up>(2)         : fileOpenDir()\n\
  Shift<Btn1Down>,<Btn1Up>: fileToggle()\n\
  :<Btn1Down>,<Btn1Up> : fileSelect()\n\
  <Btn2Down>,<Btn2Up> : fileToggle()\n\
  <Btn3Down>          : dirPopup()\n";

static char file_translations_s[] = "\
  :<Btn1Up>(2)         : fileExecAction()\n\
  Shift<Btn1Down>,<Btn1Up>: fileToggle()\n\
  :<Btn1Down>,<Btn1Up> : fileSelect()\n\
  <Btn2Down>,<Btn2Up> : fileToggle()\n\
  <Btn3Down>          : filePopup()\n";

static char exec_translations_s[] = "\
  :<Btn1Up>(2)         : fileExecFile()\n\
  Shift<Btn1Down>,<Btn1Up>: fileToggle()\n\
  :<Btn1Down>,<Btn1Up> : fileSelect()\n\
  <Btn2Down>,<Btn2Up> : fileToggle()\n\
  <Btn3Down>          : filePopup()\n";


/* This is a hack to get the icon box to recognise button events */
static char iconbox_translations_s[] = "\
    <Btn2Up> : dummy()\n\
    <Btn3Up> : dummy()\n";
  
static void dummy(Widget w, XEvent *event, String *params, 
		       Cardinal *num_params) {}

/* ---
Command line history. We need these indices:
cmd_count	The number of stored commands
cmd_curr	The entry we are currently editing
--- */

#define CMD_MAX 10

static char *cmd_history[CMD_MAX];
static int cmd_count, cmd_curr;

static void cmd_shift(void)
{
	int i;

	MwFree(cmd_history[0]);
	for (i = 1; i < cmd_count; i++)
		cmd_history[i-1] = cmd_history[i];
	cmd_history[i-1] = NULL;
	cmd_count--;
}

static void cmd_store(char *p)
{
	if (cmd_count >= CMD_MAX) cmd_shift();
	cmd_history[cmd_count++] = MwStrdup(p);
}

static char *cmd_get(int n)
{
	if (n < 0) n = 0;
	if (n > cmd_count) n = cmd_count;
	cmd_curr = n;
	if (n == cmd_count) return "";
	return cmd_history[n];
}

static FileWindowRec *find_window_by_widget(Widget w)
{
	FileWindowRec *fw;
	for (fw = file_windows; fw; fw = fw->next) {
		if (w == fw->shell || w == fw->form
		    || w == fw->viewport || w == fw->status
		    || w == fw->icon_box || w == fw->menubar
		    || w == fw->menubox || w == fw->toolbar
		    || w == fw->toolbox || w == fw->cmdform
		    || w == fw->cmdframe || w == fw->cmdtext
		    || w == fw->menuhandle || w == fw->toolhandle) {
			return fw;
		}
	}
	return NULL;
}

static void cmdline_run(Widget w, XEvent *e, String *p, Cardinal *n)
{
	String string = MwTextFieldGetString(w);

	/* This is a hack of the worst kind, allowing the user to invoke the
	   default action of a file just by pressing Enter when the command
	   line is empty.
	*/
	if (string[0] == '\0') {
		int i;
		FileWindowRec *fw = find_window_by_widget(w);
		if (fw == NULL) {
			return;
		}
		for (i = 0; i < fw->n_files; i++) {
			if (fw->files[i]->selected)
			break;
		}
		if (i < fw->n_files) {
			int l = strlen(fw->directory);
			char *path = MwMalloc(l+strlen(fw->files[i]->name)+2);
			strcpy(path, fw->directory);
			if (path[l-1] != '/') path[l++] = '/';
			strcpy(path+l, fw->files[i]->name);

			if (S_ISDIR(fw->files[i]->stats.st_mode)) {
				fileOpenDir(fw->files[i]->icon.toggle,
					NULL, NULL, NULL);
			} else if (S_ISREG(fw->files[i]->stats.st_mode)) {
				if (fw->files[i]->stats.st_mode &
				    (S_IXUSR | S_IXGRP | S_IXOTH)) {
					fileExecFile(fw->files[i]->icon.toggle,
						NULL, NULL, NULL);
				} else {
					fileExecAction(fw->files[i]->icon.toggle,
						NULL, NULL, NULL);
				}
			}
		}
		return;
	}

	cmd_store(string);
	cmd_curr = cmd_count;
	if (!fork()) {
		char *argv[4];
		argv[0] = "runcmd";
		argv[1] = "/bin/sh";
		argv[2] = "-c";
		argv[3] = MwStrdup(string);
		argv[4] = NULL;
		execvp(argv[0], argv);
		_exit(0);	/* shouldn't get here */
	}
	string = "";
	MwTextFieldSetString(w, string);
}

static void cmdline_cancel(Widget w, XEvent *e, String *p, Cardinal *n)
{
	String string = "";
	MwTextFieldSetString(w, string);
	cmd_curr = cmd_count;
}

static void cmdline_previous(Widget w, XEvent *e, String *p, Cardinal *n)
{
	String string = cmd_get(cmd_curr-1);
	MwTextFieldSetString(w, string);
}

static void cmdline_next(Widget w, XEvent *e, String *p, Cardinal *n)
{
	String string = cmd_get(cmd_curr+1);
	MwTextFieldSetString(w, string);
}

static void cmdline_tab(Widget w, XEvent *e, String *p, Cardinal *n)
{
	int i;
	FileWindowRec *fw = find_window_by_widget(w);
	if (fw == NULL) {
		return;
	}
	for (i = 0; i < fw->n_files; i++) {
		if (fw->files[i]->selected)
		break;
	}
	if (i < fw->n_files) {
		if (p[0][0] == 'l' && i > 0) {				/* left */
			fileToggle(fw->files[i]->icon.toggle, NULL, NULL, 0);
			fileSelect(fw->files[i-1]->icon.toggle, NULL, NULL, 0);
		} else if (p[0][0] == 'r' && i < (fw->n_files-1)) {	/* right */
			fileToggle(fw->files[i]->icon.toggle, NULL, NULL, 0);
			fileSelect(fw->files[i+1]->icon.toggle, NULL, NULL, 0);
		}
	} else if (p[0][0] == 'l') {
		fileSelect(fw->files[fw->n_files-1]->icon.toggle, NULL, NULL, 0);
	} else if (p[0][0] == 'r') {
		fileSelect(fw->files[0]->icon.toggle, NULL, NULL, 0);
	}
}

static void cmdline_copy(Widget w, XEvent *e, String *p, Cardinal *n)
{
#if 0
	FileWindowRec *fw = find_window_by_widget(w);
#endif
}

static void cmdline_cut(Widget w, XEvent *e, String *p, Cardinal *n)
{
#if 0
	FileWindowRec *fw = find_window_by_widget(w);
#endif
}

static void cmdline_paste(Widget w, XEvent *e, String *p, Cardinal *n)
{
#if 0
	FileWindowRec *fw = find_window_by_widget(w);
#endif
}

static void cmdline_quit(Widget w, XEvent *e, String *p, Cardinal *n)
{
	FileWindowRec *fw = find_window_by_widget(w);
	toplevelCloseCb(w, fw, NULL);
}

static void cmdline_close(Widget w, XEvent *e, String *p, Cardinal *n)
{
	FileWindowRec *fw = find_window_by_widget(w);
	fileCloseCb(w, fw, NULL);
}

static void cmdline_update(Widget w, XEvent *e, String *p, Cardinal *n)
{
	FileWindowRec *fw = find_window_by_widget(w);
	updateIconsCb(w, fw, NULL);
}

static void cmdline_new(Widget w, XEvent *e, String *p, Cardinal *n)
{
	FileWindowRec *fw = find_window_by_widget(w);
	newFileWindow(fw->directory, resources.default_display_type,
		False, False);
}

static XtActionsRec file_actions[] = {
  { "fileRefresh", fileRefresh },
  { "fileToggle", fileToggle },
  { "fileSelect", fileSelect },
  { "fileOpenDir", fileOpenDir },
  { "fileExecFile", fileExecFile },
  { "fileExecAction", fileExecAction },
  { "filePopup", filePopup },
  { "dirPopup", dirPopup },
  { "dummy", dummy },
  { "cmdline-run", cmdline_run },
  { "cmdline-cancel", cmdline_cancel },
  { "cmdline-previous", cmdline_previous },
  { "cmdline-next", cmdline_next },
  { "cmdline-tab", cmdline_tab },
  { "cmdline-update", cmdline_update },
  { "cmdline-copy", cmdline_copy },
  { "cmdline-cut", cmdline_cut },
  { "cmdline-paste", cmdline_paste },
  { "cmdline-close", cmdline_close },
  { "cmdline-quit", cmdline_quit },
  { "cmdline-new", cmdline_new },
};

static XtTranslations label_translations, dir_translations, file_translations, 
  iconbox_translations, tree_translations, exec_translations;

/*-----------------------------------------------------------------------------
  PRIVATE FUNCTIONS
-----------------------------------------------------------------------------*/


static XtCallbackProc updateXY( Widget w, XtPointer client_data,
								XtPointer call_data )
{
  XawPannerReport *rep;
  FileWindowRec *fw;

  rep = (XawPannerReport *)call_data;
  fw = (FileWindowRec *)client_data;

  fw->x = rep->slider_x;
  fw->y = rep->slider_y;

  /* This is to tell updateFileDisplay that it's okay to adjust the
	 x,y coords now without fear of coredumping. */
  fw->ready = True;

	return 0;
}

static int longestName(FileWindowRec *fw)
{
  int i,l;
  int longest = 0;

  for (i=0; i<fw->n_files; i++)
    if ((l = XTextWidth(resources.icon_font, fw->files[i]->name, 
			strlen(fw->files[i]->name))) > longest)
      longest = l;
  return longest;
}

/*---------------------------------------------------------------------------*/

static int parseType(FILE *fp, char **pattern, 
#ifdef MAGIC_HEADERS
                 char **magic_type,
#endif
				 char **icon, char **push_action, char **drop_action)
{
  static char s[MAXCFGLINELEN];
  int l;

 start:
  if (feof(fp)||!fgets(s, MAXCFGLINELEN, fp))
    return 0;
  if(!strncmp(s, "# Device section", 16))
	return 0;
  l = strlen(s);
  if (s[l-1] == '\n')
    s[--l] = '\0';
  if (!l || *s == '#')
    goto start;
  if (!(*pattern = split(s, ':')))
    return -1;
#ifdef MAGIC_HEADERS
  if (**pattern == '<') {
    char *ptr;
    ptr = *pattern + 1;
    while(*ptr && (*ptr != '>' || ptr[-1] == '\\'))
	ptr++;
    if(*ptr != '>')
	return -1;
    *ptr = '\0';
    *magic_type = *pattern + 1;
    *pattern = ptr + 1;
  }
  else
    *magic_type = NULL;
#endif
  if (!(*icon = split(NULL, ':')))
    return -1;
  if (!(*push_action = split(NULL, ':')))
    return -1;
  if (!(*drop_action = split(NULL, ':')))
    return -1;
  return l;
}

/*---------------------------------------------------------------------------*/

static void readFileBitmaps()
{
  int i;

  for (i=0; i<n_types; i++)
    if (!types[i].icon[0])
      types[i].icon_bm = bm[FILE_BM];
     else if ((types[i].icon_bm = readIcon(types[i].icon)) == None) {
#ifdef MAGIC_HEADERS
      fprintf(stderr, "%s: can't read icon for type %s%s%s%s%s%s\n", progname,
	      types[i].magic_type?"<":"",
	      types[i].magic_type?types[i].magic_type:"",
	      types[i].magic_type?">":"",
	      types[i].dir<0?"*":"", types[i].pattern,
	      types[i].dir>0?"*":"");
#else
      fprintf(stderr, "%s: can't read icon for type %s%s%s\n", progname,
	      types[i].dir<0?"*":"", types[i].pattern,
	      types[i].dir>0?"*":"");
      types[i].icon_bm = bm[FILE_BM];
#endif
    }
}

/*---------------------------------------------------------------------------*/

static void readFileTypes(String path)
{
  FILE *fp;
  char *pattern, *icon, *push_action, *drop_action;
#ifdef MAGIC_HEADERS
  char *magic_type;
#endif
  char s[MAXCFGSTRINGLEN];
  int i, l, p;
  
  n_types = 0;
  types = NULL;

  if (!(fp = fopen(path, "r"))) return;

  for (i=0; (p = parseType(fp, &pattern, 
#ifdef MAGIC_HEADERS
                           &magic_type,
#endif
						   &icon, &push_action,
			   &drop_action)) > 0; i++) 
  {
    types = (TypeList) XTREALLOC(types, (i+1)*sizeof(TypeRec) );
    l = strlen(pattern);
    if (pattern[0] == '*') {
      types[i].dir = -1;
#ifdef MAGIC_HEADERS
      strparse(s, pattern+1, "\\:<>");
#else
      strparse(s, pattern+1, "\\:");
#endif
    } else if (pattern[l-1] == '*') {
      types[i].dir = 1;
      pattern[l-1] = '\0';
#ifdef MAGIC_HEADERS
      strparse(s, pattern, "\\:<>");
#else
      strparse(s, pattern, "\\:");
#endif
    } else {
      types[i].dir = 0;
#ifdef MAGIC_HEADERS
      strparse(s, pattern, "\\:<>");
#else
      strparse(s, pattern, "\\:");
#endif
    }
    types[i].len = strlen(s);
    types[i].pattern = XtNewString(s);
#ifdef MAGIC_HEADERS
    if(magic_type)
      types[i].magic_type = XtNewString(strparse(s, magic_type, "\\:"));
    else
      types[i].magic_type = NULL;
#endif
    types[i].icon = XtNewString(strparse(s, icon, "\\:"));
    types[i].push_action = XtNewString(strparse(s, push_action, "\\:"));
    types[i].drop_action = XtNewString(strparse(s, drop_action, "\\:"));
  }

  if (p == -1)
    error("Error in configuration file", "");

  n_types = i;
  
  if (fclose(fp))
    sysError("Error reading configuration file:");

  readFileBitmaps();
}

/*---------------------------------------------------------------------------*/

#ifdef MAGIC_HEADERS
static TypeRec *fileType(char *name, char *magic_type)
#else
static TypeRec *fileType(char *name)
#endif
{
  int i, l = strlen(name);

  for (i = 0; i < n_types; i++){
#ifdef MAGIC_HEADERS
    if (types[i].magic_type) {
      if(strcmp(types[i].magic_type, magic_type))
        continue;
      else if (!strcmp(types[i].pattern, "")) /* Empty pattern. */
        return types+i;
    }
#endif	  
    switch (types[i].dir) {
    case 0:
      if (!strcmp(name, types[i].pattern))
	return types+i;
      break;
    case 1:
      if (!strncmp(types[i].pattern, name, types[i].len))
	return types+i;
      break;
    case -1:
      if (l >= types[i].len && !strncmp(types[i].pattern, name+l-types[i].len,
					types[i].len))
	return types+i;
      break;
    }
  }	
  return NULL;
}

/*---------------------------------------------------------------------------*/

static int parseDev(FILE *fp, char **name, char **mount_action,
		    char **umount_action)
{
  static char s[MAXCFGLINELEN];
  static int flag_device_section = False;
  int l;
	
	if (flag_device_section)
	  	goto start;
	
	while(!feof(fp) && fgets(s, MAXCFGLINELEN, fp))
	{
		if (!strncmp(s, "# Device section", 16))
		{			
			flag_device_section = True;			
			goto start;
		}		
	}
	/* no device section */	
    return 0; 	
	
 start:
  if (feof(fp)||!fgets(s, MAXCFGLINELEN, fp))
    return 0;
  l = strlen(s);
  if (s[l-1] == '\n')
    s[--l] = '\0';
  if (!l || *s == '#')
    goto start;
  if (!(*name = split(s, ':')))
    return -1;
  if (!(*mount_action = split(NULL, ':')))
    return -1;
  if (!(*umount_action = split(NULL, ':')))
    return -1;
  return l;
}

/*---------------------------------------------------------------------------*/

static void readDevices(String path)
{
  FILE *fp;
  char *name, *mount_action, *umount_action;
  char s[MAXCFGSTRINGLEN];
  int i, p;
  
  n_devices = 0;
  devs = NULL;
  
  if (!(fp = fopen(path, "r"))) return;

  for (i=0; (p = parseDev(fp, &name, &mount_action, &umount_action)) > 0;
       i++) {
    devs = (DevList) XTREALLOC(devs, (i+1)*sizeof(DevRec) );
    devs[i].name = XtNewString(strparse(s, name, "\\:"));
    devs[i].mount_action = XtNewString(strparse(s, mount_action, "\\:"));
    devs[i].umount_action = XtNewString(strparse(s, umount_action, "\\:"));
    devs[i].mounted = 0;
  }

  if (p == -1)
    error("Error in devices file", "");

  n_devices = i;
  
  if (fclose(fp))
    sysError("Error reading devices file:");
}

/*---------------------------------------------------------------------------*/

static int devAction(int d, char *action)
{
  int pid, status;

  if ((pid = fork()) == -1) {
    sysError("Can't fork:");
    return 0;
  } else if (chdir(user.home)) {
    sysError("Can't chdir:");
    return 0;
  } else if (!pid) {
    if (resources.echo_actions)
      fprintf(stderr, "%s\n", action);
    freopen("/dev/null", "r", stdin);
    if (user.arg0flag)
      execlp(user.shell, user.shell, "-c", action, user.shell, NULL);
    else
      execlp(user.shell, user.shell, "-c", action, NULL);
    perror("Exec failed");
    exit(1);
  } else if (waitpid(pid, &status, 0) == -1 || !WIFEXITED(status) ||
	     WEXITSTATUS(status))
    return 0;
  else
    return 1;
}

typedef struct pm_list {
	char *name;
	Pixmap pm;
	struct pm_list *next;
} pm_list;

static pm_list *pm_cache = NULL;

void forget_icons(Display *dpy)
{
	pm_list *pl = pm_cache;
	while (pm_cache) {
		pl = pm_cache;
		pm_cache = pl->next;
		MwFree(pl->name);
		XFreePixmap(dpy, pl->pm);
		MwFree(pl);
	}
}

static Pixmap load_custom_icon(Widget w, char *name)
{
	Pixmap pm;
	char b[1024], c[1024];
	struct stat statbuf;
	pm_list *pl;
	Pixel color;

	getcwd(b, 1000);
	/* new method, place files in ~/.siag/.xfiler/`pwd`/filename.xpm */
	sprintf(c, "%s/.xfiler%s/%s.xpm", siag_basedir, b, name);
	for (pl = pm_cache; pl; pl = pl->next) {
		if (pl->name && !strcmp(pl->name, c)) {
			return pl->pm;
		}
	}
	if (stat(c, &statbuf)) return None;
	XtVaGetValues(w, XtNbackground, &color, (char *)0);
	pm = load_pixmap(XtDisplay(w), color, c);
	if (pm) {
		pl = (pm_list *)malloc(sizeof(pm_list));
		pl->pm = pm;
		pl->name = MwStrdup(c);
		pl->next = pm_cache;
		pm_cache = pl;
	}
	return pm;
}

/*---------------------------------------------------------------------------*/

static void createFileIcons(FileWindowRec *fw)
{
  int i;
  Dimension width;
  FileRec *file;
  Pixmap icon;
  char *iconname[4] = {"dir_icon","file_icon","exe_icon","other_icon"};
  int icontype;

  XtVaGetValues(fw->viewport, XtNwidth, &width, NULL);
  icon_box_args[0].value = (XtArgVal) width;

  width = longestName(fw);
  if (width < resources.file_icon_width)
    width = resources.file_icon_width;
  icon_form_args[1].value = (XtArgVal) width;
  icon_toggle_args[4].value = (XtArgVal) width;
  icon_toggle_args[5].value = (XtArgVal) resources.file_icon_height;
  icon_label_args[4].value = (XtArgVal) width;

  fw->icon_box = XtCreateWidget("icon box",  boxWidgetClass,
    fw->viewport, icon_box_args, XtNumber(icon_box_args) );
	/* register icon_box as a "droppable" */
	MwDndRegisterDropWidget(fw->icon_box, IconBoxDropEventHandler, NULL);

  for (i=0; i < fw->n_files; i++) {
    Pixel back;
    
    file = fw->files[i];
    file->icon.form = XtCreateManagedWidget(file->name,
      formWidgetClass, fw->icon_box, icon_form_args,
      XtNumber(icon_form_args) );

#ifdef MAGIC_HEADERS
    /* determine file type first, to allow special items like directories to
       have custom icons */
    file->type = fileType(file->name, file->magic_type);
    if (file->type)
      icon = (XtArgVal) file->type->icon_bm;
    else
      icon = None;
#else
    icon = None;
#endif
    icontype = 3;

    /* Symbolic link to non-existent file */
    if (S_ISLNK(file->stats.st_mode)) {
      icon_toggle_args[3].value = (XtArgVal) file_translations;
      if (icon == None)
        icon = (XtArgVal) bm[BLACKHOLE_BM];
    }
    else if (S_ISDIR(file->stats.st_mode)) {
      icon_toggle_args[3].value = (XtArgVal) dir_translations;
      if (icon == None) {
	if (file->sym_link) {
	  icon = (XtArgVal) bm[DIRLNK_BM];
	} else if (!strcmp(file->name, "..")) {
	  icon = (XtArgVal) bm[UPDIR_BM];
        } else {
	  icon = (XtArgVal) bm[DIR_BM];
	}
      }
      icontype = 0;
    }
    else if (file->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) {
      icon_toggle_args[3].value = (XtArgVal) exec_translations;
      if (icon == None) {
	if (file->sym_link) {
	  icon = (XtArgVal) bm[EXECLNK_BM];
	} else {
	  icon = (XtArgVal) bm[EXEC_BM];
	}
      }
      icontype = 2;
    }
    else {
      icon_toggle_args[3].value = (XtArgVal) file_translations;
#ifdef MAGIC_HEADERS
      /* Already got file->type. */
#else
      file->type = fileType(file->name);
#endif

      /* This is our opportunity to load a custom icon /Ulric */
      icon = load_custom_icon(file->icon.form, file->name);

      if (icon == None) {
        if (file->type) {
	  icon = (XtArgVal) file->type->icon_bm;
	} else if (file->sym_link) {
	  icon = (XtArgVal) bm[SYMLNK_BM];
	} else {
	  icon = (XtArgVal) bm[FILE_BM];
	}
      icontype = 1;
      }
    }
    icon_toggle_args[2].value = icon;

    file->icon.toggle = XtCreateManagedWidget(iconname[icontype],
      toggleWidgetClass, file->icon.form, icon_toggle_args,
      XtNumber(icon_toggle_args) );
    /* Register Widget for Dnd */
    MwDndRegisterDragWidget(file->icon.toggle, DragEventHandler, NULL);
    MwDndRegisterDropWidget(file->icon.toggle, DropEventHandler, NULL);
	  
    XtVaGetValues(file->icon.toggle, XtNbackground, &back, NULL);
    XtVaSetValues(file->icon.toggle, XtNborder, (XtArgVal) back, NULL);

    icon_label_args[0].value = (XtArgVal) NULL;
    icon_label_args[1].value = (XtArgVal) file->icon.toggle;
    icon_label_args[2].value = (XtArgVal) file->name;
    file->icon.label = XtCreateManagedWidget("label",
      labelWidgetClass, file->icon.form, icon_label_args,
      XtNumber(icon_label_args) );
  }
}

/*----------------------------------------------------------------------------*/

static void createTextDisplay(FileWindowRec *fw)
{
  int i, l;
  Widget w;
  Dimension width, m_width, name_w, size_w, perm_w, own_w = 0, date_w;
  char s[10], name[FILENAME_MAX];
  struct passwd *pw;
  char **owners = NULL;
  FileRec *file;

  XtVaGetValues(fw->viewport, XtNwidth, &width, NULL);
  icon_box_args[0].value = (XtArgVal) width;

  m_width = XTextWidth(resources.icon_font, "m", 1);
  name_w = longestName(fw) + 2*m_width;
  size_w = m_width * 7;
  perm_w = m_width * 9;
  date_w = m_width * 20;
  
  if (resources.show_owner) {
    owners = (char **) XtMalloc(fw->n_files * sizeof(char *));
    own_w = 0;
    for (i=0; i<fw->n_files; i++) { 
      /* bug fixed by hkarhune@hydra.helsinki.fi - Thanks */
      if((pw = getpwuid(fw->files[i]->stats.st_uid)) == NULL) {
	char tmp[11];
 	
	sprintf(tmp, "%lu", (unsigned long) fw->files[i]->stats.st_uid);
	owners[i] = XtNewString(tmp);
      }
      else
	owners[i] = XtNewString(pw->pw_name);
      l = XTextWidth(resources.icon_font, owners[i], strlen(owners[i]));
      if (l > own_w)
	own_w = l;
    }
  }

  fw->icon_box = XtCreateWidget("icon box",  boxWidgetClass,
				fw->viewport, icon_box_args,
				XtNumber(icon_box_args) );
  /* register icon_box as a "droppable" */
  MwDndRegisterDropWidget(fw->icon_box, IconBoxDropEventHandler, NULL);

  for (i=0; i<fw->n_files; i++) {
    Pixel pix;
    
    file = fw->files[i];
#ifdef MAGIC_HEADERS
    file->type = fileType(file->name, file->magic_type);
#endif
    if (S_ISDIR(file->stats.st_mode)) {
      sprintf(name, "[%s]", file->name);
      text_toggle_args[2].value = (XtArgVal) name;
      text_toggle_args[5].value = (XtArgVal) dir_translations;
    }
    else  {
      text_toggle_args[2].value = (XtArgVal) file->name;
      if (file->stats.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))
	text_toggle_args[5].value = (XtArgVal) exec_translations;
      else {
#ifdef MAGIC_HEADERS
	/* already got file type */
#else
        file->type = fileType(file->name); /* needed for push/drop-actions */
#endif
	text_toggle_args[5].value = (XtArgVal) file_translations;
      }
    }

    file->icon.form = XtCreateManagedWidget(file->name,
      formWidgetClass, fw->icon_box, icon_form_args,
      XtNumber(icon_form_args) );

    text_toggle_args[0].value = (XtArgVal) NULL;
    text_toggle_args[4].value = (XtArgVal) name_w;
    w = file->icon.toggle = XtCreateManagedWidget("name", 
      toggleWidgetClass, file->icon.form, text_toggle_args,
      XtNumber(text_toggle_args) );

	MwDndRegisterDragWidget(file->icon.toggle, DragEventHandler, NULL);
	MwDndRegisterDropWidget(file->icon.toggle, DropEventHandler, NULL);

    XtVaGetValues(file->icon.toggle, file->selected?XtNforeground:
		  XtNbackground, &pix, NULL);
    XtVaSetValues(file->icon.toggle, XtNborder, (XtArgVal) pix, NULL);

    if (resources.show_length) {
      sprintf(s, "%ld", (long) file->stats.st_size);
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal) s;
      text_label_args[4].value = (XtArgVal) size_w;
      text_label_args[5].value = (XtArgVal) XtJustifyRight;
      w = XtCreateManagedWidget("size", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }

    if (resources.show_owner) {
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal) owners[i];
      text_label_args[4].value = (XtArgVal) own_w;
      text_label_args[5].value = (XtArgVal) XtJustifyLeft;
      w = XtCreateManagedWidget("owner", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }

    if (resources.show_perms) {
      makePermissionsString(s, file->stats.st_mode);
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal) s;
      text_label_args[4].value = (XtArgVal) perm_w;
      text_label_args[5].value = (XtArgVal) XtJustifyLeft;
      w = XtCreateManagedWidget("permissions", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }

    if (resources.show_date) {
      text_label_args[0].value = (XtArgVal) w;
      text_label_args[2].value = (XtArgVal)ctime(&file->stats.st_mtime);
      text_label_args[4].value = (XtArgVal) date_w;
      text_label_args[5].value = (XtArgVal) XtJustifyLeft;
      w = XtCreateManagedWidget("date", labelWidgetClass, 
				file->icon.form, text_label_args,
				XtNumber(text_label_args) );
    }
  }

  if (resources.show_owner) {
    for(i=0; i<fw->n_files; i++)
      XTFREE(owners[i]);
    XTFREE(owners);
  }
}


/*----------------------------------------------------------------------------*/

/* create a directory icon in position specified by horiz & vert */
static Widget createDirIcon(FileWindowRec *fw, int i, Widget horiz,Widget vert)
{
  FileRec *file = fw->files[i];
  char *dirlabel;
  Pixel back;
  Pixmap icon = None;

#ifdef MAGIC_HEADERS
  file->type = fileType(file->name, file->magic_type);
  if (file->type)
    icon = (XtArgVal) file->type->icon_bm;
#endif
  if (icon == None)
    icon = bm[DIR_BM];

  /* create form */
  tree_form_args[0].value = (XtArgVal) horiz;
  tree_form_args[1].value = (XtArgVal) vert;
  file->icon.form = XtCreateManagedWidget(file->name,
    formWidgetClass, fw->icon_box, tree_form_args, XtNumber(tree_form_args) );

  /* create icon */
  icon_toggle_args[0].value = (XtArgVal) NULL;
  icon_toggle_args[1].value = (XtArgVal) NULL;
  icon_toggle_args[2].value = (XtArgVal) icon;
  icon_toggle_args[3].value = (XtArgVal) tree_translations;
  file->icon.toggle = XtCreateManagedWidget("icon",
    toggleWidgetClass, file->icon.form, icon_toggle_args,
    XtNumber(icon_toggle_args) );

 MwDndRegisterDragWidget(file->icon.toggle, DragEventHandler, NULL);
 MwDndRegisterDropWidget(file->icon.toggle, DropEventHandler, NULL);

  XtVaGetValues(file->icon.toggle, XtNbackground, &back, NULL);
  XtVaSetValues(file->icon.toggle, XtNborder, (XtArgVal) back, NULL);

  /* create label */
  icon_label_args[0].value = (XtArgVal) NULL;
  icon_label_args[1].value = (XtArgVal) file->icon.toggle;
  if (i == 0)
    dirlabel = fw->directory[1]?strrchr(fw->directory, '/')+1:fw->directory;
  else
    dirlabel = file->name;
  icon_label_args[2].value = (XtArgVal)dirlabel;
  file->icon.label = XtCreateManagedWidget("label",
    labelWidgetClass, file->icon.form, icon_label_args,
    XtNumber(icon_label_args) );

  return file->icon.form;
}

/*----------------------------------------------------------------------------*/

/* create the icons for the directory display */
static void createTreeDisplay(FileWindowRec *fw)
{
  int i, l;
  char *s = fw->directory[1]?strrchr(fw->directory, '/')+1:fw->directory;
  Widget vert, horiz;
  Pixmap line_bm;
  Dimension width;
  FileList files = fw->files;

  /* find width of icons */
  width = longestName(fw);
#if 1
  if (width < (l = XTextWidth(resources.icon_font, s, strlen(s))))
#else
	here we need to extract the XFontStruct from the real icon font
#endif
    width = l;
  if (width < resources.tree_icon_width)
    width = resources.tree_icon_width;
  tree_form_args[3].value = (XtArgVal) width;
  icon_toggle_args[4].value = (XtArgVal) width;
  icon_toggle_args[5].value = (XtArgVal) resources.tree_icon_height;
  icon_label_args[4].value = (XtArgVal) width;

  /* create icon box in viewport */
  XtVaGetValues(fw->viewport, XtNwidth, &width, NULL);
  tree_box_args[0].value = (XtArgVal) width;
  fw->icon_box = XtCreateWidget("icon box", formWidgetClass,
    fw->viewport, tree_box_args, XtNumber(tree_box_args) );

	/* register icon_box as a "droppable" */
	 MwDndRegisterDropWidget(fw->icon_box, IconBoxDropEventHandler, NULL);

  /* The '..' directory is not displayed, and no arrow for '.'  */
  files[1]->icon.form = files[1]->icon.toggle = 
    files[1]->icon.label = NULL;
  files[0]->icon.arrow = NULL;
    
  /* create left arrow */
  arrow_args[0].value = (XtArgVal) NULL;
  arrow_args[1].value = (XtArgVal) NULL;
  if (!permission(&files[1]->stats, P_EXECUTE)) {
    arrow_args[2].value = bm[NOENTRY_CBM];
    arrow_args[3].value = False;
  }
  else {
    arrow_args[2].value = bm[LARROW_BM];
    arrow_args[3].value = True;
  }
  horiz = files[1]->icon.arrow = XtCreateManagedWidget("left arrow",
	commandWidgetClass, fw->icon_box, arrow_args, XtNumber(arrow_args) );
  XtAddCallback(horiz, XtNcallback, (XtCallbackProc) mainArrowCb, fw);

  /* create current directory icon */
  horiz = createDirIcon(fw, 0,  horiz, NULL);

  vert = NULL;
 
  for(i = 2; i < fw->n_files; i++, horiz = files[0]->icon.form) {
    
    /* create line */
    if (i == 2)
      if (fw->n_files == 3)
	line_bm = bm[LLINE_BM];
      else
	line_bm = bm[TLINE_BM];
    else
      if (i == fw->n_files - 1)
	line_bm = bm[CLINE_BM];
      else
	line_bm = bm[FLINE_BM];
    line_args[0].value = (XtArgVal) horiz;
    line_args[1].value = (XtArgVal) vert;
    line_args[2].value = (XtArgVal) line_bm;
    horiz  = XtCreateManagedWidget("line", labelWidgetClass, 
      fw->icon_box, line_args, XtNumber(line_args) );
    
    /* create icon */
    horiz = createDirIcon(fw, i, horiz, vert);
    
    /* create right arrow */
    arrow_args[0].value = (XtArgVal) horiz;
    arrow_args[1].value = (XtArgVal) vert;
    if (!permission(&files[i]->stats, P_EXECUTE)) {
      arrow_args[2].value = bm[NOENTRY_CBM];
      arrow_args[3].value = False;
    }
    else if (files[i]->sym_link) {
      arrow_args[2].value = bm[WAVY_BM];
      arrow_args[3].value = True;
    }
    else {
      arrow_args[2].value = bm[RARROW_BM];
      arrow_args[3].value = True;
    }
    vert = files[i]->icon.arrow 
      = XtCreateManagedWidget("right arrow", commandWidgetClass, fw->icon_box, 
			      arrow_args, XtNumber(arrow_args) );
    XtAddCallback(vert, XtNcallback, (XtCallbackProc) mainArrowCb, fw);
  } 
}

/*-----------------------------------------------------------------------------
  PUBLIC FUNCTIONS
-----------------------------------------------------------------------------*/

/* find the device for a directory */
int findDev(char *path)
{
  int d;

  for (d = 0; d < n_devices; d++)
    if (prefix(devs[d].name, path))
      return d;
  return -1;
}

/*---------------------------------------------------------------------------*/
/* mount a device */
void mountDev(int d)
{
  if (d == -1)
    ;
  else if (devs[d].mounted)
    devs[d].mounted++;
  else
    devs[d].mounted += devAction(d, devs[d].mount_action);
}

/*---------------------------------------------------------------------------*/
/* unmount a device */
void umountDev(int d)
{
  if (d == -1 || !devs[d].mounted)
    ;
  else if (devs[d].mounted > 1)
    devs[d].mounted--;
  else
    devs[d].mounted -= devAction(d, devs[d].umount_action);
}

/*---------------------------------------------------------------------------*/
/* initialise the file Windows module */
void initFileWindows()
{
	char UsersCfgFile[MAXPATHLEN];
	FILE *fp;
	
  XtAppAddActions(app_context, file_actions, XtNumber(file_actions));
  label_translations = XtParseTranslationTable(label_translations_s);
  dir_translations = XtParseTranslationTable(dir_translations_s);
  file_translations = XtParseTranslationTable(file_translations_s);
  iconbox_translations = XtParseTranslationTable(iconbox_translations_s);
  tree_translations = XtParseTranslationTable(tree_translations_s);
  exec_translations = XtParseTranslationTable(exec_translations_s);

  icon_box_args[1].value = (XtArgVal) iconbox_translations;
  label_args[9].value = (XtArgVal) label_translations;

  label_args[3].value = (XtArgVal) resources.label_font;
  icon_label_args[3].value = (XtArgVal) resources.icon_font;
  text_toggle_args[3].value = (XtArgVal) resources.icon_font;
  text_label_args[3].value = (XtArgVal) resources.icon_font;
  shell_args[1].value = (XtArgVal) bm[ICON_BM];
  shell_args[2].value = (XtArgVal) bm[ICONMSK_BM];

  file_popup_items = createFloatingMenu("file popup", file_popup_menu,
					XtNumber(file_popup_menu), 4, toplevel,
					NULL, &file_popup_widget);
  XtRegisterGrabAction(filePopup, True, ButtonPressMask | ButtonReleaseMask,
		       GrabModeAsync, GrabModeAsync);
  dir_popup_items = createFloatingMenu("dir popup", dir_popup_menu,
					XtNumber(dir_popup_menu), 4, toplevel,
					NULL, &dir_popup_widget);
  XtRegisterGrabAction(dirPopup, True, ButtonPressMask | ButtonReleaseMask,
		       GrabModeAsync, GrabModeAsync);

	/* read config file*/
  	strcpy(UsersCfgFile, USERS_CFG_FILE);
	fnexpand(UsersCfgFile);
	if (!(fp = fopen(UsersCfgFile, "r")))
	{	  /* system-wide */	
		readFileTypes(resources.cfg_file);
		readDevices(resources.cfg_file);
	}
	else
	{
		/* user's */
		fclose(fp);
		readFileTypes(UsersCfgFile);
		readDevices(UsersCfgFile);
	}		
#ifdef MAGIC_HEADERS
#ifdef USE_MAGIC_HEADERS
		/* read config file*/
  	strcpy(UsersCfgFile, USERS_MAGIC_FILE);
	fnexpand(UsersCfgFile);
	if (!(fp = fopen(UsersCfgFile, "r")))
	    /* system-wide */	
		magic_parse_file(resources.magic_file);
	else
	{
		/* user's */
		fclose(fp);
		magic_parse_file(UsersCfgFile);
 	}		
#endif
#endif
}

static Widget make_command(FmCallbackProc cb,
			XtPointer data, Widget pw, char *pm, char *tt)
{
        Widget w;
        Pixmap pm_return;
        Pixel color;

        XtVaGetValues(pw, XtNbackground, &color, (char *)0);
        w = XtVaCreateManagedWidget("toolbar_command",
                commandWidgetClass, pw,
                XtNforeground, color,
                (char *)NULL);
	pm_return = load_pixmap(XtDisplay(pw), color, pm);

        XtVaSetValues(w,
                XtNbitmap, pm_return,
                (char *)0);

        XtAddCallback(w,
                XtNcallback, (XtCallbackProc)cb, (XtPointer)data);
	MwTooltipAdd(tooltip, w, _(tt));
        return w;
}

static void create_toolbar(Widget pw, XtPointer fw)
{
	make_command(fileCloseCb, fw, pw, "quit.xpm",
		     "Close window");
	make_command(fileHomeCb, fw, pw, "home.xpm",
		     "Go to home directory");
	make_command(fileUpCb, fw, pw, "fld_up.xpm",
		     "Go up one directory");
	make_command(mkdirPopup, fw, pw, "fld_new.xpm",
		     "Create folder");
	make_command(fileComHereCb, fw, pw, "xterm16.xpm",
		     "Open command window");
	make_command(fileViewModeCb, fw, pw, "viewmode.xpm",
		     "Change view mode");
	make_command(fileSortModeCb, fw, pw, "sortmode.xpm",
		     "Change sort mode");
	make_command(updateIconsCb, fw, pw, "icons.xpm",
		     "Update icons");
	make_command(editorCb, fw, pw, "editor.xpm",
		     "Text editor");
	make_command(fileFindCb, fw, pw, "search.xpm",
		     "Find file");
	make_command(helpContentsCb, fw, pw, "info.xpm",
		     "Display the online documentation");
}

static void remake_ylayout(FileWindowRec *fw)
{
	char b[100];
	sprintf(b, "%s %s 30 100%% 30",
		(fw->bars&MENUBAR) ? "30" : "0",
		(fw->bars&TOOLBAR) ? "30" : "0");
	XtVaSetValues(fw->form,
		XtNyLayout, b,
		(char *)0);
}

static void attach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	FileWindowRec *fw = find_window_by_widget(w);
	if (fw == NULL) return;
	if (w == fw->menubar) fw->bars |= MENUBAR;
	if (w == fw->toolbar) fw->bars |= TOOLBAR;
	remake_ylayout(fw);
}

static void detach_bar(Widget w, XtPointer client_data, XtPointer call_data)
{
	FileWindowRec *fw = find_window_by_widget(w);
	if (fw == NULL) return;
	if (w == fw->menubar) fw->bars &= ~MENUBAR;
	if (w == fw->toolbar) fw->bars &= ~TOOLBAR;
	remake_ylayout(fw);
}


/*---------------------------------------------------------------------------*/
/* Create a file Window at the specified path, in the specified format */

static FileWindowRec *createFileWindow(String path, String title, 
				       DisplayType format)
{
  FileWindowRec *fw;
  char *shell_name;
  
#ifdef DEBUG_MALLOC
  fprintf(stderr, "entering createFileWindow: %lu\n", malloc_inuse(NULL));
#endif

  if (chdir(path)) {
    sysError("Can't open folder:");
    return NULL;
  }

  /* put at front of linked list */
  fw = (FileWindowRec *) XtMalloc(sizeof(FileWindowRec));
  fw->next = file_windows;
  file_windows = fw;
  
  if (!getcwd(fw->directory, MAXPATHLEN)) {
    sysError("Can't open folder:");
    return NULL;
  }

  /* set up defaults */
  fw->dev = -1;
  fw->display_type = format;
  fw->sort_type = resources.default_sort_type;
  fw->show_dirs = True;
  fw->show_hidden = False;
  fw->dirs_first = True;
  fw->n_selections = 0;
  fw->n_bytes_selected = 0;
  fw->unreadable = NULL;
  fw->files = NULL;
  fw->n_files = 0;
  fw->n_bytes = 0;
  fw->update = False;
  /* KMR */ /* AG removed inherited do_filter attribute */
  fw->do_filter = False;
  fw->dirFilter[0] = '\0';
  fw->x = 0;
  fw->y = 0;
  fw->ready = False;

  shell_name = "file window";
  shell_args[0].value = (XtArgVal) title;
  fw->shell = XtCreatePopupShell(shell_name, topLevelShellWidgetClass,
				 toplevel, shell_args, XtNumber(shell_args) );
  if (resources.init_geometry)
    XtVaSetValues(fw->shell, XtNgeometry, resources.init_geometry, NULL);
  
  /* create form */
  fw->form = XtVaCreateManagedWidget("topbox",
		mwRudegridWidgetClass, fw->shell,
		(char *)0);
  fw->menubar = XtVaCreateManagedWidget("menubar",
		mwRudegridWidgetClass, fw->form,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
  MwMakeHandle(fw->menubar, fw->menubar, detach_bar, attach_bar);
  fw->menubox = XtVaCreateManagedWidget("menubox",
		mwMenuBarWidgetClass, fw->menubar,
		XtNgridx, 1,
		(char *)0);
  fw->bars |= MENUBAR;
  fw->toolbar = XtVaCreateManagedWidget("toolbar",
		mwRudegridWidgetClass, fw->form,
		XtNborderWidth, 0,
		XtNxLayout, "9 100%",
		(char *)0);
  fw->toolhandle = MwMakeHandle(fw->toolbar, fw->toolbar,
			detach_bar, attach_bar);
  fw->toolbox = XtVaCreateManagedWidget("frame2",
		mwFrameWidgetClass, fw->toolbar,
		XtNgridx, 1,
		XtNshadowWidth, 1,
		(char *)0);
  fw->toolbox = XtVaCreateManagedWidget("toolbox",
		boxWidgetClass, fw->toolbox,
		(char *)0);
  fw->bars |= TOOLBAR;
  fw->cmdform = XtVaCreateManagedWidget("cmdform",
		formWidgetClass, fw->form,
		(char *)0);
  fw->cmdframe = XtVaCreateManagedWidget("cmdframe",
		mwFrameWidgetClass, fw->cmdform,
		(char *)0);
  fw->cmdtext = XtVaCreateManagedWidget("cmdtext",
		mwTextfieldWidgetClass, fw->cmdframe,
		(char *)0);

  XtSetKeyboardFocus(fw->form, fw->cmdtext);

  /* create the menus */
  fw->file_items = createMenu("file", "File", file_menu, XtNumber(file_menu),
			      4, fw->menubox, (XtPointer) fw);
  fw->folder_items = createMenu("folder", "Folder", folder_menu, 
				XtNumber(folder_menu), 4, fw->menubox,
				(XtPointer) fw);
  fw->view_items = createMenu("view", "View", view_menu, XtNumber(view_menu),
			      16, fw->menubox, (XtPointer) fw);

  fw->help_items = createMenu("help", "Help", help_menu, XtNumber(help_menu),
			4, fw->menubox, (XtPointer) fw);

  create_toolbar(fw->toolbox, (XtPointer)fw);

  /* create viewport */
  fw->viewport = XtVaCreateManagedWidget("viewport",
		viewportWidgetClass, fw->form,
		(char *)0);

  /* Set up the x/y updater callback */
	/* FIXME: updateXY is wrong type */
  XtAddCallback( fw->viewport, XtNreportCallback,
				 (XtCallbackProc)updateXY, fw );

  /* create status line */
  fw->status = XtVaCreateManagedWidget("status",
		labelWidgetClass, fw->form,
		(char *)0);

#ifdef DEBUG_MALLOC
  fprintf(stderr, "exiting createFileWindow: %lu\n", malloc_inuse(NULL));
#endif

  /* Drag and drop stuff */
  MwDndAddShell(fw->shell);

  return fw;
}

/*----------------------------------------------------------------------------*/

void newFileWindow(String path, DisplayType d, Boolean by_cursor,
		   Boolean iconic)
{
  FileWindowRec *fw;

#ifdef DEBUG_MALLOC
  fprintf(stderr, "entering newFileWindow: %lu\n", malloc_inuse(NULL));
#endif

  if (!(fw = createFileWindow(path, "Xfiler", d)))
    return;
  createFileDisplay(fw);
	
  if(iconic==True)
      XtVaSetValues(fw->shell,XtNinitialState,IconicState,0);
  else
      XtVaSetValues(fw->shell,XtNinitialState,NormalState,0);
      
  XtRealizeWidget(fw->shell);
  XSetIconName(XtDisplay(fw->shell), XtWindow(fw->shell), fw->directory);
  XSetWMProtocols(XtDisplay(fw->shell), XtWindow(fw->shell),
	  	  &wm_delete_window, 1);
  XtAddEventHandler(fw->shell, (EventMask)0L, True,
		    (XtEventHandler)clientMessageHandler, (XtPointer)NULL);

  if (by_cursor)
    popupByCursor(fw->shell, XtGrabNone);
  else
    XtPopup(fw->shell, XtGrabNone);

#ifdef DEBUG_MALLOC
  fprintf(stderr, "exiting newFileWindow: %lu\n", malloc_inuse(NULL));
#endif
}

/*---------------------------------------------------------------------------*/

/* Main procedure to create the display in the viewport */
void createFileDisplay(FileWindowRec *fw)
{
  int i;

#ifdef DEBUG_MALLOC
  fprintf(stderr, "entering createFileDisplay: %lu\n", malloc_inuse(NULL));
#endif

  XtVaSetValues(fw->shell,
		XtNtitle, (XtArgVal) fw->directory,
		(char *)NULL);

  fw->icon_box = NULL;

  if (fw->unreadable) {
    XtDestroyWidget(fw->unreadable);
    fw->unreadable = NULL;
  }

  if (!readDirectory(fw)) {
    fw->unreadable = 
      XtVaCreateManagedWidget("label", labelWidgetClass, fw->viewport,
			      XtNlabel, _("Directory is unreadable"),
			      NULL);
    return;
  }

  for (i=0; i<fw->n_files; i++)
  {	  		
    fw->files[i]->selected = False;
    fw->files[i]->type = NULL;
  }		  
  fw->n_selections = 0;
  fw->n_bytes_selected = 0;

  switch (fw->display_type) {
  case Tree:
    filterDirectory(fw, Directories);
    sortDirectory(fw->files+2, fw->n_files-2, fw->sort_type, False);
    createTreeDisplay(fw);
    break;
  case Icons:
    filterDirectory(fw, fw->show_dirs ? All : Files);
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createFileIcons(fw);
    break;
  case Text:
    filterDirectory(fw, fw->show_dirs ? All : Files);
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createTextDisplay(fw);
    break;
  }

  updateStatus(fw);

  XtManageChild(fw->icon_box);

#ifdef DEBUG_MALLOC
  fprintf(stderr, "exiting createFileDisplay: %lu\n", malloc_inuse(NULL));
#endif
}

/*---------------------------------------------------------------------------*/

/* Update the display in the viewport */
void updateFileDisplay(FileWindowRec *fw)
{
  int d;
  Position tmpx, tmpy;

#ifdef DEBUG_MALLOC
  fprintf(stderr, "entering updateFileDisplay: %lu\n", malloc_inuse(NULL));
#endif

  zzz();

  d = fw->dev;

  /* These get trashed if we don't save them now. BAH. */
  tmpx = fw->x;
  tmpy = fw->y;

  if (fw->icon_box)
    XtDestroyWidget(fw->icon_box);

  freeFileList(fw);
  createFileDisplay(fw);

  /* Now jiggle the window back to where we left it */
  if ( fw->ready )
	XawViewportSetCoordinates( fw-> viewport, tmpx, tmpy );

  if (d != -1) umountDev(d);

  XSetIconName(XtDisplay(fw->shell), XtWindow(fw->shell), fw->directory);

  wakeUp();

#ifdef DEBUG_MALLOC
  fprintf(stderr, "exiting updateFileDisplay: %lu\n", malloc_inuse(NULL));
#endif
}

/*---------------------------------------------------------------------------*/

/* resort the icons in the display */
void reSortFileDisplay(FileWindowRec *fw)
{
#ifdef DEBUG_MALLOC
  fprintf(stderr, "entering resortFileDisplay: %lu\n", malloc_inuse(NULL));
#endif

  if (fw->unreadable)
    return;

  zzz();

  XtDestroyWidget(fw->icon_box);
  fw->n_selections = 0;
  fw->n_bytes_selected = 0;

  switch (fw->display_type) {
  case Tree:
    sortDirectory(fw->files+2, fw->n_files-2, fw->sort_type, False);
    createTreeDisplay(fw);
    break;
  case Icons:
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createFileIcons(fw);
    break;
  case Text:
    sortDirectory(fw->files, fw->n_files, fw->sort_type, fw->dirs_first);
    createTextDisplay(fw);
    break;
  }

  updateStatus(fw);
  XtManageChild(fw->icon_box);

  wakeUp();

#ifdef DEBUG_MALLOC
  fprintf(stderr, "exiting resortFileDisplay: %lu\n", malloc_inuse(NULL));
#endif
}

/*---------------------------------------------------------------------------*/

void reDisplayFileWindow(FileWindowRec *fw)
{
#ifdef DEBUG_MALLOC
  fprintf(stderr, "entering redisplayFileWindow: %lu\n", malloc_inuse(NULL));
#endif

  if (fw->unreadable)
    return;

  zzz();

  XtDestroyWidget(fw->icon_box);

  switch (fw->display_type) {
  case Tree:
    createTreeDisplay(fw);
    break;
  case Icons:
    createFileIcons(fw);
    break;
  case Text:
    createTextDisplay(fw);
    break;
  }

  updateStatus(fw);
  XtManageChild(fw->icon_box);

  wakeUp();

#ifdef DEBUG_MALLOC
  fprintf(stderr, "exiting redisplayFileWindow: %lu\n", malloc_inuse(NULL));
#endif
}

/*----------------------------------------------------------------------------
  Intelligent update - only update the windows needed.
  Use markForUpdate() to explicitly mark a directory for update.
  Call intUpdate to execute all the actions.
-----------------------------------------------------------------------------*/
void markForUpdate(String path)
{
  FileWindowRec *fw;

  for (fw = file_windows; fw; fw = fw->next)
    if (!strcmp(path, fw->directory))
      fw->update = True;
}

void intUpdate()
{
  FileWindowRec *fw;
  struct stat cur;

  for (fw = file_windows; fw; fw = fw->next) {
    if (fw->update ||
	stat(fw->directory, &cur) ||
	cur.st_ctime > fw->stats.st_ctime)
      updateFileDisplay(fw);
  }

  for (fw = file_windows; fw; fw = fw->next)
    fw->update = False;
}

/*-----------------------------------------------------------------------------
  Keep menus and status line consistent with the number of selections in each
  window. Currently this must be called manually, which is bad news.
-----------------------------------------------------------------------------*/
void updateStatus(FileWindowRec *fw)
{
  char s[1024], t[1024];
  int n_files, n_selections;
  long n_bytes, n_bytes_selected;

  if (fw->n_selections >= 1) {
    fillIn(fw->file_items[2]);
    fillIn(fw->file_items[3]);
    fillIn(fw->file_items[4]);
    fillIn(fw->file_items[6]);
    fillIn(fw->file_items[10]);
  }else {
    grayOut(fw->file_items[2]);
    grayOut(fw->file_items[3]);
    grayOut(fw->file_items[4]);
    grayOut(fw->file_items[6]);
    grayOut(fw->file_items[10]);
  }

  if (fw->display_type == Tree) {         /* incremented the view_item */
    grayOut(fw->view_items[10]);          /* numbers by 1 since I added */
    grayOut(fw->view_items[11]);          /* a new menu pick in slot 7 */
    noTick(fw->view_items[10]);           /* only affects items 8 and above */
    noTick(fw->view_items[11]);           /* KMR */
  }
  else {
    fillIn(fw->view_items[10]);
    if (fw->show_dirs) {
      fillIn(fw->view_items[11]);
      noTick(fw->view_items[10]);
      if (fw->dirs_first)
	noTick(fw->view_items[11]);
      else
	tick(fw->view_items[11]);
    }
    else {
      grayOut(fw->view_items[11]);
      tick(fw->view_items[10]);
      noTick(fw->view_items[11]);
    }
  }

  if (fw->show_hidden)
    tick(fw->view_items[12]);
  else
    noTick(fw->view_items[12]);

  noTick(fw->view_items[0]);
  noTick(fw->view_items[1]);
  noTick(fw->view_items[2]);
  noTick(fw->view_items[4]);
  noTick(fw->view_items[5]);
  noTick(fw->view_items[6]);
  noTick(fw->view_items[8]);

  switch (fw->display_type) {
  case Tree:
    tick(fw->view_items[0]);
    break;
  case Icons:
    tick(fw->view_items[1]);
    break;
  case Text:
    tick(fw->view_items[2]);
    break;
  }

  switch (fw->sort_type) {
  case SortByName:
    tick(fw->view_items[4]);
    break;
  case SortBySize:
    tick(fw->view_items[5]);
    break;
  case SortByMTime:
    tick(fw->view_items[6]);
    break;
  }

  /* update the status line */

  n_bytes = fw->n_bytes;
  n_files = fw->n_files;
  n_bytes_selected = fw->n_bytes_selected;
  n_selections = fw->n_selections;

  if (fw->display_type == Tree) {
    n_bytes -= fw->files[1]->stats.st_size;
    n_files--;
  }

  if (fw->do_filter)
    sprintf(t, " [%s]", fw->dirFilter);
  else
    *t = '\0';

/* this needs some work to internationalize */

  if (n_selections > 0)
    sprintf(s,
	"%ld byte%s in %d item%s, %ld byte%s in %d selected item%s%s",
	n_bytes, n_bytes==1?"":"s",
	n_files, n_files==1?"":"s",
	n_bytes_selected, n_bytes_selected==1?"":"s",
	n_selections, n_selections==1?"":"s", t);
  else
    sprintf(s, "%ld byte%s in %d item%s%s", n_bytes, n_bytes==1?"":"s",
	n_files, n_files==1?"":"s", t);

  XtVaSetValues(fw->status, XtNlabel, (XtArgVal) s, NULL);
}
