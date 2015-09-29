/*---------------------------------------------------------------------------
  Module FmFwCb

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994

  Callback routines for widgets in a file window
---------------------------------------------------------------------------*/

#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Toggle.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <Mowitz/Mowitz.h>

#include "Files.h"

#include "../common/common.h"

/*-----------------------------------------------------------------------------
  This function is also used in FmFwActions when a directory is pulled onto
  the root window. In this case, w will be zero and we use this to popup
  the new window by the cursor.
-----------------------------------------------------------------------------*/
void fileOpenCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;
  char pathname[MAXPATHLEN];

  if (fw == NULL)
    fw = popup_fw;

  for (i=0; i<fw->n_files; i++) {
    if (fw->files[i]->selected && S_ISDIR(fw->files[i]->stats.st_mode)) {
      strcpy(pathname, fw->directory);
      if (pathname[strlen(pathname)-1] != '/')
	strcat(pathname, "/");
      strcat(pathname, fw->files[i]->name);      
      newFileWindow(pathname,resources.default_display_type,
		    w ? False : True, False);
    }
  }
}

/*---------------------------------------------------------------------------*/

void fileEditCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;

  if (fw == NULL)
    fw = popup_fw;

  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->selected && !S_ISDIR(fw->files[i]->stats.st_mode))
	doEdit(fw->directory,fw->files[i]->name);
}

void editorCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	MwSpawn("siagrun editor");
}

/*---------------------------------------------------------------------------*/

void fileViewCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;

  if (fw == NULL)
    fw = popup_fw;

  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->selected && !S_ISDIR(fw->files[i]->stats.st_mode))
	doView(fw->directory,fw->files[i]->name);
}

/*---------------------------------------------------------------------------*/

void fileComHereCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;
 int j=0;
  if (fw == NULL)
    fw = popup_fw;

  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->selected && S_ISDIR(fw->files[i]->stats.st_mode))
	{
	  doComHere(fw->directory,fw->files[i]->name); 
	  j=1;
	}
    if (j == 0) 
	 doComHere(fw->directory,fw->directory);
}

/*---------------------------------------------------------------------------*/

void fileTreeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->display_type = Tree;
  updateFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileIconsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  DisplayType t = fw->display_type;

  fw->display_type = Icons;
  if (t == Text)
    reDisplayFileWindow(fw);
  else
    updateFileDisplay(fw);
}

void updateIconsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char b[1024];
	forget_icons(XtDisplay(w));
	sprintf(b, "%s/xfiler/makeicons", datadir);
	system(b);
	fileIconsCb(w, fw, call_data);
}

/*---------------------------------------------------------------------------*/

void fileTextCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  DisplayType t = fw->display_type;

  fw->display_type = Text;
  if (t == Icons)
    reDisplayFileWindow(fw);
  else
    updateFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileSelectAllCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;
  Pixel pix;
  
  fw->n_selections = 0;
  fw->n_bytes_selected = 0;
  for (i=0; i < fw->n_files; i++) {
    if (fw->files[i]->icon.toggle &&
	strcmp(fw->files[i]->name, ".") &&
	strcmp(fw->files[i]->name, "..")) {
      fw->files[i]->selected = True;
      fw->n_selections++;
      fw->n_bytes_selected += fw->files[i]->stats.st_size;
    }
    else
      fw->files[i]->selected = False;
    if (fw->files[i]->icon.toggle) {
      XtVaGetValues(fw->files[i]->icon.toggle,
		    fw->files[i]->selected?XtNforeground:XtNbackground, &pix,
		    NULL);
      XtVaSetValues(fw->files[i]->icon.toggle, XtNborder, (XtArgVal) pix,
		    NULL);
    }
  }
  updateStatus(fw);
}

/*---------------------------------------------------------------------------*/

void fileDeselectCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;
  Pixel pix;
  
  for (i=0; i < fw->n_files; i++)
    if (fw->files[i]->selected && fw->files[i]->icon.toggle) {
      fw->files[i]->selected = False;
      XtVaGetValues(fw->files[i]->icon.toggle, XtNbackground, &pix, NULL);
      XtVaSetValues(fw->files[i]->icon.toggle, XtNborder, (XtArgVal) pix,
		    NULL);
    }
  fw->n_selections = 0;
  fw->n_bytes_selected = 0;
  updateStatus(fw);
}

/*---------------------------------------------------------------------------*/

void fileSortNameCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->sort_type = SortByName;
  reSortFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileSortSizeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->sort_type = SortBySize;
  reSortFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileSortMTimeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->sort_type = SortByMTime;
  reSortFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileCloseCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  FileWindowRec *p;
  int d;

  if (fw == file_windows && fw->next == NULL) {
    if (!resources.confirm_quit || confirm("", "Exit file manager?", "")) {
      quit();
    } else {
      return;
    }
  }

  if ((d = findDev(fw->directory)) != -1) umountDev(d);

  XtDestroyWidget(fw->shell);

  if (fw == file_windows)
    file_windows = fw->next;
  else {
    for (p = file_windows; p->next != fw; p = p->next);
    p->next = fw->next;
  }

  freeFileList(fw);
  XTFREE(fw->file_items);
  XTFREE(fw->folder_items);
  XTFREE(fw->view_items);
  XTFREE(fw);

  chdir(user.home);
}

/*---------------------------------------------------------------------------*/

void toplevelCloseCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  if (resources.confirm_quit && !confirm("", "Exit file manager?", ""))
    return;

  quit();
}

/*---------------------------------------------------------------------------*/

void fileHomeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  freeze = True;
  if (chdir(user.home))
    sysError("Can't open folder:");
  else if (!getcwd(fw->directory, MAXPATHLEN))
    sysError("System error:");
  getcwd(fw->directory, MAXPATHLEN);
  updateFileDisplay(fw);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

void fileUpCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  freeze = True;
  if (chdir(fw->directory) || chdir(".."))
    sysError("Can't open folder:");
  else if (!getcwd(fw->directory, MAXPATHLEN))
    sysError("System error:");
  getcwd(fw->directory, MAXPATHLEN);
  updateFileDisplay(fw);
  freeze = False;
}

void makeIconsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	freeze = True;
	updateFileDisplay(fw);
	freeze = False;
}

/*---------------------------------------------------------------------------*/

void mainArrowCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;

  freeze = True;

  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->icon.arrow == w) {
/*      getcwd(fw->directory, MAXPATHLEN); */
      if (chdir(fw->directory) || chdir(fw->files[i]->name))
	sysError("Can't open folder:");
      else if (!getcwd(fw->directory, MAXPATHLEN))
	sysError("System error:");
      break;
    }
  updateFileDisplay(fw);
  freeze = False;
}


/*---------------------------------------------------------------------------*/

void fileShowDirsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->show_dirs = !fw->show_dirs;
  updateFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileDirsFirstCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->dirs_first = !fw->dirs_first;
  reSortFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileShowHiddenCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  fw->show_hidden = !fw->show_hidden;
  updateFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void timeoutCb(XtPointer data, XtIntervalId *id)
{
  if (!freeze) intUpdate();
  XtAppAddTimeOut(app_context, resources.update_interval, timeoutCb, NULL);
}

void fileViewModeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	switch (fw->display_type) {
	case Tree:
		fileIconsCb(w, fw, call_data);
		break;
	case Icons:
		fileTextCb(w, fw, call_data);
		break;
	default:	/* Text */
		fileTreeCb(w, fw, call_data);
		break;
	}
}

void fileSortModeCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	switch (fw->sort_type) {
	case SortByName:
		fileSortSizeCb(w, fw, call_data);
		break;
	case SortBySize:
		fileSortMTimeCb(w, fw, call_data);
		break;
	default:	/* SortByMTime */
		fileSortNameCb(w, fw, call_data);
		break;
	}
}

void fileFindCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char fn[1024];
	char cmd[1024];
	char **files = NULL;
	int nfiles = 0, i;
	FILE *fp;

	fn[0] = '\0';
	if (MwDialogInput(toplevel, "Find pattern:", fn)) {
		sprintf(cmd, "find %s -name '%s' -print",
			fw->directory, fn);
		if (!(fp = popen(cmd, "r"))) {
			MwErrorBox(toplevel, "Can't exec find");
			return;
		}
		while ((fgets(fn, sizeof fn, fp))) {
			MwChomp(fn);
			files = MwRealloc(files, (nfiles+1)*(sizeof(char *)));
			files[nfiles++] = MwStrdup(fn);
		}
		pclose(fp);
		if (nfiles) {
			int n = MwListBox(toplevel,
				"Pick one:", files, nfiles);
			if (n != -1) {
				char *p = strrchr(files[n], '/');
				if (p) {
					*(p+1) = '\0';
					chdir(files[n]);
					getcwd(fw->directory, MAXPATHLEN);
					updateFileDisplay(fw);
				} else {
					; /* bogus path, ignore */
				}
			}
		} else {
			MwErrorBox(toplevel, "No files found");
		}
		for (i = 0; i < nfiles; i++)
			MwFree(files[i]);
		MwFree(files);
	}
}

static void do_help(char *p)
{
	char b[1024];

	sprintf(b, "siagrun help file:%s/%s", docdir, p);
	MwSpawn(b);
}

void helpContentsCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	do_help("xfiler/xfiler.html");
}

void helpAboutXfilerCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char msg[1024];
	sprintf(msg, "%s\n\n%s\n\nUlric Eriksson, ulric@siag.nu\n\n%s\n",
		version,
		_("A file manager for X"),
		_("Part of Siag Office"));
	MwAboutBox(toplevel, "xfiler.xpm", msg);
}

void helpAboutSiagCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	MwAboutSiag(toplevel);
}

