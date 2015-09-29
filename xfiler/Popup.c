/*---------------------------------------------------------------------------
  Module FmPopup

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994

  Routines for creating and managing popup forms and dialog boxes

  modified 1-29-95 by rodgers@lvs-emh.lvs.loral.com (Kevin M. Rodgers)
  to add filtering of icon/text directory displays by a filename filter.

  Modified 1999 by Ulric, mainly to reduce global variable spaghetti.

---------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Toggle.h>

#include "Files.h"

#include "../common/common.h"	/* for ABORT */

#include <Mowitz/Mowitz.h>

/*---------------------------------------------------------------------------
  STATIC DATA
---------------------------------------------------------------------------*/

/* this is a mess */
typedef struct {
  FileWindowRec *fw;
  Widget select, filter;
  char s[MAXPATHLEN], t[MAXPATHLEN];
  char select_s[MAXPATHLEN];
  char filter_s[MAXPATHLEN];
} PopupsRec;

static PopupsRec popups;

/*---------------------------------------------------------------------------
  PRIVATE FUNCTIONS
---------------------------------------------------------------------------*/

char *dir_prefix(char *dir, char *path)
{
  char *p;
  if ((p = strrchr(path, '/'))) {
    strcpy(dir, path);
    if (p == path)
      dir[1] = '\0';
    else
      dir[p-path] = '\0';
  } else
    dir[0] = '\0';
  return dir;
}

static FmCallbackProc 
  selectAddCb, selectRemoveCb, selectCancelCb,
  selectReplaceCb, filterOkCb, filterClearCb, filterCancelCb;

/*---------------------------------------------------------------------------*/

/* The following variant of fnmatch matches the . and .. dirs only if
   specified explicitly. */

#define fnmatchnodot(pattern,fn) (strcmp(fn,".")&&strcmp(fn,"..")? \
				  fnmatch(pattern,fn):!strcmp(pattern,fn))

static void selectReplaceCb(Widget w, FileWindowRec *fw, 
			    XtPointer call_data)
{
  int i;
  Pixel pix;

  XtPopdown(popups.select);
  popups.fw->n_selections = 0;
  popups.fw->n_bytes_selected = 0;
  for (i=0; i<popups.fw->n_files; i++) {
    if (popups.fw->files[i]->icon.toggle) {
      if (fnmatchnodot(popups.select_s, popups.fw->files[i]->name)) {
	popups.fw->files[i]->selected = True;
	popups.fw->n_selections++;
	popups.fw->n_bytes_selected += popups.fw->files[i]->stats.st_size;
      }
      else
	popups.fw->files[i]->selected = False;
      XtVaGetValues(popups.fw->files[i]->icon.toggle,
		    popups.fw->files[i]->selected?XtNforeground:XtNbackground,
		    &pix, NULL);
      XtVaSetValues(popups.fw->files[i]->icon.toggle, XtNborder,
		    (XtArgVal) pix, NULL);
    }
  }
  updateStatus(popups.fw);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

static void selectAddCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;
  Pixel pix;
  
  XtPopdown(popups.select);
  for(i=0; i<popups.fw->n_files; i++)
    if (popups.fw->files[i]->icon.toggle) {
      if (!popups.fw->files[i]->selected && 
	  (fnmatchnodot(popups.select_s, popups.fw->files[i]->name))) {
	popups.fw->files[i]->selected = True;
	popups.fw->n_selections++;
	popups.fw->n_bytes_selected += popups.fw->files[i]->stats.st_size;
	XtVaGetValues(popups.fw->files[i]->icon.toggle, XtNforeground, &pix,
		      NULL);
	XtVaSetValues(popups.fw->files[i]->icon.toggle, XtNborder,
		      (XtArgVal) pix, NULL);
      }
    }
  updateStatus(popups.fw);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

static void selectRemoveCb(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  int i;
  Pixel pix;
  
  XtPopdown(popups.select);
  for(i=0; i<popups.fw->n_files; i++)
    if (popups.fw->files[i]->icon.toggle) {
      if (popups.fw->files[i]->selected && 
	  (fnmatch(popups.select_s, popups.fw->files[i]->name))) {
	popups.fw->files[i]->selected = False;
	popups.fw->n_selections--;
	popups.fw->n_bytes_selected -= popups.fw->files[i]->stats.st_size;
	XtVaGetValues(popups.fw->files[i]->icon.toggle, XtNbackground, &pix,
		      NULL);
	XtVaSetValues(popups.fw->files[i]->icon.toggle, XtNborder,
		      (XtArgVal) pix, NULL);
      }
    }
  updateStatus(popups.fw);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

static void selectCancelCb(Widget w, FileWindowRec *fw, 
			   XtPointer call_data)
{
  XtPopdown(popups.select);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

/* KMR */
static void filterCancelCb(Widget w, FileWindowRec *fw, 
			   XtPointer call_data)
{
  XtPopdown(popups.filter);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

/* KMR */
static void filterOkCb(Widget w, FileWindowRec *fw, 
			   XtPointer call_data)
{
  popups.fw->do_filter = True;
  strcpy(popups.fw->dirFilter,popups.filter_s);
  updateFileDisplay(popups.fw);
  freeze = False;
}

/*---------------------------------------------------------------------------*/

/* KMR */
static void filterClearCb(Widget w, FileWindowRec *fw, 
			   XtPointer call_data)
{
  XtPopdown(popups.filter);
  popups.fw->do_filter = False;
  popups.fw->dirFilter[0] = '\0';
  updateFileDisplay(popups.fw);
  freeze = False;
}

/*---------------------------------------------------------------------------
  Button information for popups
---------------------------------------------------------------------------*/

static ButtonRec select_buttons[] = {
  { "replace", "Replace", selectReplaceCb },
  { "add", "Add", selectAddCb },
  { "remove", "Remove", selectRemoveCb },
  { "cancel", "Cancel", selectCancelCb }
};

static ButtonRec filter_buttons[] = {       /* KMR */
  { "ok", "Ok", filterOkCb },
  { "clear", "Clear", filterClearCb },
  { "cancel", "Cancel", filterCancelCb }
}; 

/*---------------------------------------------------------------------------
  Question information for popups
---------------------------------------------------------------------------*/

static QuestionRec select_questions[] = {
  { "Filename pattern:", popups.select_s, MAXPATHLEN, NULL }
};

static QuestionRec filter_questions[] = {             /* KMR */
  { "Filename pattern:", popups.filter_s, MAXPATHLEN, NULL }
};

/*---------------------------------------------------------------------------
  PUBLIC FUNCTIONS
---------------------------------------------------------------------------*/

void createMainPopups()
{
  /* Select */
  popups.select = createPopupQuestions("select", "Select", bm[FILES_BM],
			       select_questions, XtNumber(select_questions),
			       select_buttons, XtNumber(select_buttons) );

  /* Filter */  /* KMR */
  popups.filter = createPopupQuestions("filter", "Filter", bm[FILES_BM],
			       filter_questions, XtNumber(filter_questions),
			       filter_buttons, XtNumber(filter_buttons) );

  /* Change Access Mode */
  createChmodPopup();

  /* Info */
  createInfoPopup();

  /* Errors */
  createErrorPopup();

  /* Deletions */
  createConfirmPopup();
}

/*---------------------------------------------------------------------------*/ 

void selectPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  popups.fw = fw;
  XtVaSetValues(select_questions[0].widget, XtNstring, 
		(XtArgVal) popups.select_s, NULL);
  freeze = True;
  popupByCursor(popups.select, XtGrabExclusive);
}

/*---------------------------------------------------------------------------*/ 

/* KMR */
void filterPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  popups.fw = fw;
  strcpy(popups.filter_s,fw->dirFilter);
  XtVaSetValues(filter_questions[0].widget, XtNstring, 
		(XtArgVal) popups.filter_s, NULL);
  freeze = True;
  popupByCursor(popups.filter, XtGrabExclusive);
}

/*---------------------------------------------------------------------------*/

/* ---
Here is a good example of something to convert. There are three
callbacks: mkdirPopup below is the main callback which is called
from the menu (Folder - New) or from the toolbar. The popup
has two callbacks, mkdirOkCb and mkdirCancelCb, which are used
by the dialog's two buttons.

Instead of this, we will call MwDialogInputIcon() in xcommon.
There is no need for the two button-specific callbacks and we
do not need to create the dialog in advance. By implementing
this for all callbacks, it will be possible to remove a large
amount of code, which is also poorly written and hard to maintain.

MwDialogInputIcon has these advantages:
 - handles keyboard focus
 - handles translation for i18n
*/
void mkdirPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "New Folder";
	char *p = "Create folder:";
	char b[1024];
	Pixmap icon = bm[DIR_BM];
	int n;

	freeze = True;	/* what's that for? */
	b[0] = '\0';
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
		fnexpand(b);
		if (chdir(fw->directory)) {
			sysError("System error:");
		} else if (mkdir(b, user.umask & 0777)) {
			char s[0xff];
			sprintf(s, "Error creating folder %s:", b);
			sysError(s);
		} else {
			intUpdate();
		}
	}
	freeze = False;
}

/*---------------------------------------------------------------------------*/

void createFilePopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "New File";
	char *p = "Create file:";
	char b[1024];
	Pixmap icon = bm[FILE_BM];
	int n;

	freeze = True;	/* what's that for? */
	b[0] = '\0';
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
		fnexpand(b);
		if (chdir(fw->directory)) {
			sysError("System error:");
		} else if (create(b, user.umask & 0666)) {
			char s[0xff];
			sprintf(s, "Error creating file %s:", b);
			sysError(s);
		} else {
			intUpdate();
		}
	}
	freeze = False;
}

/*---------------------------------------------------------------------------*/
 
void openWithPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "Open With";
	char *p = "Open with:";
	char b[1024];
	Pixmap icon = bm[FILE_BM];
	int i, n;

	popups.fw = fw;
	freeze = True;	/* what's that for? */
	b[0] = '\0';
	if (fw == NULL) fw = popup_fw;
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
		fnexpand(b);

		for (i=0; i<fw->n_files; i++) {
			if (fw->files[i]->selected
			    && !S_ISDIR(fw->files[i]->stats.st_mode))
				doOpenWith(fw->directory,
					   fw->files[i]->name,
					   b);
		}
	}
}

/*---------------------------------------------------------------------------*/

void goToPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "Go To";
	char *p = "Go to folder:";
	char b[1024];
	Pixmap icon = bm[DIR_BM];
	int n;

	popups.fw = fw;
	freeze = True;	/* what's that for? */
	b[0] = '\0';
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
	   fnexpand(b);
	   if (chdir(popups.fw->directory) || chdir(b)) {
	      char s[0xff];
	      sprintf(s, "Can't open folder %s:", b);
	      sysError(s);
	   } else if (!getcwd(b, MAXPATHLEN))
		sysError("System error:");
	   else {
	      strcpy(popups.fw->directory, b);
	      updateFileDisplay(popups.fw);
	   }
	}
   	freeze = False;
}

/*---------------------------------------------------------------------------*/

void movePopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "Move";
	char *p = "Move to:";
	char b[1024];
	Pixmap icon = bm[FILES_BM];
	int n;

   if (fw == NULL) fw = popup_fw;

   if (!fw->n_selections) return;

	popups.fw = fw;
	freeze = True;	/* what's that for? */
	b[0] = '\0';
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
	   struct stat stats;
	   int i, toi, n_moved = 0;
	   char *from = NULL, to[MAXPATHLEN], todir[MAXPATHLEN];

	   strcpy(to, b);
	   fnexpand(to);

	   if (chdir(popups.fw->directory))

	     sysError("System error:");

	   else {

	      /* if target exists and is a directory, move the source into that
	       directory */

	      if (!stat(to, &stats) && S_ISDIR(stats.st_mode)) {

		 if (chdir(to) || !getcwd(to, MAXPATHLEN) || chdir(popups.fw->directory)) {
		    sysError("System error:");
		    goto out;
		 } else if (!strcmp(popups.fw->directory, to)) {
		    error("Move:", "Source and destination are identical");
		    goto out;
		 }

		 strcpy(todir, to);

		 toi = strlen(to);
		 if (to[toi-1] != '/') {
		    to[toi++] = '/';
		    to[toi] = '\0';
		 }

		 for (i=0; i < popups.fw->n_files; i++)
		   if (popups.fw->files[i]->selected) {
		      if (!strcmp(popups.fw->files[i]->name, ".") ||
			  !strcmp(popups.fw->files[i]->name, "..")) {
			 error("Cannot move . or ..", "");
			 continue;
		      }
		      from = popups.fw->files[i]->name;
		      strcpy(to+toi, from);
		      if (exists(to) && resources.confirm_overwrite) {
			 char s[0xff];
			 sprintf(s, "Move: file %s already exists at destination", from);
			 if (!confirm(s, "Overwrite?", "")) {
			   if (aborted) break;
			 else continue;
			 }
		      }
		      if (rename(from,to)) {
			 char s[0xff];
			 sprintf(s, "Error moving %s:", from);
			 sysError(s);
		      } else
			n_moved++;
		   }
	      }

	      /* otherwise only a single file may be selected; move it to the target
	       file */

	      else if (popups.fw->n_selections > 1) {

		 error("Move: target for multiple files", "must be a folder");
		 goto out;

	      } else {

		 struct stat stats1;

		 for (i = 0; i < popups.fw->n_files; i++)
		   if (popups.fw->files[i]->selected) {
		      from = popups.fw->files[i]->name;
		      break;
		   }

		 if (!strcmp(from, ".") || !strcmp(from, "..")) {
		    error("Cannot move . or ..", "");
		    goto out;
		 } else if (!lstat(to, &stats) && !lstat(from, &stats1) &&
			    stats.st_ino == stats1.st_ino) {
		    error("Move:", "Source and destination are identical");
		    goto out;
		 }

		 if (exists(to) && resources.confirm_overwrite) {
		    char s[0xff];
		    sprintf(s, "Move: file %s already exists", to);
		    if (!confirm(s, "Overwrite?", ""))
		      goto out;
		 }

		 if (rename(from, to)) {
		    char s[0xff];
		    sprintf(s, "Error moving %s:", from);
		    sysError(s);
		 } else {
		    n_moved = 1;
		    dir_prefix(todir, to);
		    if ((*todir?chdir(todir):0) || !getcwd(todir, MAXPATHLEN))
		      sysError("System error:");
		 }
	      }

	      if (n_moved) {
		 markForUpdate(popups.fw->directory); markForUpdate(todir);
		 intUpdate();
	      }

	   }

	   out:
	   ;
	}
   	freeze = False;
}

/*---------------------------------------------------------------------------*/

void copyPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "Copy";
	char *p = "Copy to:";
	char b[1024];
	Pixmap icon = bm[FILES_BM];
	int n;

  if (fw == NULL) fw = popup_fw;

  if (!fw->n_selections) return;
	popups.fw = fw;
	freeze = True;	/* what's that for? */
	b[0] = '\0';
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
	   struct stat stats;
	   int i, toi, n_copied = 0;
	   char *from = NULL, to[MAXPATHLEN], todir[MAXPATHLEN];

	   strcpy(to, b);
	   fnexpand(to);

	   if (chdir(popups.fw->directory))

	     sysError("System error:");

	   else {

	      /* if target exists and is a directory, copy the source into that
	       directory */

	      if (!stat(to, &stats) && S_ISDIR(stats.st_mode)) {

		 if (chdir(to) || !getcwd(to, MAXPATHLEN) || chdir(popups.fw->directory)) {
		    sysError("System error:");
		    goto out;
		 } else if (!strcmp(popups.fw->directory, to)) {
		    error("Copy:", "Source and destination are identical");
		    goto out;
		 }

		 strcpy(todir, to);

		 toi = strlen(to);
		 if (to[toi-1] != '/') {
		    to[toi++] = '/';
		    to[toi] = '\0';
		 }

		 for (i=0; i < popups.fw->n_files; i++)
		   if (popups.fw->files[i]->selected) {
		      if (!strcmp(popups.fw->files[i]->name, ".") ||
			  !strcmp(popups.fw->files[i]->name, "..")) {
			 error("Cannot copy . or ..", "");
			 continue;
		      }
		      from = popups.fw->files[i]->name;
		      strcpy(to+toi, from);
		      if (exists(to) && resources.confirm_overwrite) {
			 char s[0xff];
			 sprintf(s, "Copy: file %s already exists at destination", from);
			 if (!confirm(s, "Overwrite?", "")) {
			   if (aborted) break;
			 else continue;
			 }
		      }
		      if (rcopy(from,to)) {
			 char s[0xff];
			 sprintf(s, "Error copying %s:", from);
			 sysError(s);
		      } else
			n_copied++;
		   }
	      }

	      /* otherwise only a single file may be selected; copy it to the target
	       file */

	      else if (popups.fw->n_selections > 1) {

		 error("Copy: target for multiple files", "must be a folder");
		 goto out;

	      } else {

		 struct stat stats1;

		 for (i = 0; i < popups.fw->n_files; i++)
		   if (popups.fw->files[i]->selected) {
		      from = popups.fw->files[i]->name;
		      break;
		   }

		 if (!strcmp(from, ".") || !strcmp(from, "..")) {
		    error("Cannot copy . or ..", "");
		    goto out;
		 } else if (!lstat(to, &stats) && !lstat(from, &stats1) &&
			    stats.st_ino == stats1.st_ino) {
		    error("Copy:", "Source and destination are identical");
		    goto out;
		 }

		 if (exists(to) && resources.confirm_overwrite) {
		    char s[0xff];
		    sprintf(s, "Copy: file %s already exists", to);
		    if (!confirm(s, "Overwrite?", ""))
		      goto out;
		 }

		 if (rcopy(from, to)) {
		    char s[0xff];
		    sprintf(s, "Error copying %s:", from);
		    sysError(s);
		 } else {
		    n_copied = 1;
		    dir_prefix(todir, to);
		    if ((*todir?chdir(todir):0) || !getcwd(todir, MAXPATHLEN))
		      sysError("System error:");
		 }
	      }

	      if (n_copied) {
		 markForUpdate(todir);
		 intUpdate();
	      }

	   }

	   out:
	   ;
	}
   	freeze = False;
}

/*---------------------------------------------------------------------------*/

void linkPopup(Widget w, FileWindowRec *fw, XtPointer call_data)
{
	char *t = "Link";
	char *p = "Link to:";
	char b[1024];
	Pixmap icon = bm[SYMLNK_BM];
	int n;

   if (fw == NULL) fw = popup_fw;

  if (!fw->n_selections) return;

	popups.fw = fw;
	freeze = True;	/* what's that for? */
	b[0] = '\0';
	n = MwDialogInputIcon(fw->shell, t, p, b, icon);

	if (n != ABORT) {
	   struct stat stats;
	   int i, namei, toi, n_linked = 0;
	   char *from = NULL, name[MAXPATHLEN], to[MAXPATHLEN], todir[MAXPATHLEN];

	   strcpy(to, b);
	   fnexpand(to);

	   strcpy(name, popups.fw->directory);

	   namei = strlen(name);
	   if (name[namei-1] != '/') {
	      name[namei++] = '/';
	      name[namei] = '\0';
	   }

	   if (chdir(popups.fw->directory))

	     sysError("System error:");

	   else {

	      /* if target exists and is a directory, link the source into that
	       directory */

	      if (!stat(to, &stats) && S_ISDIR(stats.st_mode)) {

		 if (chdir(to) || !getcwd(to, MAXPATHLEN) || chdir(popups.fw->directory)) {
		    sysError("System error:");
		    goto out;
		 } else if (!strcmp(popups.fw->directory, to)) {
		    error("Link:", "Source and destination are identical");
		    goto out;
		 }

		 strcpy(todir, to);

		 toi = strlen(to);
		 if (to[toi-1] != '/') {
		    to[toi++] = '/';
		    to[toi] = '\0';
		 }

		 for (i=0; i < popups.fw->n_files; i++)
		   if (popups.fw->files[i]->selected) {
		      from = popups.fw->files[i]->name;
		      strcpy(name+namei, from);
		      strcpy(to+toi, from);
		      if (exists(to) && resources.confirm_overwrite) {
			 char s[0xff];
			 sprintf(s, "Link: file %s already exists at destination", from);
			 if (!confirm(s, "Overwrite?", "")) {
			   if (aborted) break;
			 else continue;
			 }
		      }
		      if (symlink(name,to)) {
			 char s[0xff];
			 sprintf(s, "Error linking %s:", from);
			 sysError(s);
		      } else
			n_linked++;
		   }
	      }

	      /* otherwise only a single file may be selected; link it to the target
	       file */

	      else if (popups.fw->n_selections > 1) {

		 error("Link: target for multiple files", "must be a folder");
		 goto out;

	      } else {

		 struct stat stats1;

		 for (i = 0; i < popups.fw->n_files; i++)
		   if (popups.fw->files[i]->selected) {
		      from = popups.fw->files[i]->name;
		      break;
		   }

		 strcpy(name+namei, from);

		 if (!lstat(to, &stats) && !lstat(from, &stats1) &&
		     stats.st_ino == stats1.st_ino) {
		    error("Link:", "Source and destination are identical");
		    goto out;
		 }

		 if (exists(to) && resources.confirm_overwrite) {
		    char s[0xff];
		    sprintf(s, "Link: file %s already exists", to);
		    if (!confirm(s, "Overwrite?", ""))
		      goto out;
		 }

		 if (symlink(name, to)) {
		    char s[0xff];
		    sprintf(s, "Error linking %s:", from);
		    sysError(s);
		 } else {
		    n_linked = 1;
		    dir_prefix(todir, to);
		    if ((*todir?chdir(todir):0) || !getcwd(todir, MAXPATHLEN))
		      sysError("System error:");
		 }
	      }

	      if (n_linked) {
		 markForUpdate(todir);
		 intUpdate();
	      }

	   }

	   out:
	   ;
	}
   	freeze = False;
}
