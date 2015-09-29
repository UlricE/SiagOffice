/*---------------------------------------------------------------------------
  Module FmFwActions

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994

  Action procedures for widgets in a file window
---------------------------------------------------------------------------*/


#include <stdio.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Toggle.h>

#include "Files.h"

#include "../common/common.h"
/*---------------------------------------------------------------------------
  PUBLIC FUNCTIONS
---------------------------------------------------------------------------*/
int findWidget(Widget w, FileWindowRec **fw_ret)
{
  int i;
  FileWindowRec *fw;

  for (fw = file_windows; fw; fw = fw->next) {
    if (fw->icon_box == w /*|| fw->label == w*/) {
      *fw_ret = fw;
      return -1;
    }
    for (i = 0; i < fw->n_files; i++)
      if (fw->files[i]->icon.toggle == w) {
	*fw_ret = fw;
	return i;
      }
  }
  *fw_ret = NULL;
  return 0;
}

int findWindow(Window w, FileWindowRec **fw_ret)
{
    Widget w_source = XtWindowToWidget(XtDisplay(toplevel),w);
    return findWidget(w_source, fw_ret);
    
    /* 
    for(fw=file_windows; fw; fw=fw->next)
    {
	if(XtWindow(fw->icon_box)==w || XtWindow(fw->label)==w)
	{
	    *fw_ret=fw;
	    return -1;
	}
	for(i=0;i<fw->n_files;i++)
	    if(XtWindow(fw->files[i]->icon.toggle)==w)
	    {
		*fw_ret = fw;
		return i;
	    }
    }
    *fw_ret=NULL;
    return 0;
    */
}


void filePopup(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  int i;
  FileWindowRec *fw;
  Display *dpy;
  Window root, child;
  int x, y, x_win, y_win;
  unsigned int mask;

  i = findWidget(w, &fw);
  if (!fw) {
/*
    error("Internal error:", "widget not found in filePopup");
*/
    return;
  }
  popup_fw = fw;
  fileSelect(w, event, params, num_params);

  if (S_ISLNK(fw->files[i]->stats.st_mode)) {
    grayOut(file_popup_items[0]);
    grayOut(file_popup_items[1]);
  } else {
    fillIn(file_popup_items[0]);
    fillIn(file_popup_items[1]);
  }

  dpy = XtDisplay(toplevel);
  
  XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &child, &x, &y, 
		&x_win, &y_win, &mask);
  
  XtVaSetValues(file_popup_widget, XtNx, (XtArgVal) x, XtNy, (XtArgVal) y,
		NULL);
  
  XtPopupSpringLoaded(file_popup_widget);

}  

/*---------------------------------------------------------------------------*/

void dirPopup(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  int i;
  FileWindowRec *fw;
  Display *dpy;
  Window root, child;
  int x, y, x_win, y_win;
  unsigned int mask;

  i = findWidget(w, &fw);
  if (!fw) {
/*
    error("Internal error:", "widget not found in dirPopup");
*/
    return;
  }
  popup_fw = fw;
  fileSelect(w, event, params, num_params);

  if (!strcmp(fw->files[i]->name, ".") ||
      !strcmp(fw->files[i]->name, "..")) {
    grayOut(dir_popup_items[2]);
    grayOut(dir_popup_items[3]);
    grayOut(dir_popup_items[6]);
  } else {
    fillIn(dir_popup_items[2]);
    fillIn(dir_popup_items[3]);
    fillIn(dir_popup_items[6]);
  }

  dpy = XtDisplay(toplevel);
  
  XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &child, &x, &y, 
		&x_win, &y_win, &mask);
  
  XtVaSetValues(dir_popup_widget, XtNx, (XtArgVal) x, XtNy, (XtArgVal) y,
		NULL);
  
  XtPopupSpringLoaded(dir_popup_widget);
}  

/*---------------------------------------------------------------------------*/

void fileToggle(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  int i;
  FileWindowRec *fw;
  Pixel pix;
  extern int Faking;
	
  i = findWidget(w, &fw);
  if (!fw) {
/*
    error("Internal error:", "widget not found in fileToggle");
*/
    return;
  }
	if(Faking)
	{
		fileRestoreSelect(w, event, NULL, 0);
		Faking =0;
	}
	
  XtVaGetValues(w, fw->files[i]->selected?XtNbackground:XtNforeground, &pix,
		NULL);
  XtVaSetValues(w, XtNborder, (XtArgVal) pix, NULL);

  if (fw->files[i]->selected) {
    fw->files[i]->selected = False;
    fw->n_selections--;
    fw->n_bytes_selected -= fw->files[i]->stats.st_size;
  }
  else {
    fw->files[i]->selected = True;
    fw->n_selections++;
    fw->n_bytes_selected += fw->files[i]->stats.st_size;
  }
  updateStatus(fw);
}

/*---------------------------------------------------------------------------*/

void fileSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  int i, j;
  FileWindowRec *fw;
  Pixel back, fore;

  j = findWidget(w, &fw);
  if (!fw) {
/*
    error("Internal error:", "widget not found in fileSelect");
*/
    return;
  }
  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->selected) {
      XtCallActionProc(fw->files[i]->icon.toggle, "unset", event, NULL, 0);
      XtVaGetValues(fw->files[i]->icon.toggle, XtNbackground, &back, NULL);
      XtVaSetValues(fw->files[i]->icon.toggle, XtNborder, (XtArgVal) back,
		    NULL);
      fw->files[i]->selected = False;
    }

  XtVaGetValues(w, XtNforeground, &fore, NULL);
  XtVaSetValues(w, XtNborder, (XtArgVal) fore, NULL);

  fw->files[j]->selected = True;
  fw->n_selections = 1;
  fw->n_bytes_selected = fw->files[j]->stats.st_size;
  updateStatus(fw);
}

void fileFakeSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  int i, j;
  FileWindowRec *fw;
  Pixel back, fore;

  j = findWidget(w, &fw);
  if (!fw) {
/*
    error("Internal error:", "widget not found in fileSelect");
*/
    return;
  }
  for (i=0; i<fw->n_files; i++)
    if (fw->files[i]->selected) {
      XtCallActionProc(fw->files[i]->icon.toggle, "unset", event, NULL, 0);
      XtVaGetValues(fw->files[i]->icon.toggle, XtNbackground, &back, NULL);
      XtVaSetValues(fw->files[i]->icon.toggle, XtNborder, (XtArgVal) back,
		    NULL);
    }

  XtVaGetValues(w, XtNforeground, &fore, NULL);
  XtVaSetValues(w, XtNborder, (XtArgVal) fore, NULL);
}

void fileRestoreSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  int i, j;
  FileWindowRec *fw;
  Pixel back, fore;

  j = findWidget(w, &fw);
  if (!fw) {
/*
    error("Internal error:", "widget not found in fileSelect");
*/
    return;
  }
  	for (i=0; i<fw->n_files; i++)
	{
    	XtCallActionProc(fw->files[i]->icon.toggle, "unset", event, NULL, 0);
    	XtVaGetValues(fw->files[i]->icon.toggle, XtNbackground, 
					  							&back, NULL);
    	XtVaSetValues(fw->files[i]->icon.toggle, XtNborder, 
					  							(XtArgVal) back,NULL);
		if (fw->files[i]->selected) 
		{	  
			XtVaGetValues(fw->files[i]->icon.toggle, XtNforeground, 
						  						&fore, NULL);
			XtVaSetValues(fw->files[i]->icon.toggle, XtNborder, 
						  						(XtArgVal) fore, NULL);
    	}		  
	}
	
}

/*---------------------------------------------------------------------------*/

void fileRefresh(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  FileWindowRec *fw;
  
  findWidget(w, &fw);
  if (fw)
    updateFileDisplay(fw);
}

/*---------------------------------------------------------------------------*/

void fileOpenDir(Widget w, XEvent *event, String *params,Cardinal *num_params)
{
  FileWindowRec *fw;
  int i;
  char path[MAXPATHLEN];

  i = findWidget(w, &fw);
  if (!fw)
    return;
  if (chdir(fw->directory) || chdir(fw->files[i]->name))
    sysError("Can't open folder:");
  else if (!getcwd(path, MAXPATHLEN))
    sysError("System error:");
  else {
    strcpy(fw->directory, path);
    updateFileDisplay(fw);
  }
}
  
/*---------------------------------------------------------------------------*/

void fileExecFile(Widget w, XEvent *event, String *params, 
		  Cardinal *num_params)
{
  int i, l;
  char *path;
  char **argv;
  FileWindowRec *fw;

  i = findWidget(w, &fw);
  if (i == -1 || !fw) {
/*
    error("Internal error:","widget not found in fileExecFile");
*/
    return;
  }

  l = strlen(fw->directory);
  path = (char *)alloca(l+strlen(fw->files[i]->name)+2);
  strcpy(path, fw->directory);
  if (path[l-1] != '/')
    path[l++] = '/';
  strcpy(path+l, fw->files[i]->name);
  argv = (char **) XtMalloc(2 * sizeof(char *));
  argv[0] = XtNewString(fw->files[i]->name);
  argv[1] = NULL;
  executeApplication(path, fw->directory, argv);

  freeArgv(argv);
}

/*---------------------------------------------------------------------------*/
void fileExecAction(Widget w, XEvent *event, String *params, 
		    Cardinal *num_params)
{
  int i;
  FileWindowRec *fw;
   char **argv;

  i = findWidget(w, &fw);
  if (i == -1 || !fw) {
/*
    error("Internal error:","widget not found in fileExecAction");
*/
    return;
  }

  if (fw->files[i]->type) 
  {
    if (*fw->files[i]->type->push_action)
      if (!strcmp(fw->files[i]->type->push_action, "EDIT"))
		doEdit(fw->directory, fw->files[i]->name);
      else if (!strcmp(fw->files[i]->type->push_action, "VIEW"))
		doView(fw->directory, fw->files[i]->name);
      else if (!strcmp(fw->files[i]->type->push_action, "IMAGE"))
		doImage(fw->directory, fw->files[i]->name);
      else if (!strcmp(fw->files[i]->type->push_action, "SIAGHELP"))
		doSiaghelp(fw->directory, fw->files[i]->name);

	  else 
	  {
        int k = 0;
	    char *action = varPopup(fw->files[i]->type->icon_bm,
				fw->files[i]->type->push_action);

	 	if (!action) return;

		argv = (char **) XtMalloc( (user.arg0flag ? 6 : 5) * sizeof(char *));
		argv[k++] = user.shell;
		argv[k++] = "-c";
		argv[k++] = action;
        if (user.arg0flag)
          argv[k++] = user.shell;
		argv[k++] = fw->files[i]->name;
		argv[k] = NULL;

		executeApplication(user.shell, fw->directory, argv);

		XTFREE(argv);
      }
  } 
  else
    doEdit(fw->directory, fw->files[i]->name);
}

void doSiaghelp(char *directory, char *fname)
{
	char b[1024];

	sprintf(b, "siagrun help file://%s/%s", directory, fname);
	MwSpawn(b);
}

void doImage(char *directory, char *fname)
{
	char image[1024];
	char b[1024], c[1024];

	sprintf(image, "%s/plugins/image", libdir);
	sprintf(b, "%s/%s", directory, fname);
	MwQuotecpy(c, b, "\\\"\'$?*|&;()<>[]{}! \t\n\r");
	sprintf(b, "%s -fit %s", image, c);
	MwSpawn(b);
}

/*---------------------------------------------------------------------------*/
void doEdit(char *directory, char *fname)
{
  char path[MAXPATHLEN];
  char **argv;

  if (resources.default_editor) {

    sprintf(path, "siagrun editor");
    strcat(path, " ");
    strcat(path, fname);

    argv = (char **) XtMalloc(4 * sizeof(char *));
    argv[0] = user.shell;
    argv[1] = "-c";
    argv[2] = path;
    argv[3] = NULL;

    executeApplication(user.shell, directory, argv);
  
    XTFREE(argv);
  }

  else
    error("No default editor", "");
}

/*---------------------------------------------------------------------------*/
void doOpenWith(char *directory, char *fname, char *with)
 {
   char path[MAXPATHLEN];
   char **argv;
 
 
     strcpy(path, with);
     strcat(path, " ");
     strcat(path, fname);
 
     argv = (char **) XtMalloc(4 * sizeof(char *));
     argv[0] = user.shell;
     argv[1] = "-c";
     argv[2] = path;
     argv[3] = NULL;
 
     executeApplication(user.shell, directory, argv);
   
     XTFREE(argv);
 }

/*---------------------------------------------------------------------------*/
void doView(char *directory, char *fname)
{
  char path[MAXPATHLEN];
  char **argv;

  if (resources.default_viewer) {

    sprintf(path, "siagrun editor");
    strcat(path, " ");
    strcat(path, fname);

    argv = (char **) XtMalloc(4 * sizeof(char *));
    argv[0] = user.shell;
    argv[1] = "-c";
    argv[2] = path;
    argv[3] = NULL;

    executeApplication(user.shell, directory, argv);
  
    XTFREE(argv);
  }

  else
    error("No default viewer", "");
}

/*---------------------------------------------------------------------------*/
void doComHere(char *directory, char *fname)
{
  char path[MAXPATHLEN];
  char **argv;
  char default_term[1000];

  if (resources.default_term) {
    strcpy(path, "cd ");
    strcat(path, fname);
    strcat(path, "&&");
    sprintf(default_term, "siagrun terminal");
    strcat(path, default_term);

    argv = (char **) XtMalloc(4 * sizeof(char *));
    argv[0] = user.shell;
    argv[1] = "-c";
    argv[2] = path;
    argv[3] = NULL;

    executeApplication(user.shell, directory, argv);
  
    XTFREE(argv);
  }

  else
    error("No default terminal program", "");
}
