#include <X11/Intrinsic.h>
#include "Files.h"
#include <stdlib.h>
#include <unistd.h>
#include <sys/file.h>

extern void ExternalApplication(Display *dpy,int x, int y, char *label, 
			    char *icon, char *cmd, char *drop, char *dir);

static void executeAction(char *action, char *filename, 
			  char *workingdir, char*args);
static void executeFile(char *file, char *workingdir, char*args);
static int handleDndData(XEvent* event, char **Data, unsigned long* Size);
static void moveOrCopyToDir(char *files, char* to, char* workingdir, int move);
static void RootDropCommand(FileRec *file, char *directory, int type);
static void handleDataDrop(char *dir, char *data, unsigned long size);

int Faking = 0;

/*---------------------------------------------------------------------------
  PUBLIC FUNCTIONS
---------------------------------------------------------------------------*/
void DragEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
    int i, indice, n, size, type;
    FileWindowRec *fw;
    char *os_dados=NULL, path[MAXPATHLEN];
	
    indice = findWidget(widget, &fw);
	
    if (!fw)  return;
    if (indice == -1) return;

    size = strlen(fw->directory);
    strcpy(path,fw->directory);
    if (path[size-1] != '/')
    {
	path[size++] = '/';
	path[size] = '\0';
    }
    if (!Faking && !fw->files[indice]->selected)
	Faking = 1;
    if(Faking)
	fileFakeSelect(widget, event, NULL,0);
	
    if (fw->n_selections == 1 || Faking)
    {
	int type;
	if (S_ISDIR(fw->files[indice]->stats.st_mode)) 
	    type = MW_DndDir;
	else if (S_ISLNK(fw->files[indice]->stats.st_mode))
	    type = MW_DndLink;  /* Link */
	else if (fw->files[indice]->stats.st_mode & 
		 (S_IXUSR | S_IXGRP | S_IXOTH))
	    type = MW_DndExe;   /* Executable */
	else type = MW_DndFile;
  
	os_dados = (char *)malloc( size + strlen(fw->files[indice]->name) + 1);
	strcpy(os_dados, path);
	strcat(os_dados, fw->files[indice]->name);
	MwDndSetData(type, os_dados, (unsigned long)(strlen(os_dados)+1));
    }	
    else
    {
	char *beginning;
	type = MW_DndFiles;
	/* How much memory do we have to alloc */
	for (i=0, n=0; i<fw->n_files; i++)
	    if (fw->files[i]->selected) 
		n += strlen(fw->files[i]->name) + size + 1;
	/* do it */
	os_dados = (char *)calloc(1, n+1);
	/* fill in the data */
	beginning = os_dados;
	for (i=0; i<fw->n_files; i++)
	    if (fw->files[i]->selected)
	    {
		strcpy(os_dados, path);
		strcat(os_dados, fw->files[i]->name);
		os_dados += strlen(os_dados) + 1;
	    }
	MwDndSetData(type, beginning, (unsigned long)(n+1));
	os_dados = beginning;
    }	
    if(MwDndHandleDragging(widget, event))
    {
	if (Faking)
	{
	    Faking = 0;
	    fileSelect(widget, event, NULL, 0);
	}
	if (!freeze) intUpdate();
    }
	
    if (os_dados)
	free(os_dados);
}

void RootDropEventHandler(Widget widget,XtPointer data,XEvent* event,Boolean* b)
{
    int i, type;
    FileWindowRec *fw_source;	

    if (MwDndDataType(event)==MW_DndNotDnd) return;
	
    /* where did it come from */	
    i = findWindow(MwDndSourceWindow(event), &fw_source);	
    if (!fw_source) 
    {
	error("General Protection Fault :-)", "Please, try again!");
	return;
    }
    type = MwDndDataType(event);
    /* Open directories if dragged onto the root window */	
    /* if (type == MW_DndExe || type == MW_DndDir) */
    RootDropCommand(fw_source->files[i], fw_source->directory, type);

}

void IconBoxDropEventHandler(Widget widget, XtPointer d,
			     XEvent* event, Boolean* b)
{
    int i, button, type;
    FileWindowRec *fw, *fw_source;
    int move;
    char *data, to[MAXPATHLEN];
    unsigned long size;
  
    if ((type=handleDndData(event, &data, &size))==-1)
	return;
  
    /* where did it come from */	
    findWindow(MwDndSourceWindow(event), &fw_source);	
    /* which buttons were pressed */	
    button = MwDndDragButtons(event);
    move = button & Button1Mask ? True : False;
    /* who am I */
    i =  findWidget(widget, &fw);  /* i equals -1 here */
    if (!fw) 
    {
	error("General Protection Fault :-)", "Please, try again!");
	return;
    }

    if(type == MW_DndText || type == MW_DndRawData)
    {
	handleDataDrop(fw->directory, data, size);
	return;
    }

    if (!fw_source)
	/* dropped from another application */
	return;
    strcpy(to, fw->directory);
    /* strcat(to, "/");*/
    if (fw != fw_source)
	moveOrCopyToDir(data, to, fw_source->directory, move);
}

void DropEventHandler(Widget widget,XtPointer d,XEvent* event,Boolean* b)
{
    int i, button, type;
    FileWindowRec *fw, *fw_source;
    FileRec* item;
    int move;
    int is_item_droppable;
    char *data=NULL;
    char to[1024];
    unsigned long size;

    if ((type=handleDndData(event, &data, &size))==-1)
	return;

    /* which buttons were pressed */	
    button = MwDndDragButtons(event);
    move = button & Button1Mask ? True : False;
    /* who am I */
    i =  findWidget(widget, &fw);
    if (!fw || i < 0) 
    {
	/* i < 0      --> widget is the label or the icon_box
	 * fw == NULL --> widget does not belong to this file_window
 	 */
	error("General Protection Fault :-)", "Please, try again!");
	return;
    }
    item = fw->files[i];
    /* where did it come from */	
    findWindow(MwDndSourceWindow(event), &fw_source);

    strcpy(to, fw->directory);
    strcat(to, "/");
    strcat(to, item->name);

    if(type == MW_DndText || type == MW_DndRawData)
    {
	/* ignore drops over items other than directories */
	if (S_ISDIR(item->stats.st_mode))
	    handleDataDrop(to, data, size);
	else
	    if (data) free(data);
	return;
    }

    /* Self drops occur when the user wants to cancel the drag
     * operation.  Self drops are performed onto the same file window
     * where the corresponding drag occurred but not over an
     * unselected droppable item. Droppable items are directories,
     * executable files and any file with an associated drop action.
     * Therefore, self drops are ignored.  
     */
    is_item_droppable = (S_ISDIR(item->stats.st_mode) ||
			 (item->stats.st_mode & (S_IXUSR|S_IXGRP|S_IXOTH)) ||
			 (item->type && *item->type->drop_action));
    
    if (fw == fw_source && (item->selected || !is_item_droppable))
	/* self drop -> do nothing */
	;	
    /* Otherwise, if dragged onto a directory, do the move */
    else if (S_ISDIR(item->stats.st_mode))
    {
	if (fw_source)
	    moveOrCopyToDir(data, to, fw_source->directory, move);
	else 
	    return;
    }
    /* Otherwise, if the file has a drop action, invoke the action */
    else if (item->type && *item->type->drop_action)
    { 
	char *action = varPopup(item->type->icon_bm, item->type->drop_action);
	executeAction(action, to, fw->directory, data);
    }
    /* Otherwise, if the file is executable */
    else if (item->stats.st_mode & (S_IXUSR|S_IXGRP|S_IXOTH))
	executeFile(to,fw->directory,data);
    /* Otherwise, it is a normal file (or other window) so just do the move */
    else
	;  /* better not do anything */
    
    if(data) free(data);
}

/*---------------------------------------------------------------------------
  PRIVATE FUNCTIONS
---------------------------------------------------------------------------*/
/* these functions receive a parameter which is a list of files. Each
 * entry is a absolute filename and entries are separeted by '\0'. The
 * list ends with a double '\0'. 
 */


/* executeAction
 *   a fork is made to execute `action` with `filename` and `args` as 
 * arguments. `cd workingdir` is done before executing. If `filename` 
 * is NULL, it is ignored.
 */
static void executeAction(char *action, char *filename, 
			  char *workingdir, char*args)
{
    char *tmp, **argv;
    int size, i;
    
    if (!action) return;
    
    /* How many args do we have */	
    size = user.arg0flag ? 5 : 4;
    tmp = args;
    while (*tmp != '\0')
    {
	tmp += strlen(tmp) + 1;
	size++;
    }
    if (!filename)
	size--;
    /* alloc space for args */
    argv = (char **) malloc( (size + 1) * sizeof(char *));
    /* fill in with shell parameters */
    i=0;
    argv[i++] = XtNewString(user.shell);
    argv[i++] = XtNewString("-c");
    argv[i++] = XtNewString(action);
    if (user.arg0flag)
	argv[i++] = XtNewString(user.shell);
    if (filename)
	argv[i++] = XtNewString(filename);
    /* now, we are going to append the files to the command */
    tmp = args;
    while (*tmp != '\0')
    {
	argv[i++] = XtNewString(tmp);
	tmp += strlen(tmp) + 1;
    }	
    argv[size] = NULL;
    /********** execute the action ************/
    executeApplication(user.shell, workingdir, argv);
    /* now, free argv */
    for (i=0; argv[i]; i++)
    	XTFREE(argv[i]);
    XTFREE(argv);
}

static void executeFile(char *file, char *workingdir, char*args)
{
    char *tmp, **argv;
    int size, i;
    
    /* How many args do we have */	
    size = 1;
    tmp = args;
    while (*tmp != '\0')
    {
	tmp += strlen(tmp) + 1;
	size++;
    }
    /* alloc space for args */
    argv = (char **) malloc( (size + 1) * sizeof(char *));
    /* fill in with shell parameters */
    i=0;
    argv[i++] = XtNewString(file);
    /* now, we are going to append the files to the command */
    tmp = args;
    while (*tmp != '\0')
    {
	argv[i++] = XtNewString(tmp);
	tmp += strlen(tmp) + 1;
    }	
    argv[size] = NULL;
    /********** execute the action ************/
    executeApplication(file, workingdir, argv);
    /* now, free argv */
    for (i=0; argv[i]; i++)
    	XTFREE(argv[i]);
    XTFREE(argv);
}

/* moveOrCopyToDir
 *   `files` are moved or copied from `workingdir` to `to`. `files` should
 * have `workingdir` as a prefix (sorry about that).
 */
static void moveOrCopyToDir(char *files, char* to, char* workingdir, int move)
{
    int n_files;
    char *tmp;
    int n_done;
    int len_to;
    char destination[MAXPATHLEN];
	
    if (!strcmp(workingdir, to)) 
    {
    	error(move?"Move:":"Copy:", "Source and destination are identical");
	return;
    }
    if (access(to, W_OK)) 
    {
    	error("No write access to this directory", to);
	return;
    }

    /* count how many files we will move/copy */
    n_files =0;
    for (tmp=files; *tmp != '\0'; tmp += strlen(tmp) + 1)
	n_files++;

    if (resources.confirm_moves) 
    {
	char s1[MAXPATHLEN], s2[MAXPATHLEN], s3[MAXPATHLEN];
	sprintf(s1, "%s %d item%c", move ? "Moving":"Copying",
		n_files, n_files>1 ? 's':' ');
	sprintf(s2, "from: %s", workingdir);
	sprintf(s3, "to: %s", to);
	if (!confirm(s1, s2, s3))
	    return;
    }

    len_to = strlen(to);
    if (to[len_to-1] != '/') 
    {
	to[len_to++] = '/';
	to[len_to] = '\0';
    }
    strcpy(destination, to); 

    freeze = True;
    for (tmp=files, n_done=0; *tmp != '\0'; tmp += strlen(tmp) + 1)
    {
	if (!strcmp(tmp, ".") || !strcmp(tmp, "..")) 
	{
	    char s[0xff];
	    sprintf(s, "Cannot %s . or ..", move?"move":"copy");
	    error(s, "");
	    continue;
	}

	destination[len_to] = '\0';
	strcat(destination, tmp+strlen(workingdir)+1);
	if (exists(destination) && resources.confirm_overwrite) 
	{
	    char s[MAXPATHLEN];
	    sprintf(s, "%s file %s already exists at destination",
		    move?"Move:":"Copy:", tmp+strlen(workingdir)+1);
	    if (!confirm(s, "Overwrite?", "")) {
		if (aborted) break;
		else continue;
	    }
	}
	if (move ? rename(tmp,destination) : rcopy(tmp,destination)) 
	{
	    char s[MAXPATHLEN];
	    sprintf(s, "Error %s %s:", move?"moving":"copying", tmp);
	    sysError(s);
	} 
	else
	    n_done++;	
    }	
    if(n_done)
    {	  
	markForUpdate(to); 
	if(move)  markForUpdate(workingdir);
	intUpdate();
    }
    freeze = False;
}

void RootDropCommand(FileRec *file, char *directory, int type)
{
    char *data;
    Window root, child;
    int x, y, x_win, y_win, size, i;
    unsigned int mask;
    char label[200], cmd[1024], drop[1024], icon[1024];
    Display *dpy = XtDisplay(toplevel);
    
    XQueryPointer(dpy, DefaultRootWindow(dpy), &root, &child, &x, &y, 
		  &x_win, &y_win, &mask);
    MwDndGetData((XtPointer)&data,(XtPointer)&size);
    strcpy(cmd, data);
    strcpy(label, file->name);
    searchPath(icon, resources.pixmap_path,ROOTDROP_DEFAULT_ICON);
    strcpy(drop,"None");
    
    if (file->type)
    {
#ifdef XPM			  
	if (*file->type->icon)
	    searchPath(icon, resources.pixmap_path,file->type->icon);
#endif
	if (type == MW_DndDir)
	{
	    if (resources.default_browser)
		strcpy(cmd,  resources.default_browser);
	    else
		strcpy(cmd, "files");
	    strcat(cmd,	" ");
	    strcat(cmd,	directory);
	    strcat(cmd,	"/");
	    strcat(cmd,	file->name); 
	    strcpy(drop, "cp $DROPOBJECT");
	    strcat(drop, "  ");
	    strcat(drop, directory);
	    strcat(drop, "/");
	    strcat(drop,	file->name);
	}
	else if (*file->type->push_action)
	{			  				
	    if (strcmp(file->type->push_action, "EDIT")||
		strcmp(file->type->push_action, "VIEW"))
		strcpy(cmd,	file->type->push_action);
	    
	    if (!strcmp(cmd, "EDIT"))
	    {
		sprintf(cmd, "siagrun editor");
		strcat(cmd,	" ");
		strcat(cmd,	directory);
		strcat(cmd,	"/");
		strcat(cmd,	file->name); 
	    }
	    else 
	    {
		strcpy(cmd,file->type->push_action);
		if (strchr(cmd,'$'))
		{
		    for (i=0; i<3; i++)
			cmd[strlen(cmd)-i] = 0;
		    strcat(cmd,	" ");
		    strcat(cmd,	directory);
		    strcat(cmd,	"/");
		    strcat(cmd,	file->name); 
		}
	    }
	    if (*file->type->drop_action)
	    {
		for (i=0; i<3; i++)
		    (file->type->push_action)[strlen(file->type->push_action)-i]=0;
		sprintf(drop, "%s $DROPOBJECT", file->type->push_action);
	    }
	}
    }
    if (type==MW_DndDir)
    {
	char dir[MAXPATHLEN];
	strcpy(dir, directory);
	strcat(dir, "/");
	strcat(dir, file->name); 
	ExternalApplication(dpy,x,y,label,icon,cmd,drop,dir);
    }
    else
	ExternalApplication(dpy,x,y,label,icon,cmd,drop,NULL);
}	

/* handleDndData
 *   get dnd data and trow it into `Data`. If DndType != similar(DndFile),
 * add an extra '\0' at the end. Caller must `free(Data)`
 */
int handleDndData(XEvent* event, char **Data, unsigned long *Size)
{
    char *dnd_data=NULL;
    int type=MwDndDataType(event);    

    if (type==MW_DndNotDnd) return -1;
    
    MwDndGetData((XtPointer)&dnd_data,Size);
    switch(type)
    {
    case MW_DndFile:
    case MW_DndDir:
    case MW_DndLink:
    case MW_DndExe:
	*Data = (char *)calloc(sizeof(char), strlen(dnd_data)+2);
	strcpy(*Data, dnd_data);
	(*Data)[strlen(*Data)+1]='\0';
	free(dnd_data);
	break;
    case MW_DndFiles:
    case MW_DndText:
    case MW_DndRawData:
	*Data = dnd_data;
	break; 
	/* case MW_DndURL:
	 * case MW_DndMIME:
	 */
    default: 
	{
	    free(dnd_data);
	    error("Drag and Drop error", "Cannot handle this drop!");
	    return -1;
	}
    }
    return type;
}

/* handleDataDrop
 *    create a new file at 'dir' and throw `data` into it.
 * If `dir` is NULL, just free `data`
 */
static void handleDataDrop(char *dir, char *data, unsigned long size)
{
    char *t;
    FILE *fp;
    if (!dir)
    {
	if (data) free(data);
	return;
    }
    t = tempnam(dir, "New");
    fp = fopen(t, "w");
    if(!fp || fwrite(data, 1, size, fp)!=size)
	error("Data transfer: Cannot write file ", t);
    if (fp) fclose(fp);
    if (data) free(data);
    if (t) free(t);
    markForUpdate(dir); 
    intUpdate();
    return;
}
