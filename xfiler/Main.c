/*---------------------------------------------------------------------------
 Module FmMain

 (c) S.Marlow 1990-92
 (c) A.Graef 1994
 (c) R.Vogelgesang 1994 (`Xfm.BourneShells' stuff)
 (c) Ulric Eriksson 1998-2003

 main module for file manager    
---------------------------------------------------------------------------*/

#define XFMVERSION "1.3.2"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <unistd.h>

#ifdef _AIX
#include <sys/resource.h>
#endif

#include <sys/wait.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Shell.h>

#ifdef XPM
#include <X11/Xmu/Xmu.h>
#endif

#include "Files.h"

#include "../common/common.h"
#include "../xcommon/xcommon.h"
#include <Mowitz/Mowitz.h>

#define XtRDisplayType "DisplayType"
#define XtRSortType "SortType"
/*---------------------------------------------------------------------------
  Public variables
---------------------------------------------------------------------------*/

/* principal window */
Widget toplevel;

/* program name */
char *progname;

/* information about the user */
UserInfo user;

/* application resource values */
Resources resources;

/* application context */
XtAppContext app_context;

/* Update semaphor */
int freeze = False;

/*---------------------------------------------------------------------------
  Command line options
---------------------------------------------------------------------------*/

static XrmOptionDescRec options[] = {
  { "-version", ".version", XrmoptionNoArg, "True" }
};

/*---------------------------------------------------------------------------
  Application Resources
---------------------------------------------------------------------------*/

static XtResource resource_list[] = {
  { "version", "Version", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, version), XtRImmediate, (XtPointer) False },
  { "initGeometry", "InitGeometry", XtRString, sizeof(String),
      XtOffsetOf(Resources, init_geometry), XtRString, NULL },
  { "iconFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, icon_font), XtRString, XtDefaultFont },
  { "labelFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, label_font), XtRString, XtDefaultFont },
  { "boldFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, bold_font), XtRString, XtDefaultFont },
  { "cellFont", XtCFont, XtRFontStruct, sizeof(XFontStruct *), 
      XtOffsetOf(Resources, cell_font), XtRString, XtDefaultFont },
  { "fileIconWidth", "Width", XtRInt, sizeof(int),
      XtOffsetOf(Resources, file_icon_width), XtRImmediate, (XtPointer) 48 },
  { "fileIconHeight", "Height", XtRInt, sizeof(int),
      XtOffsetOf(Resources, file_icon_height), XtRImmediate, (XtPointer) 40 },
  { "treeIconWidth", "Width", XtRInt, sizeof(int),
      XtOffsetOf(Resources, tree_icon_width), XtRImmediate, (XtPointer) 48 },
  { "treeIconHeight", "Height", XtRInt, sizeof(int),
      XtOffsetOf(Resources, tree_icon_height), XtRImmediate, (XtPointer) 32 },
  { "bitmapPath", "Path", XtRString, sizeof(String),
      XtOffsetOf(Resources, bitmap_path), XtRString, "/usr/local/bitmaps:/usr/local/lib/bitmaps:/usr/include/X11/bitmaps" },
  { "pixmapPath", "Path", XtRString, sizeof(String),
      XtOffsetOf(Resources, pixmap_path), XtRString, PIXDIR ":/usr/local/pixmaps:/usr/local/lib/pixmaps:/usr/include/X11/pixmaps" },
  { "configFile", "ConfigFile",  XtRString, sizeof(String),
      XtOffsetOf(Resources, cfg_file_r), XtRString, RCFILE },
#ifdef MAGIC_HEADERS
  { "magicFile", "ConfigFile",  XtRString, sizeof(String),
      XtOffsetOf(Resources, magic_file_r), XtRString, MAGICFILE },
#endif
  { "confirmDeletes", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_deletes), XtRImmediate, (XtPointer) True },
  { "confirmDeleteFolder", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_delete_folder), XtRImmediate,
      (XtPointer) False },
  { "confirmMoves", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_moves), XtRImmediate, (XtPointer) False },
  { "confirmCopies", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_copies), XtRImmediate, (XtPointer) False },
  { "confirmOverwrite", "Confirm", XtRBoolean, sizeof(Boolean), 
      XtOffsetOf(Resources, confirm_overwrite), XtRImmediate,
      (XtPointer) True },
  { "confirmQuit", "Confirm", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, confirm_quit), XtRImmediate, (XtPointer) True },
  { "echoActions", "Echo", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, echo_actions), XtRImmediate, (XtPointer) False },
  { "showOwner", "ShowOwner", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_owner), XtRImmediate, (XtPointer) True },
  { "showDate", "ShowDate", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_date), XtRImmediate, (XtPointer) True },
  { "showPermissions", "ShowPermissions", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_perms), XtRImmediate, (XtPointer) True },
  { "showLength", "ShowLength", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, show_length), XtRImmediate, (XtPointer) True },
  { "defaultDisplayType", "DefaultDisplayType", XtRDisplayType, 
      sizeof(DisplayType), XtOffsetOf(Resources, default_display_type),
      XtRImmediate, (XtPointer) Icons },
  { "initialDisplayType", "InitialDisplayType", XtRDisplayType, 
      sizeof(DisplayType), XtOffsetOf(Resources, initial_display_type),
      XtRImmediate, (XtPointer) Icons },
  { "defaultSortType", "DefaultSortType", XtRSortType, 
      sizeof(SortType), XtOffsetOf(Resources, default_sort_type),
      XtRImmediate, (XtPointer) SortByName },
  { "doubleClickTime", "DoubleClickTime", XtRInt, sizeof(int),
      XtOffsetOf(Resources, double_click_time), XtRImmediate,
      (XtPointer) 300 },
  { "updateInterval", "UpdateInterval", XtRInt, sizeof(int),
      XtOffsetOf(Resources, update_interval), XtRImmediate,
      (XtPointer) 10000 },
  { "defaultEditor", "DefaultEditor", XtRString, sizeof(String),
      XtOffsetOf(Resources, default_editor), XtRString, NULL },
  { "defaultViewer", "DefaultViewer", XtRString, sizeof(String),
      XtOffsetOf(Resources, default_viewer), XtRString, NULL },
  { "defaultTerm", "DefaultTerm", XtRString, sizeof(String),
      XtOffsetOf(Resources, default_term), XtRString, NULL },
  { "defaultBrowser", "DefaultBrowser", XtRString, sizeof(String),
      XtOffsetOf(Resources, default_browser), XtRString, NULL },
  { "BourneShells", "ShellList", XtRString, sizeof(String),
      XtOffsetOf(Resources, sh_list), XtRString, 
		"/bin/sh,/bin/bash,/bin/ash,/bin/ksh,/bin/zsh,/usr/bin/sh,/usr/bin/bash,/usr/bin/ash,/usr/bin/ksh,/usr/bin/zsh" },
  { "iconic", "Iconic", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(Resources, iconic), XtRImmediate,  (XtPointer) False},
};

/*---------------------------------------------------------------------------
 Fallback resources
---------------------------------------------------------------------------*/

#define APPNAME "Xfiler"

static String fallback_resources[] = {
#include "../xcommon/xcommon-ad.h"
#include "../xcommon/dialogs-ad.h"
#include "../xcommon/nws-ad.h"
#include "xfiler-ad.h"
APPNAME "*bitmapPath: "PIXDIR,
APPNAME "*pixmapPath: "PIXDIR,
NULL
};

/*---------------------------------------------------------------------------
  Widget argument lists
---------------------------------------------------------------------------*/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) "Xfiler" }
};

/*-----------------------------------------------------------------------------
  Signal handler - clears up Zombie processes
  I'll probably extend this in the future to do something useful.
-----------------------------------------------------------------------------*/
static void sigcldHandler(int i)
{
  waitpid(-1,NULL,WNOHANG);
}

static struct sigaction sigcld, sigterm;

/*---------------------------------------------------------------------------
  Resource converter functions
---------------------------------------------------------------------------*/

static void CvtStringToDisplayType(XrmValue *args, Cardinal *n_args,
				   XrmValue *fromVal, XrmValue *toVal)
{
  static DisplayType d;

  if (!strcmp(fromVal->addr, "Tree"))
    d = Tree;
  else if (!strcmp(fromVal->addr, "Icons"))
    d = Icons;
  else if (!strcmp(fromVal->addr, "Text"))
    d = Text;
  else {
    XtStringConversionWarning(fromVal->addr, XtRDisplayType);
    return;
  }
  
  toVal->addr = (caddr_t) &d;
  toVal->size = sizeof(DisplayType);
}

/*---------------------------------------------------------------------------*/
 
static void CvtStringToSortType(XrmValue *args, Cardinal *n_args,
				XrmValue *fromVal, XrmValue *toVal)
{
  static SortType d;

  if (!strcmp(fromVal->addr, "SortByName"))
    d = SortByName;
  else if (!strcmp(fromVal->addr, "SortBySize"))
    d = SortBySize;
  else if (!strcmp(fromVal->addr, "SortByDate"))
    d = SortByMTime;
  else {
    XtStringConversionWarning(fromVal->addr, XtRSortType);
    return;
  }
  
  toVal->addr = (caddr_t) &d;
  toVal->size = sizeof(SortType);
}

/*---------------------------------------------------------------------------*/

#ifdef XPM
#define done(type, address)            \
{ toVal->size = sizeof(type); *(type *)(toVal->addr) = address; }

static Boolean
CvtStringToPixmap(Display *dpy, XrmValue *args, Cardinal *num_args,
		  XrmValue *fromVal, XrmValue *toVal,
		  XtPointer *converter_data)
{
  Pixmap pixmap;
  char *name = (char *) fromVal->addr;

  if (XmuCompareISOLatin1(name, "None") == 0) {
    pixmap = None;
    done(Pixmap, pixmap)
    return True;
  }
  if (XmuCompareISOLatin1(name, "ParentRelative") == 0) {
    pixmap = ParentRelative;
    done(Pixmap, pixmap)
    return True;
  }
  if (XmuCompareISOLatin1(name, "CopyFromParent") == 0) {
    pixmap = CopyFromParent;
    done(Pixmap, pixmap)
    return True;
  }
  if (XmuCompareISOLatin1(name, "XtUnspecifiedPixmap") == 0) {
    pixmap = XtUnspecifiedPixmap;
    done(Pixmap, pixmap)
    return True;
  }

  pixmap = readIcon(name);

  if (pixmap != None) {
    done(Pixmap, pixmap)
    return True;
  } else {
    XtStringConversionWarning(name, "Pixmap");
    return False;
  }
}
#undef done
#endif /* XPM */

/*---------------------------------------------------------------------------
  `Xfm.BourneShells' related functions  
---------------------------------------------------------------------------*/  

int shell_test(UserInfo *ui)
{
  int pipe_fd[2];
  int p;
  char val[3];

  if (pipe(pipe_fd) < 0) {
    perror("Can't create pipe");
    exit(1);
  }

  p = fork();
  if (p < 0) {
    perror("Can't fork");
    exit(1);
  }

  if (!p) {       /* child; exec the shell w/ test args */
    dup2(pipe_fd[1], fileno(stdout));
    if (close(pipe_fd[0]) == -1) {
      perror("(child) Can't close pipe");
      exit(1);
    }
    execlp(ui->shell, ui->shell, "-c", "echo $*", "1", NULL);
    perror("Exec failed");
    exit(1);
  } else {        /* parent; read and check the child's output */
    if (close(pipe_fd[1]) == -1) {
      perror("(parent) Can't close pipe");
      exit(1);
    }
    val[0] = '\0';
    while ((p = read(pipe_fd[0], val, 3)) < 0) {
      if (errno != EINTR) {
	perror("Reading child's output failed");
	exit(1);
      }
    }
    if (p == 3)
      return -1;
    ui->arg0flag = (val[0] != '1');
    return 0;
  }
}

char *get_first(char *s)
{
  char *p;

  p = strtok(s, ",");
  if (p != NULL)
    while (isspace(*p))
      p++;
  return p;
}

char *get_next()
{
  char *p;
  
  p = strtok((char *) NULL, ",");
  if (p != NULL)
    while (isspace(*p))
      p++;
  return p;
}

void init_arg0flag()
{
  if (resources.sh_list == NULL || !strcmp(resources.sh_list, "AUTO")) 
  {
    if (shell_test(&user) == -1) 
	{
      fprintf(stderr, "Xfm.BourneShells: AUTO test failed. Assuming Bourne-compatible shell\n");
	  user.arg0flag = 1;
      return;
    }
  } 
  else 
  {
    char *p;
	String list = XtNewString(resources.sh_list);
    for (p = get_first(list); p != NULL; p = get_next()) 
	{
      if (!strcmp(p, user.shell))
	  {
	    user.arg0flag = 1;
		XtFree(list);  
		return;
      }
	}
	XtFree(list);    
  }
}

/*---------------------------------------------------------------------------
  Main function
---------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
  char *s, *buf;
  int bufsize;
  XtArgVal iconic;
  Window files;
  Display *dpy;
	char cwd[1024];
  
  progname = argv[0];

  /* get some information about the user */
  user.uid = getuid();
  user.gid = getgid();

  if ((s = getenv("HOME")))
    strcpy(user.home,s);
  else
    getcwd(user.home, MAXPATHLEN);

  if ((s = getenv("SHELL")))
    strcpy(user.shell,s);
  else
    strcpy(user.shell,"/bin/sh");

  user.umask = umask(0);
  umask(user.umask);
  user.umask = 0777777 ^ user.umask;

	common_init("Xfiler %s. No Warranty.");

  /* initialise the application and create the application shell */
  toplevel = XtOpenApplication(&app_context, "Xfiler", options, XtNumber(options),
			     &argc, argv, fallback_resources,
			     mwApplicationShellWidgetClass,
			     shell_args, XtNumber(shell_args) );

  theme_init(XtDisplay(toplevel));

  /* make sure we can close-on-exec the display connection */
  if (fcntl(ConnectionNumber(XtDisplay(toplevel)), F_SETFD, 1) == -1)
    abortXfm("Couldn't mark display connection as close-on-exec");

  /* is 'files' already running? */
  dpy = XtDisplay(toplevel);
  if ((files = FindFilesWin(dpy,DefaultRootWindow(dpy)))) 
  {
      /* yes! Send messages to the running process */
      openRemoteDirs(dpy, files, --argc, ++argv);
      exit(0);
  }

  /* register resource converters */
  XtAppAddConverter(app_context, XtRString, XtRDisplayType, 
		    CvtStringToDisplayType, NULL, ZERO);
  XtAppAddConverter(app_context, XtRString, XtRSortType, 
		    CvtStringToSortType, NULL, ZERO);
#ifdef XPM
  XtAppSetTypeConverter(app_context, XtRString, XtRPixmap,
			CvtStringToPixmap, (XtConvertArgList) NULL,
			(Cardinal) 0, XtCacheAll, NULL);
#endif /* XPM */

	MwHighlightInit(toplevel);
	tooltip = XtVaCreatePopupShell("tooltip",
		mwTooltipWidgetClass, toplevel,
		(char *)0);

  /* get the application resources */
  XtGetApplicationResources(toplevel, &resources, resource_list,
			    XtNumber(resource_list), NULL, ZERO);

  /* -version: print version number and exit: */
  if (resources.version) {
    printf("xfm version %s\n", XFMVERSION);
    exit(0);
  }

  /* set the multi-click time */
  XtSetMultiClickTime(XtDisplay(toplevel), resources.double_click_time);

  /* initialise the utilities module */
  initUtils();

  /* set up signal handlers */
  sigcld.sa_handler = sigcldHandler;
  sigemptyset(&sigcld.sa_mask);
  sigcld.sa_flags = 0;
  sigaction(SIGCHLD,&sigcld,NULL);
  sigterm.sa_handler = quit;
  sigemptyset(&sigterm.sa_mask);
  sigterm.sa_flags = 0;
  sigaction(SIGTERM,&sigterm,NULL);

  /* initialise the communications module */
  initComms();

  /* check the user's shell; needs signal handlers (to avoid a zombie) */
  init_arg0flag();

  /* create all the bitmaps & cursors needed */
  readBitmaps();

  /* create the main popup shells */
  createMainPopups();

  /* initialise the file windows module & create a file window */
  strcpy(resources.cfg_file, resources.cfg_file_r);
  fnexpand(resources.cfg_file);
#ifdef MAGIC_HEADERS
  strcpy(resources.magic_file, resources.magic_file_r);
  fnexpand(resources.magic_file);
#endif
  initFileWindows();

  /* get working dir */
  bufsize = MAXPATHLEN;		
  if (!(buf = (char*)malloc(bufsize))) exit(1);
  while(!getcwd(buf, bufsize))
  {	
	bufsize += MAXPATHLEN;  
  	if (!(buf = (char*)realloc(buf, bufsize))) exit(1);
  }
  strcat(buf, "/");	
  s = buf+strlen(buf);
  /* we dont need argv[0] */
  argc--; argv++;
	/* Get the preferred initial state : IconicState or NormalState */
	XtVaGetValues(toplevel,XtNinitialState,&iconic,NULL);
	iconic = iconic==IconicState ? True : False ;
    /* uses the iconic resource */
    if (resources.iconic==True) iconic=True;

  getcwd(cwd, sizeof cwd);
  if (!argc) /* no paths in cmd line: start window with home dir */
  	newFileWindow(cwd/*user.home*/,resources.initial_display_type,False,iconic);
  /* start the file windows specified in cmd line */
  while(argc--)
	if ((*argv)[0]!='/')
	{			
		if (bufsize>strlen(buf)+strlen(*argv))
			strcat(buf, *argv++);  
		else
			if (!(buf = (char*)realloc(buf,strlen(buf)+strlen(*argv)+1))) 
		  		exit(1);
			else
				strcat(buf, *argv++);
 		newFileWindow(buf,resources.initial_display_type,False,iconic);
		s[0] = '\0'; /* go back to current dir */
	}		
	else
		newFileWindow(*argv++,resources.initial_display_type,False,
			      iconic);
  free(buf);
  resources.init_geometry = NULL;
	
  /* start up window refresh timer */
  if (resources.update_interval > 0)
    XtAppAddTimeOut(app_context, resources.update_interval, timeoutCb, NULL);

  /* Drag and drop stuff */
  MwDndInitialize(toplevel);
  MwDndRegisterRootDrop(RootDropEventHandler);
  MwDndMultipleShells();
	
  /* collect & process events */
  XtAppMainLoop(app_context);

	return 0;
}

/*---------------------------------------------------------------------------*/

void quit()
{
  int d;

  for (d = 0; d < n_devices; d++)
    if (devs[d].mounted) {
      devs[d].mounted = 1;
      umountDev(d);
    }
  XtDestroyApplicationContext(app_context);
  exit(0);
}
