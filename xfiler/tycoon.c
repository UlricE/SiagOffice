#include <X11/Intrinsic.h>
#include <X11/Xatom.h>
#include <stdio.h>

extern void ExecExternalApplication(Display *dpy, int x, int y, char *label, 
				    char *icon,char *cmd,char *drop,char *dir);


/*
 * Create atoms to pass info to tycoon.
 */ 
#define INSTANCE_NAME     "TYCOON"
Atom  XA_TYCOON;
Atom  XA_KILL_ACTIVE;
Atom  XA_ADD_ICON;
Atom  XA_EDIT_ICON;
Atom  XA_KILL;
Atom  XA_ALIGN;
Atom  XA_FREEZE;
Atom  XA_UNFREEZE;
Atom  XA_LOWER;
Atom  XA_RAISE;

/* Assign cut buffers for tycoon data */
#define X_COORD       1
#define Y_COORD       2
#define LABEL         3
#define XPMFILE       4
#define COMMAND       5
#define DROP_CMD      6

/*
 * Check for presence of tycoon on this display
 */ 
static Window FindSomeIconWin (dpy)
Display *dpy;
{
    int i;
    Window root = RootWindowOfScreen (DefaultScreenOfDisplay (dpy));
    Window root2, parent, *kids;
    unsigned int nkids;
    
    /*
     * Get the whole window tree (starting from root window)
     */ 
    if (! XQueryTree (dpy, root, &root2, &parent, &kids, &nkids))
      abort ();
    if (root != root2)
      abort ();
    if (parent)
      abort ();
    if (! (kids && nkids))
      abort ();
    
    /*
     * Check each kid
     */ 
    for (i = 0; i < nkids; i++)
	{
	    Atom type;
	    int format;
	    unsigned long nitems, bytesafter;
	    char *version;
	    
	    if (XGetWindowProperty (dpy, kids[i],
				    XInternAtom (dpy, INSTANCE_NAME, False),
				    0, 1, False, XA_STRING,
				    &type, &format, &nitems, &bytesafter,
				    (unsigned char **) &version)
		== Success && type != None)
	      
	      /*
	       * Ok - found tycoon
	       */ 
	      return kids[i];
	}
    
    /*
     * Tycoon wasn't detected
     */ 
    return 0;
}

void MakeAtoms (Display * dpy)
{
  static int made=0;
  if (made) return;
  made=1;
  XA_TYCOON      = XInternAtom (dpy, INSTANCE_NAME, False);
  XA_KILL_ACTIVE = XInternAtom (dpy, "_KILL_ACTIVE", False);
  XA_ADD_ICON    = XInternAtom (dpy, "_ADD_ICON", False);
  XA_EDIT_ICON   = XInternAtom (dpy, "_EDIT_ICON", False);
  XA_KILL        = XInternAtom (dpy, "_KILL", False);
  XA_ALIGN       = XInternAtom (dpy, "_ALIGN", False);
  XA_FREEZE      = XInternAtom (dpy, "_FREEZE", False);
  XA_UNFREEZE    = XInternAtom (dpy, "_UNFREEZE", False);
  XA_RAISE       = XInternAtom (dpy, "_RAISE", False);
  XA_LOWER       = XInternAtom (dpy, "_LOWER", False);
}

void ExternalApplication(Display *dpy, int x, int y, char *label, 
			 char *icon, char *cmd, char *drop, char *dir)
{
	char X[20],Y[20];
	Window tycoonwin;
	XEvent event;
	
	MakeAtoms(dpy);
	tycoonwin=FindSomeIconWin(dpy);
	if (tycoonwin == 0)
	{
	    ExecExternalApplication(dpy,x,y,label,icon,cmd,drop,dir);
	    return;
	}
	
	sprintf(X,"%i",x);
	sprintf(Y,"%i",y);
	XStoreBuffer(dpy,X,
		     strlen(X)+1,X_COORD);
	XStoreBuffer(dpy,Y, 
		     strlen(Y)+1,Y_COORD);
	XStoreBuffer(dpy,label, 
		     strlen(label)+1,LABEL);
	XStoreBuffer(dpy,icon, 
		     strlen(icon)+1,XPMFILE);
	XStoreBuffer(dpy,cmd, 
		     strlen(cmd)+1,COMMAND);
	XStoreBuffer(dpy,drop, 
		     strlen(drop)+1,DROP_CMD);
	event.xany.type = ClientMessage;
	event.xclient.display = dpy ;
	event.xclient.window = tycoonwin;
	event.xclient.format = 32;
	event.xclient.message_type = XInternAtom (dpy, "_ADD_ICON", False);
	XSendEvent (dpy, tycoonwin, False, 0L, &event);
}
