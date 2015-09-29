/*-----------------------------------------------------------------------------
  FmComms.c

  (c) Simon Marlow 1990-1993
  (c) Albert Graef 1994

  support for receiving instructions from other X processes
------------------------------------------------------------------------------*/

#include <stdlib.h>
#include <unistd.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include "Files.h"
#include "Comms.h"

Atom xfm_open_window, xfm_update_window, wm_delete_window, wm_protocols;

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
/*
 * Check for presence of files on this display
 */ 
Window FindFilesWin (Display *dpy, Window win)
{
  int i, n_children;
  Window *children, w;
  XClassHint ch;

  XQueryTree(dpy, win, &w, &w, &children, &n_children);

  for (i=0; i < n_children; i++) 
  {
      if (XGetClassHint(dpy, children[i], &ch))
      {	  
	  if (ch.res_class && ch.res_name &&
	      !strcmp(ch.res_class, "Xfiler") && 
	      !strcmp(ch.res_name,"file window"))
	      return children[i];
	  XFree(ch.res_class);
	  XFree(ch.res_name);
      }
      if ((w = FindFilesWin(dpy,children[i])))
	  return w;
  }
  return None;
}


void sendOpenUpdateMessage(Display *dpy, Window win, char *dir, Atom type)
{
    int i;
    char *s;
    XClientMessageEvent e;
    
    e.message_type = type;
    e.type = ClientMessage;
    e.display = dpy;
    e.window = win;
    e.format = 8;

    s = dir;
    /* Send messages in batches of 20 characters each, because that's all that
     * fits in the ClientMessage event structure */
    for (i = strlen(s); i >= 0; i -= 20, s += 20) 
    {    
	strncpy(e.data.b,s,20);  
	if (!XSendEvent(dpy, win, False, 0, (XEvent *)&e)) 
	{
	    fprintf(stderr, "XSendEvent failed\n");
	    exit(1);
	}
    }
    XFlush(dpy); /* doesn't work without this! */
}

void openRemoteDirs(Display *dpy, Window files, int argc, char **argv)
{
    Atom open, update;
    int bufsize;
    char *buf, *s;
    
    /* initialize atoms */
    open   = XInternAtom(dpy, FILES_OPEN_WINDOW,   True);
    update = XInternAtom(dpy, FILES_UPDATE_WINDOW, True);
    if (open == None || update == None) 
    {
	fprintf(stderr, "Unable to find atoms\n");
	exit(1);
    }
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
    /* start sending messages */
    while(argc--)
    {
	if (*argv[0]!='/')
	{			
	    if (bufsize>strlen(buf)+strlen(*argv))
		strcat(buf, *argv++);  
	    else
		if (!(buf = (char*)realloc(buf,strlen(buf)+strlen(*argv)+1))) 
		    exit(1);
		else
		    strcat(buf, *argv++);
	    sendOpenUpdateMessage(dpy, files, buf, open);
	    s[0] = '\0'; /* go back to current dir */
	}		
	else
	    sendOpenUpdateMessage(dpy, files, *argv++, open);    
    }  
}

void handleOpenMessage(char *data)
{
    static char dirname[400] = "";
    if (data[19] == '\0')
    {
	strcat(dirname, data);
	newFileWindow(dirname,resources.initial_display_type,True,False);
	dirname[0] = '\0';
    }
    else
	strcat(dirname, data);
}

void clientMessageHandler(Widget w, XtPointer closure, XEvent *e)
{
    /* The client message handler must be re-entrant because the invokation of
       the callbacks in response to a wm_delete_window message can cause more
       events to be dispatched. We handle this by just ignoring these recursive
       calls. */

    static int in_use = 0;
    XClientMessageEvent *c = (XClientMessageEvent *)e;
   
    if (in_use || 
	e->type != ClientMessage || 
	(c->message_type != wm_protocols && freeze))
	return;
    in_use = 1;

    if (c->message_type == xfm_open_window) 
    {
	handleOpenMessage(c->data.b);
	in_use = 0;
	return;
    }    
    if (c->message_type == xfm_update_window) 
	return;
    if (c->message_type == wm_protocols) {
	if (w == toplevel) {
	    toplevelCloseCb(w, file_windows, (XtPointer)NULL);
	} else {
	    FileWindowRec *fw;
	    for (fw = file_windows; fw; fw = fw->next)
		if (w == fw->shell) break;
	    if (!fw)
		error("Internal error:", "Widget not found in clientMessageHandler");
	    else
		fileCloseCb(w, fw, (XtPointer)NULL);
	}
    }
    in_use = 0;
}

void initComms(void)
{
  /* Make up some new atoms */
  xfm_open_window = XInternAtom(XtDisplay(toplevel), FILES_OPEN_WINDOW,
				False);
  xfm_update_window = XInternAtom(XtDisplay(toplevel), FILES_UPDATE_WINDOW,
				  False);
  wm_delete_window = XInternAtom(XtDisplay(toplevel), WM_DELETE_WINDOW,
  				 False);
  wm_protocols = XInternAtom(XtDisplay(toplevel), WM_PROTOCOLS,
			     False);

  if (xfm_open_window == None || xfm_update_window == None ||
      wm_delete_window == None || wm_protocols == None)
    abortXfm("Couldn't initialize client message handler");

}
