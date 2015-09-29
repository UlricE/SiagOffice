/*
 *  Copyright (c) 1996 André Hentz
 *  Copyright (c) 1998-2003 Ulric Eriksson
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/AsciiText.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/wait.h>

#include <Mowitz/Mowitz.h>
#include "../common/common.h"
#include "../xcommon/xcommon.h"

#define APPNAME "Runcmd"

static String fallback_resources[] = {
#include "../xcommon/xcommon-ad.h"
#include "runcmd-ad.h"
NULL
};

#ifndef XawChainTop  /* X11R4 */
#define XawChainTop XtChainTop
#define XawChainLeft XtChainLeft
#define XawChainRight XtChainRight
#define XawChainBottom XtChainBottom
#define XtSetLanguageProc(x,y,z)  /* nothing */
#endif

/*-------------- prototypes --------------------*/
void QuitButtonCb(Widget w, XtPointer client_data, XtPointer call_data);
void StopButtonCb(Widget w, XtPointer client_data, XtPointer call_data);
void RedoButtonCb(Widget w, XtPointer client_data, XtPointer call_data);
void WriteMessage(const char* Data, int Size);
int pipe_command(char *command,char **argv);
void on();
void off();
void TimeoutProc(XtPointer data, XtIntervalId *id);
void ResetTimer(int time);
/*-------------- Public variables --------------*/
XtAppContext	app;
Widget	toplevel,form;
Widget	MessageArea;
Widget QuitButton, RedoButton, StopButton;
char * progname;
char **myargs;
int ChildPid = -1;		/* no child yet */
int PipeToRead = -1;	/* no pipe to read data from */
XtIntervalId UpdateTimer;

int main(int argc, char* argv[])
{
	int i; 
	Dimension Height;
	Display *dpy;

	progname = argv[0];

	XtSetLanguageProc(NULL,NULL,NULL);

	common_init("Runcmd %s. No Warranty");

	toplevel=XtVaOpenApplication(&app,"Runcmd",NULL,0,
				   (Cardinal*)&argc,argv,
				   fallback_resources,
				   mwApplicationShellWidgetClass, NULL);

	dpy = XtDisplay(toplevel);
	MwHighlightInit(toplevel);
	theme_init(dpy);

 	if (argc<2)
	{  
		fprintf(stderr, "No parameter given!\n");
	  	exit(1);
	}
	
	form=XtVaCreateManagedWidget("Form",
		formWidgetClass, toplevel,
		XtNborderWidth, 0,
		NULL);

	QuitButton = XtVaCreateManagedWidget("Quit",
		commandWidgetClass, form,
		XtNlabel, _("Quit"),
 		XtNleft, XawChainLeft,
		XtNright, XawChainLeft,
		XtNtop, XawChainTop,
		XtNbottom, XawChainTop,
		NULL);
   	XtAddCallback(QuitButton, XtNcallback,
		        (XtCallbackProc) QuitButtonCb,
			NULL );

	RedoButton = XtVaCreateManagedWidget("Redo",
		commandWidgetClass, form,
		XtNlabel, _("Redo"),
 		XtNleft, XawChainLeft,
		XtNright, XawChainLeft,
		XtNtop, XawChainTop,
		XtNbottom, XawChainTop,
		XtNfromHoriz, QuitButton,
		NULL);
   	XtAddCallback(RedoButton, XtNcallback,
		        (XtCallbackProc) RedoButtonCb,
			NULL );

	StopButton = XtVaCreateManagedWidget("Stop",
		commandWidgetClass, form,
		XtNlabel, _("Stop"),
 		XtNleft, XawChainLeft,
		XtNright, XawChainLeft,
		XtNtop, XawChainTop,
		XtNbottom, XawChainTop,
		XtNfromHoriz, RedoButton,
		NULL);
   	XtAddCallback(StopButton, XtNcallback,
		        (XtCallbackProc) StopButtonCb,
			NULL );

	MessageArea=XtVaCreateManagedWidget("Messages",
		asciiTextWidgetClass, form,
 		XtNleft, XawChainLeft,
		XtNright, XawChainRight,
		XtNtop, XawChainTop,
		XtNbottom, XawChainBottom,
		XtNborderWidth, 1,
      	XtNwidth, 300,
		XtNscrollVertical, XawtextScrollAlways,
		XtNfromVert, QuitButton,
		NULL);
	XtVaGetValues(MessageArea,
		 	XtNheight,&Height,
			NULL);
	XtVaSetValues(MessageArea,
			XtNheight,4*Height-2,
			XtNscrollHorizontal, XawtextScrollWhenNeeded,
			NULL);

	XtRealizeWidget(toplevel);
	
	myargs = (char**)MwMalloc(argc*sizeof(char*));
	i = 0;
	while(i<argc-1)
	{
		myargs[i] = argv[i+1];
		i++;
	}
	myargs[argc-1] = NULL;

	on();

	XtAppMainLoop(app);
	return 0;
}

/* 
 * Callbacks
 */
void QuitButtonCb(Widget w, XtPointer client_data, XtPointer call_data)
{
	if (ChildPid!=-1)
		kill(ChildPid, SIGKILL);
	exit(0);
}

void RedoButtonCb(Widget w, XtPointer client_data, XtPointer call_data)
{
	if(ChildPid==-1)
	{
		XtVaSetValues(MessageArea, XtNstring, NULL, NULL);
		on();
	}
}

void StopButtonCb(Widget w, XtPointer client_data, XtPointer call_data)
{
	if (ChildPid!=-1)
		kill(ChildPid, SIGKILL);
	off();
}

/*
 * Write Message - write data to the MessageArea
 */
void WriteMessage(const char* Data, int Size)
{
	static XawTextPosition fim = 0;
	XawTextBlock text;
	char *tmp;
	if (!Size) return;
	tmp = (char*)MwMalloc((Size+1)*sizeof(char));
	strncpy(tmp, Data, Size);
	XtVaSetValues(MessageArea, XtNeditType, XawtextEdit, NULL);
	text.firstPos = 0;
	text.length = Size;
	text.ptr = tmp;
#ifdef XawFmt8Bit
	text.format = XawFmt8Bit;
#else
	text.format = FMT8BIT;
#endif
	XawTextReplace(MessageArea, fim, fim, &text);
	fim += text.length;
	XawTextSetInsertionPoint(MessageArea, fim+1);
	XtVaSetValues(MessageArea, XtNeditType, XawtextRead, NULL);
	XFlush(XtDisplay(toplevel));
	MwFree(tmp);
}

/* 
 *         Pipe Command
 * Receives the same parameters as execvp. Fork and execvp. 
 * Sets ChildPid to the pid of the child processes and return the 
 * file descriptor that should be used to read data from the child
 */
int pipe_command(command, argv)
	char *command;
	char **argv;
{
	int out_pipe[2];
	char buffer[BUFSIZ];

 	if (pipe(out_pipe)==-1)
	{
		perror(progname);
		return -1;
	}

	ChildPid = fork();
	if (ChildPid==-1)
	{
		perror(progname);
		close(out_pipe[0]);
		close(out_pipe[1]);
		PipeToRead = -1;
		return -1;
	}

	if (!ChildPid)
	{	/* child's thread */
		sprintf(buffer, "%s(child)", progname);
		close(out_pipe[0]); /* there's no reading of output pipe! */
		/* redirect stdout */
		if (dup2(out_pipe[1], fileno(stdout))==-1)
		{
			perror(buffer);
			exit(1);
		}
		/* redirect stderr */
		if (dup2(out_pipe[1], fileno(stderr))==-1)
		{
			perror(buffer);
			exit(1);
		}
		execvp(command, argv);
		perror(buffer);
		exit(1);
	}
	else
	{	/* parent's thread */
		close(out_pipe[1]); /* there's no writing on output pipe! */
		fcntl(out_pipe[0], F_SETFL, O_NONBLOCK); /* we don't want to wait */
		return out_pipe[0];
	}
}

static char *title_cmd(void)
{
	if (myargs[0] && !strcmp(myargs[0], "/bin/sh")
		&& myargs[1] && !strcmp(myargs[1], "-c")
		&& myargs[2]) return myargs[2];
	return myargs[0];
}

void on()
{
	char buffer[512];

	ResetTimer(1200);
	PipeToRead = pipe_command(myargs[0], myargs);
	sprintf(buffer, "Executing %s...", title_cmd());
	XtVaSetValues(toplevel, XtNtitle, buffer, NULL);
	XtVaSetValues(RedoButton, XtNsensitive, False, NULL);
	XtVaSetValues(QuitButton, XtNsensitive, False, NULL);
	XtVaSetValues(StopButton, XtNsensitive, True, NULL);
}

void off()
{
	char buffer[512];
	int status;
	
	waitpid(ChildPid, &status, WNOHANG);	
	close(PipeToRead);
	ChildPid = PipeToRead = -1;
	sprintf(buffer, "Runcmd (%s)", title_cmd());
	XtVaSetValues(toplevel, XtNtitle, buffer, NULL);
	XtVaSetValues(QuitButton, XtNsensitive, True, NULL);
	XtVaSetValues(RedoButton, XtNsensitive, True, NULL);
	XtVaSetValues(StopButton, XtNsensitive, False, NULL);
}

/*
 * Timer-related functions
 */
void TimeoutProc(XtPointer data, XtIntervalId *id)
{
	int bytes;
	char buffer[BUFSIZ];

	if (PipeToRead==-1)
	  return;

	/* start reading pipe */
	bytes=read(PipeToRead, buffer, BUFSIZ-1);
	if (bytes==-1)
	{
		/* no data yet */
		ResetTimer(600);
		return;
	}
	if (bytes==0)
	{
		/* end of data */
		WriteMessage("I'm done! Press Quit, please\n", 29);
		off();
		return;
	}

 	buffer[bytes]=0;
	WriteMessage(buffer, bytes);
	ResetTimer(600);
}

void ResetTimer(int time)
{
	UpdateTimer = XtAppAddTimeOut(app, time, TimeoutProc, NULL);
}
