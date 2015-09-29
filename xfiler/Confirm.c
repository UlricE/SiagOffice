/*-----------------------------------------------------------------------------
  Module FmConfirm

  (c) Simon Marlow 1990-1993

  Module for creating and implementing the confirmation window
-----------------------------------------------------------------------------*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>

#include "../xcommon/xcommon.h"

#include "Files.h"

#define LABEL_WIDTH 400

/*-----------------------------------------------------------------------------
  STATIC DATA
-----------------------------------------------------------------------------*/

static Widget shell, label1, label2, label3;
static enum { DontKnow, Ok, Cancel, Abort } confirm_flag;
Boolean aborted;

/*-----------------------------------------------------------------------------
  Widget Argument Lists
-----------------------------------------------------------------------------*/

static Arg shell_args[] = {
  { XtNtitle, (XtArgVal) "Confirm" }
};

static Arg *form_args = NULL;

static Arg label_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNlabel, (XtArgVal) NULL },
  { XtNwidth, (XtArgVal) LABEL_WIDTH },
  { XtNfont, (XtArgVal) NULL },
  { XtNresize, (XtArgVal) False },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainRight }
};

static Arg button_box_args[] = {
  { XtNfromHoriz, (XtArgVal) NULL },
  { XtNfromVert, (XtArgVal) NULL },
  { XtNtop, XtChainTop },
  { XtNbottom, XtChainTop },
  { XtNleft, XtChainLeft },
  { XtNright, XtChainLeft }
};

/*-----------------------------------------------------------------------------
  PRIVATE FUNCTIONS
-----------------------------------------------------------------------------*/

static void confirmOkCb(Widget w, XtPointer client_data, XtPointer call_data)
{
  XtPopdown(shell);
  confirm_flag = Ok;
}

/*---------------------------------------------------------------------------*/

static void confirmCancelCb(Widget w, XtPointer client_data, 
			    XtPointer call_data)
{
  XtPopdown(shell);
  confirm_flag = Cancel;
}

/*---------------------------------------------------------------------------*/

static void confirmAbortCb(Widget w, XtPointer client_data, 
			   XtPointer call_data)
{
  XtPopdown(shell);
  confirm_flag = Abort;
}

/*-----------------------------------------------------------------------------
  Button Data
-----------------------------------------------------------------------------*/

static ButtonRec confirm_buttons[] = {
  { "ok", "Continue", (FmCallbackProc *) confirmOkCb },
  { "cancel", "Cancel", (FmCallbackProc *) confirmCancelCb },
  { "abort", "Abort", (FmCallbackProc *) confirmAbortCb },
};


/*-----------------------------------------------------------------------------
  PUBLIC FUNCTIONS
-----------------------------------------------------------------------------*/

void createConfirmPopup()
{
  Widget form, button_box;

  /* create shell */
  shell = XtCreatePopupShell("confirm", transientShellWidgetClass,
			     toplevel, shell_args, XtNumber(shell_args));

  /* create outer form */
  form = XtCreateManagedWidget("form", formWidgetClass, shell,
				      form_args, XtNumber(form_args) );

  /* create label 1 */
  label_args[4].value = (XtArgVal) resources.label_font;
  label1 = XtCreateManagedWidget("label1",labelWidgetClass, form,
				 label_args, XtNumber(label_args) );

  /* create label 2 */
  label_args[1].value = (XtArgVal) label1;
  label2 = XtCreateManagedWidget("label2",labelWidgetClass, form,
				 label_args, XtNumber(label_args) );

  /* create label 3 */
  label_args[1].value = (XtArgVal) label2;
  label3 = XtCreateManagedWidget("label3",labelWidgetClass, form,
				 label_args, XtNumber(label_args) );

  /* create button box */
  button_box_args[1].value = (XtArgVal) label3;
  button_box = XtCreateManagedWidget("button box", boxWidgetClass, form,
				     button_box_args, 
				     XtNumber(button_box_args) );
  createButtons(confirm_buttons, XtNumber(confirm_buttons), button_box,
		NULL);
  XtRealizeWidget(shell);
}

/*---------------------------------------------------------------------------*/

int confirm(String s1, String s2, String s3)
{
  XEvent e;

	MwLabelSet(label1, s1);
	MwLabelSet(label2, s2);
	MwLabelSet(label3, s3);
  popupByCursor(shell, XtGrabExclusive);

  confirm_flag = DontKnow;

  do {
    XtAppNextEvent(app_context, &e);
    XtDispatchEvent(&e);
  } while (confirm_flag == DontKnow);

  aborted = (confirm_flag == Abort);
  return (confirm_flag == Ok);
}
