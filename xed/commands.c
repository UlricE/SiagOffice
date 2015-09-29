/*

   Copyright (c) 1995  Randolf Werner 

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the X Consortium shall
   not be used in advertising or otherwise to promote the sale, use or
   other dealings in this Software without prior written authorization
   from the X Consortium.

 */

/*******************************************************************
 * Module "commands.c" containing stuff for the "Commads" and      *
 * "Pipes" menu      					           *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 4.3.95                                                *
 *******************************************************************/

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include "xedit.h"
#include <limits.h>

/* Used global widgets */
extern Widget textwindow;
extern Widget command_popup, command_text;
extern Widget pipe_popup, pipe_text;


/*******************************************************************
 * Callback for "Command" menu entry				   *
 *******************************************************************/
void DoCommand0(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	centerpopup(textwindow, command_popup);
	XtPopup(command_popup, XtGrabNone);
	set_wm_delete(command_popup);
	XRaiseWindow(XtDisplay(command_popup), XtWindow(command_popup));
}


/*******************************************************************
 * Callback for "Do it" button in command popup			   *
 *******************************************************************/
void command_do(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	char *commandstring;

	XtSetArg(args[0], XtNstring, &commandstring);
	XtGetValues(command_text, args, 1);
	closure = (caddr_t) commandstring;
	command_exec(widget, closure, calldata);
}


/*******************************************************************
 *  Callback for user defined command entry		           *
 *  (also called from command_do)			           *
 *******************************************************************/
void command_exec(Widget widget, XtPointer closure, XtPointer calldata)
{
	char *commandstring, command[1024], *filename;
	Arg args[2];
	FILE *fp;
	int file_d, nbytes;
	XawTextBlock buf;
	XawTextPosition begin, beginmerk, end, length, lengthmerk;
	char tmpin_file[L_xedtmpnam + 3], tmpsel_file[L_xedtmpnam + 3],
	 tmpcom_file[L_xedtmpnam + 3], stripped[1024], *pos;

	commandstring = (char *) closure;
	xed_tmpnam(tmpin_file);
	strcat(tmpin_file, "in");
	XawAsciiSaveAsFile(XawTextGetSource(textwindow), tmpin_file);
	XawTextGetSelectionPos(textwindow, &begin, &end);
	beginmerk = begin;
	lengthmerk = length = end - begin;
	if (length <= 0)
		sprintf(tmpsel_file, "/dev/null");
	else {
		xed_tmpnam(tmpsel_file);
		strcat(tmpsel_file, "sel");
		file_d = open(tmpsel_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		while (begin < end) {
			if (length > 1024)
				nbytes = 1024;
			else
				nbytes = length;
			XawTextSourceRead(XawTextGetSource(textwindow), begin, &buf, nbytes);
			write(file_d, buf.ptr, buf.length);
			begin += buf.length;
			length -= buf.length;
		}
		close(file_d);
	}
	XtSetArg(args[0], XtNstring, &filename);
	XtGetValues(textwindow, args, 1);
	pos = rindex(filename, '/');
	if (pos != NULL)
		pos++;
	else
		pos = filename;
	strcpy(stripped, pos);
	pos = rindex(stripped, '.');
	if (pos != NULL)
		*pos = '\0';
	xed_tmpnam(tmpcom_file);
	strcat(tmpcom_file, "com");
	fp = fopen(tmpcom_file, "w");
	fprintf(fp, "filename=%s\nstripped=%s\nselection=%s\ntempfile=%s\n%s\n",
	     filename, stripped, tmpsel_file, tmpin_file, commandstring);
	fclose(fp);
	if (lengthmerk > 0)
		sprintf(command, "(<%s 2>&1 sh | xedplus - ;rm %s %s %s) &", tmpcom_file, tmpin_file, tmpsel_file, tmpcom_file);
	else
		sprintf(command, "(<%s 2>&1 sh | xedplus - ;rm %s %s) &", tmpcom_file, tmpin_file, tmpcom_file);
	system(command);
}


/*******************************************************************
 * Callback for "Close" button in command popup			   *
 *******************************************************************/
void command_close(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(command_popup);
}


/*******************************************************************
 * Callback for "Pipe" menu entry				   *
 *******************************************************************/
void DoPipe0(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	centerpopup(textwindow, pipe_popup);
	XtPopup(pipe_popup, XtGrabNone);
	set_wm_delete(pipe_popup);
	XRaiseWindow(XtDisplay(pipe_popup), XtWindow(pipe_popup));
}


/*******************************************************************
 * Callback for "Do it" button in pipes popup			   *
 *******************************************************************/
void pipe_do(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	char *commandstring;

	XtSetArg(args[0], XtNstring, &commandstring);
	XtGetValues(pipe_text, args, 1);
	closure = (caddr_t) commandstring;
	pipe_exec(widget, closure, calldata);
}


/*******************************************************************
 * Callback for user defined pipes     				   *
 * (also called from pipe_do)					   *
 *******************************************************************/
void pipe_exec(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *commandstring, command[1024], *filename;
	Arg args[2];
	FILE *fp;
	int file_d, nbytes;
	char buffer[1024];
	XawTextBlock buf, insertblock;
	XawTextPosition begin, beginmerk, end, length, lengthmerk, position;
	char tmpin_file[L_xedtmpnam + 3], tmpsel_file[L_xedtmpnam + 3],
	 tmpcom_file[L_xedtmpnam + 3], tmpout_file[L_xedtmpnam + 3],
	 stripped[1024], *pos;

	commandstring = (char *) closure;
	xed_tmpnam(tmpin_file);
	strcat(tmpin_file, "in");
	XawAsciiSaveAsFile(XawTextGetSource(textwindow), tmpin_file);
	xed_tmpnam(tmpout_file);
	strcat(tmpout_file, "out");
	XawTextGetSelectionPos(textwindow, &begin, &end);
	beginmerk = begin;
	lengthmerk = length = end - begin;
	if (length <= 0)
		sprintf(tmpsel_file, "/dev/null");
	else {
		xed_tmpnam(tmpsel_file);
		strcat(tmpsel_file, "sel");
		file_d = open(tmpsel_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
		while (begin < end) {
			if (length > 1024)
				nbytes = 1024;
			else
				nbytes = length;
			XawTextSourceRead(XawTextGetSource(textwindow), begin, &buf, nbytes);
			write(file_d, buf.ptr, buf.length);
			begin += buf.length;
			length -= buf.length;
		}
		close(file_d);
	}
	XtSetArg(args[0], XtNstring, &filename);
	XtGetValues(textwindow, args, 1);
	pos = rindex(filename, '/');
	if (pos != NULL)
		pos++;
	else
		pos = filename;
	strcpy(stripped, pos);
	pos = rindex(stripped, '.');
	if (pos != NULL)
		*pos = '\0';
	xed_tmpnam(tmpcom_file);
	strcat(tmpcom_file, "com");
	fp = fopen(tmpcom_file, "w");
	fprintf(fp, "filename=%s\nstripped=%s\nselection=%s\ntempfile=%s\n%s\n",
	     filename, stripped, tmpsel_file, tmpin_file, commandstring);
	fclose(fp);
	sprintf(command, "<%s 1> %s 2> %s sh", tmpcom_file, tmpout_file, tmpout_file);
	system(command);
	file_d = open(tmpout_file, O_RDONLY);
	insertblock.firstPos = 0;
	insertblock.length = 0;
	insertblock.ptr = buffer;
	insertblock.format = FMT8BIT;
	XawTextReplace(textwindow, beginmerk, end, &insertblock);
	position = beginmerk;
	while ((insertblock.length = read(file_d, insertblock.ptr, 1024)) > 0) {
		XawTextReplace(textwindow, position, position, &insertblock);
		position += 1024;
	}
	close(file_d);
	unlink(tmpin_file);
	if (lengthmerk > 0)
		unlink(tmpsel_file);
	unlink(tmpcom_file);
	unlink(tmpout_file);
}


/*******************************************************************
 * Callback for "Close" button in pipe popup			   *
 *******************************************************************/
void pipe_close(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	XtPopdown(pipe_popup);
}
