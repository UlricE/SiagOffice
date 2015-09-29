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
 * Module "file.c" containing stuff for File menu                  *
 *                                                                 *
 * Randolf Werner                                                  *
 * University Koblenz                                              *
 * Germany , 6.3.91                                                *
 *******************************************************************/

#include "xedit.h"
#include <stdlib.h>
#include <X11/Xos.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "../common/common.h"

extern Widget textwindow, top;
extern Widget labelwindow;
extern Widget file_s;
extern Widget dirtylabel;
extern Display *CurDpy;

static Boolean source_changed = FALSE;
static char creat_file[1024];
static int nofilename = 1;
static struct stat filestatus;
static char internal_filename[1024];

Boolean IsSourceChanged(void)
{
	return source_changed;
}

/*******************************************************************
 * Make temporary filename					   *
 *******************************************************************/
char *xed_tmpnam(filename)
char *filename;
{
	static int nr = 0;
	static int pid;
	struct stat buf;

	if (nr == 0)
		pid = (int) getpid();
	if (filename != NULL) {
		do {
			sprintf(filename, "%s/xedtmp_%d_%d",
				siag_tmpdir, pid, nr++);
		}
		while (stat(filename, &buf) == 0);
		return (filename);
	} else {
		do {
			sprintf(internal_filename, "%s/xedtmp_%d_%d",
				siag_tmpdir, pid, nr++);
		}
		while (stat(internal_filename, &buf) == 0);
		return (internal_filename);
	}
}


/*******************************************************************
 * Mark text as changed						   *
 *******************************************************************/
void SourceChanged(w, junk, garbage)
Widget w;
XtPointer junk, garbage;
{
	XtRemoveCallback(w, XtNcallback, SourceChanged, NULL);
	source_changed = TRUE;
	XtVaSetValues(dirtylabel, XtNlabel, "**", (char *)0);
}

/*      Function Name: ResetSourceChanged.
 *      Description: Sets the source changed to FALSE, and
 *                   registers a callback to set it to TRUE when
 *                   the source has changed.
 *      Arguments: widget - widget to register the callback on.
 *      Returns: none.
 */

#ifdef NORENAME
static int rename(from, to)
char *from, *to;
{
	(void) unlink(to);
	if (link(from, to) == 0) {
		unlink(from);
		return 0;
	} else {
		return -1;
	}
}
#endif


/*******************************************************************
 * Check file permissions					   *
 *******************************************************************/
FileAccess CheckFilePermissions(file, exists)
char *file;
Boolean *exists;
{
	char temp[BUFSIZ], *ptr;

	if (access(file, F_OK) == 0) {
		*exists = TRUE;

		if (access(file, R_OK) != 0)
			return (NO_READ);

		if (access(file, R_OK | W_OK) == 0)
			return (WRITE_OK);
		return (READ_OK);
	}
	*exists = FALSE;

	strcpy(temp, file);
	if ((ptr = rindex(temp, '/')) == NULL)
		strcpy(temp, ".");
	else
		*ptr = '\0';

	if (access(temp, R_OK | W_OK | X_OK) == 0)
		return (CREATE_OK);

	return (NO_READ);
}


/*******************************************************************
 * Mark text as unchanged					   *
 *******************************************************************/
void ResetSourceChanged(w)
Widget w;
{
	XtAddCallback(XawTextGetSource(w), XtNcallback, SourceChanged, NULL);
	source_changed = FALSE;
	XtVaSetValues(dirtylabel, XtNlabel, "", (char *)0);
}


/*******************************************************************
 * Load textfile						   *
 *******************************************************************/
void load_file(filename)
char *filename;
{
	char b[1024], *p;

	Arg args[5];
	Cardinal num_args = 0;
	char label_buf[BUFSIZ];
	char *basename;

	if ((filename != NULL) && (strlen(filename) > 0)) {
		Boolean exists;

		switch (CheckFilePermissions(filename, &exists)) {
		case NO_READ:
			if (exists)
				error("Cannot open file for reading!");
			else
				error("File does not exist and directory is write protected!");
			return;
		case READ_OK:
			XtSetArg(args[num_args], XtNeditType, XawtextRead);
			num_args++;
			sprintf(label_buf, "%s       READ ONLY", filename);
			creat_file[0] = '\0';
			error("File opened READ ONLY");
			break;
		case WRITE_OK:
			XtSetArg(args[num_args], XtNeditType, XawtextEdit);
			num_args++;
			sprintf(label_buf, "%s       Read - Write", filename);
			creat_file[0] = '\0';
			break;
		case CREATE_OK:
			XtSetArg(args[num_args], XtNeditType, XawtextEdit);
			num_args++;
			sprintf(label_buf, "%s       created", filename);
			sprintf(creat_file, "%s", filename);
			break;
		default:
			return;
		}

		nofilename = 0;

		if (exists) {
			XtSetArg(args[num_args], XtNstring, filename);
			num_args++;
			sprintf(b, "XedPlus: ");
			p = strrchr(filename, '/');
			if (p)
				++p;
			else
				p = filename;
			strncat(b, p, 1020);
		} else {
			XtSetArg(args[num_args], XtNstring, NULL);
			num_args++;
			sprintf(b, "XedPlus");
		}

		XtSetValues(textwindow, args, num_args);

		num_args = 0;
		XtSetArg(args[num_args], XtNlabel, _(label_buf));
		num_args++;
		XtSetValues(labelwindow, args, num_args);
		basename = rindex(filename, '/');
		if (basename != NULL)
			basename++;
		else
			basename = filename;
		XSetIconName(XtDisplay(textwindow), XtWindow(XtParent(XtParent(textwindow))), basename);
		XtVaSetValues(top, XtNtitle, b, (char *) 0);
		stat(filename, &filestatus);
		return;
	}
	Feep();
}


/*******************************************************************
 * Callback for "Load" menu entry				   *
 *******************************************************************/
void DoFileLoad(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *new_file;

	if (source_changed) {
		if (warning("Unsaved Changes!") == XED_ABORT)
			return;
	}
	new_file = file_select("", "Load File");
	if (new_file == NULL)
		return;
	load_file(new_file);
	ResetSourceChanged(textwindow);

}


/*******************************************************************
 * Callback for "Insert" menu entry				   *
 *******************************************************************/
void DoFileInsert(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *new_file;
	XawTextBlock insertblock;
	XawTextPosition position;
	char buffer[1024];
	int file_d;


	new_file = file_select("", "Insert File");
	if (new_file == NULL)
		return;
	insertblock.firstPos = 0;
	insertblock.ptr = buffer;
	insertblock.format = FMT8BIT;

	file_d = open(new_file, O_RDONLY);
	if (file_d == -1) {
		error("Cannot open file!");
		return;
	}
	position = XawTextGetInsertionPoint(textwindow);
	while ((insertblock.length = read(file_d, insertblock.ptr, 1024)) > 0) {
		XawTextReplace(textwindow, position, position, &insertblock);
		position += 1024;
	}
	close(file_d);
	SourceChanged(XawTextGetSource(textwindow), NULL, NULL);
}


/*******************************************************************
 * Make filename for backup					   *
 *******************************************************************/
char *makeBackupName(buf, filename)
String buf, filename;
{
	sprintf(buf, "%s%s", filename, app_resources.backupNameSuffix);
	return (buf);
}


/*******************************************************************
 * Callback for "Save" menu entry				   *
 *******************************************************************/
void DoFileSave(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	Arg args[2];
	char *filename;
	XawTextEditType mode;
	struct stat filestatusneu;
	static int first_save = 1;

	XtSetArg(args[0], XtNeditType, &mode);
	XtSetArg(args[1], XtNstring, &filename);
	XtGetValues(textwindow, args, 2);
	if (mode == XawtextRead) {
		error("Cannot save file!");
		return;
	}
	if (nofilename) {
		DoFileSaveAs(widget, closure, calldata);
		return;
	}
	if (strcmp(creat_file, "") == 0) {
		stat(filename, &filestatusneu);
		if (filestatusneu.st_mtime != filestatus.st_mtime) {
			if (warning("File has been modified by someone else!") == XED_ABORT)
				return;
		}
		if (app_resources.enableBackups) {
			char backup_file[256];
			makeBackupName(backup_file, filename);
			if (rename(filename, backup_file) != 0) {
				error("Cannot create backup file!");
				return;
			}
		}
		if (!XawAsciiSaveAsFile(XawTextGetSource(textwindow), filename)) {
			error("Cannot save file!");
			return;
		}
		stat(filename, &filestatus);
	} else {
		if (!first_save) {
			stat(creat_file, &filestatusneu);
			if (filestatusneu.st_mtime != filestatus.st_mtime) {
				if (warning("File has been modified by someone else!") == XED_ABORT)
					return;
			}
		}
		first_save = 0;
		if (!XawAsciiSaveAsFile(XawTextGetSource(textwindow), creat_file)) {
			error("Cannot save file!");
			return;
		}
		stat(creat_file, &filestatus);
	}
	ResetSourceChanged(textwindow);
}


/*******************************************************************
 * Callback for "Save as" menu entry				   *
 *******************************************************************/
void DoFileSaveAs(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *new_file;
	XawTextPosition insert_pos;

	new_file = file_select("", "Save File As");
	if (new_file == NULL)
		return;
	insert_pos = XawTextGetInsertionPoint(textwindow);
	if (XawAsciiSaveAsFile(XawTextGetSource(textwindow), new_file)) {
		load_file(new_file);
		ResetSourceChanged(textwindow);
		XawTextSetInsertionPoint(textwindow, insert_pos);
	} else
		error("Cannot save file!");
}


/*******************************************************************
 * Callback for "Save Selection" menu entry      		   *
 *******************************************************************/
void DoFileSaveSelection(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char *new_file;
	XawTextBlock buf;
	XawTextPosition begin, end, length;
	int file_d, nbytes;


	new_file = file_select("", "Save Selection");
	if (new_file == NULL)
		return;
	file_d = open(new_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
	if (file_d == -1) {
		error("Cannot create file!");
		return;
	}
	XawTextGetSelectionPos(textwindow, &begin, &end);
	length = end - begin;
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


/*******************************************************************
 * Parse printer command string					   *
 *******************************************************************/
static int parse_printer_command(command, print_file, filename)
char *command;
char *print_file, *filename;
{
	char *pos, *fileat, *nameat;
	char printcommand[1024], formatstring[1024];

	strcpy(printcommand, app_resources.printCommand);
	pos = printcommand;
	fileat = nameat = NULL;
	while (*pos != '\0') {
		if (*pos == '%')
			switch (*++pos) {
			case 'f':{
					if (fileat != NULL) {
						return (0);
					}
					*pos = 's';
					fileat = pos;
					break;
				}
			case 't':{
					if (nameat != NULL) {
						return (0);
					}
					*pos = 's';
					nameat = pos;
					break;
				}
			default:{
					return (0);
				}
			}
		pos++;
	}
	sprintf(formatstring, "%s ; rm %s &", printcommand, print_file);
	if (nameat == NULL)
		sprintf(command, formatstring, print_file);
	else if (fileat < nameat)
		sprintf(command, formatstring, print_file, filename);
	else
		sprintf(command, formatstring, filename, print_file);
	return (1);
}


/*******************************************************************
 * Callback for "Print" menu entry				   *
 *******************************************************************/
void DoFilePrint(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char command[1024], *print_file;
	char *filename;
	Arg args[1];

	print_file = xed_tmpnam(NULL);
	XtSetArg(args[0], XtNstring, &filename);
	XtGetValues(textwindow, args, 1);
	if (parse_printer_command(command, print_file, filename) == 0) {
		error("Error in Printer Command String");
		return;
	}
	XawAsciiSaveAsFile(XawTextGetSource(textwindow), print_file);
	system(command);
}


/*******************************************************************
 * Callback for "Print Selection" menu entry			   *
 *******************************************************************/
void DoFilePrintSelection(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	char command[1024], *print_file;
	char *filename;
	XawTextBlock buf;
	XawTextPosition begin, end, length;
	Arg args[1];
	int file_d, nbytes;

	print_file = xed_tmpnam(NULL);
	XtSetArg(args[0], XtNstring, &filename);
	XtGetValues(textwindow, args, 1);
	if (parse_printer_command(command, print_file, filename) == 0) {
		error("Error in Printer Command String");
		return;
	}
	file_d = open(print_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
	XawTextGetSelectionPos(textwindow, &begin, &end);
	length = end - begin;
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
	system(command);
}


/*******************************************************************
 * Callback for "Quit" menu entry				   *
 *******************************************************************/
void DoFileQuit(widget, closure, calldata)
Widget widget;
caddr_t closure, calldata;
{
	if (source_changed) {
		if (warning("Unsaved Changes!") == XED_ABORT)
			return;
	}
	exit(0);
}
