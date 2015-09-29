/*
 *  Editor - a menu-driven text editor
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

#include "xedit.h"
#include <Mowitz/Mowitz.h>

extern Widget top;
extern Widget textwindow;
extern Widget labelwindow;

void FileDragHandler(Widget widget, XtPointer data, XEvent * event, Boolean * p)
{
	Arg args[1];
	char *filename;

	/* We will only drag if the file is saved */
	if (IsSourceChanged())
		return;

	/* Get the filename */
	XtSetArg(args[0], XtNstring, &filename);
	XtGetValues(textwindow, args, 1);

	/* Do the drag */
	MwDndSetData(MW_DndFile, filename, strlen(filename) + 1);
	MwDndHandleDragging(widget, event);
}

void FileDropHandler(Widget widget, XtPointer data, XEvent * event, Boolean * p)
{
	unsigned long Size;
	unsigned char *Data;

	/* We only know how to handle simple file names */
	if (MwDndDataType(event) != MW_DndFile && MwDndDataType(event) != MW_DndExe) {
		error("Don't know how to handle this drop!");
		return;
	}
	/* If the drop is from this editor, do nothing! */
	/* Updated for version 1 protocol               */
	if (MwDndSourceWindow(event) == XtWindow(widget))
		return;

	/* Test for source changes */
	if (IsSourceChanged())
		if (warning("Unsaved Changes!") == XED_ABORT)
			return;

	/* Load the file */
	MwDndGetData(&Data, &Size);
	if (Size && Data != NULL) {
		load_file(Data);
		ResetSourceChanged(textwindow);
		/* de-iconify us */
		XMapWindow(XtDisplay(top), XtWindow(top));
	}
}
