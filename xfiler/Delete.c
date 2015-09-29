/*---------------------------------------------------------------------------
  Module FmDelete

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994

  Functions for implementing the delete and empty operations
---------------------------------------------------------------------------*/

#include <unistd.h>
#include <X11/Intrinsic.h>

#include "Files.h"

void deleteItems(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  char error_string[0xff];
  int i, n_deleted = 0;

  if (fw == NULL) fw = popup_fw;

  if (!fw->n_selections) return;

  if (chdir(fw->directory)) {
    sysError("System error:");
    return;
  }

  freeze = True;

  if (resources.confirm_deletes) {
    sprintf(error_string, "Deleting %d item%s from", fw->n_selections,
	    fw->n_selections > 1 ? "s" : "" );
    if (!confirm(error_string, fw->directory, ""))
      goto out;
  }

  for (i=0; i < fw->n_files; i++) {
    if (fw->files[i]->selected) {
      if (!fw->files[i]->sym_link && S_ISDIR(fw->files[i]->stats.st_mode))
	if (!strcmp(fw->files[i]->name, ".") ||
	    !strcmp(fw->files[i]->name, "..")) {
	  error("Cannot delete . or ..", "");
	  continue;
	} else if (resources.confirm_delete_folder &&
		 !confirm("Do you REALLY wish to delete folder",
			  fw->files[i]->name,
			  "and ALL items contained in it?")) {
	  if (aborted) break;
          else continue;
      }
      if (rdel(fw->files[i]->name)) {
	sprintf(error_string, "Error deleting %s:", fw->files[i]->name);
	sysError(error_string);
      } else
	n_deleted++;
    }
  }

  if (n_deleted) {
    markForUpdate(fw->directory);
    intUpdate();
  }

 out:
  freeze = False;
}

/*---------------------------------------------------------------------------*/

void emptyDir(Widget w, FileWindowRec *fw, XtPointer call_data)
{
  char error_string[0xff];
  int i, n_deleted = 0;

  if (chdir(fw->directory)) {
    sysError("System error:");
    return;
  }

  freeze = True;

  if (resources.confirm_deletes) {
    int n_files = fw->display_type == Tree?fw->n_files-2:fw->n_files-1;

    sprintf(error_string, "Deleting %d item%s from", n_files,
	    n_files > 1 ? "s" : "" );
    if (!confirm(error_string, fw->directory, ""))
      goto out;
  }

  for (i=0; i < fw->n_files; i++) {
    if (strcmp(fw->files[i]->name, ".") && strcmp(fw->files[i]->name, "..")) {
      if (!fw->files[i]->sym_link && S_ISDIR(fw->files[i]->stats.st_mode) &&
	  resources.confirm_delete_folder &&
	  !confirm("Do you REALLY wish to delete folder", fw->files[i]->name,
		   "and ALL items contained in it?")) {
	if (aborted) break;
        else continue;
      }
      if (rdel(fw->files[i]->name)) {
	sprintf(error_string, "Error deleting %s:", fw->files[i]->name);
	sysError(error_string);
      } else
	n_deleted++;
    }
  }

  if (n_deleted) {
    markForUpdate(fw->directory);
    intUpdate();
  }

 out:
  freeze = False;
}
