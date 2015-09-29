/*---------------------------------------------------------------------------
  Module FmDirs.c                                                           

  (c) Simon Marlow 1990-1993
  (c) Albert Graef 1994

  functions for manipulating directory lists, and some other utilities
  related to the file system.

  modified 1-29-95 by rodgers@lvs-emh.lvs.loral.com (Kevin M. Rodgers)
  to add filtering of icon/text directory displays by a filename filter.

-----------------------------------------------------------------------------*/

#include <memory.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <X11/Intrinsic.h>

#include "Files.h"

/*-----------------------------------------------------------------------------
  STATIC DATA                                       
-----------------------------------------------------------------------------*/

static SortType sort_type;
static Boolean dirs_first;

/*-----------------------------------------------------------------------------
  PRIVATE FUNCTIONS
-----------------------------------------------------------------------------*/

static int comp(FileRec **fr1, FileRec **fr2)
{
  FileRec *fl1 = *fr1, *fl2 = *fr2;

  if (dirs_first) {
    if (S_ISDIR(fl1->stats.st_mode)) {
      if (!S_ISDIR(fl2->stats.st_mode))
	return -1;
    }
    else if (S_ISDIR(fl2->stats.st_mode))
      return 1;
  }
	
  switch (sort_type) {
  case SortByName:
    return strcmp(fl1->name, fl2->name);
  case SortBySize:
    return (int)(fl2->stats.st_size - fl1->stats.st_size);
  case SortByMTime:
    return (int)(fl2->stats.st_mtime - fl1->stats.st_mtime);
  }

  return 0;
}

/*-----------------------------------------------------------------------------
  PUBLIC FUNCTIONS
-----------------------------------------------------------------------------*/

/* Read in the directory for the file window specified. Note that since we have
   the stats available for the directory, we could simply check the modification
   time, and only read in the directory if necessay. This isn't worth it though-
   the time taken to stat all the files (which still needs to be done) far
   outweighs the time to read in the dir */

Boolean readDirectory(FileWindowRec *fw)
{
  FileList fl = NULL;
  DIR *dir;
  struct dirent *entry;
  int d, i, m;

  if ((d = findDev(fw->directory)) != -1) mountDev(d);
  fw->dev = d;

  if (stat(fw->directory, &fw->stats))
    goto error2;

  if (chdir(fw->directory))
    goto error2;

  if (!(dir = opendir(".")))
    goto error2;

  fw->n_bytes = 0;

  for(i = 0; (entry = readdir(dir)); i++) {
    fl = (FileRec **) XTREALLOC(fl, (i+1)*sizeof(FileRec *));
    fl[i] = (FileRec *) XtMalloc(sizeof(FileRec));
    strcpy(fl[i]->name, entry->d_name);
#ifdef MAGIC_HEADERS 
#ifdef USE_MAGIC_HEADERS	  
    magic_get_type(entry->d_name, fl[i]->magic_type);
#else
	strcpy(fl[i]->magic_type, "");
#endif
#endif
    if (lstat(entry->d_name, &(fl[i]->stats)))
      goto error1;
    if (S_ISLNK(fl[i]->stats.st_mode)) {
      fl[i]->sym_link = True;
      stat(entry->d_name, &(fl[i]->stats));
    }
    else
      fl[i]->sym_link = False;
    fl[i]->selected = False;
    fw->n_bytes += fl[i]->stats.st_size;
  }

  if (closedir(dir))
    goto error1;

  fw->files = fl;
  fw->n_files = i;
  return True;

 error1:
  for(m = 0; m <= i; m++)
    XTFREE(fl[m]);
  XTFREE(fl);

 error2:
  return False;
} 

/*-----------------------------------------------------------------------------
  Remove either files or directories from a FileList
-----------------------------------------------------------------------------*/

void filterDirectory(FileWindowRec *fw, FilterType type)
{
  FileList fl = NULL, oldfl = fw->files;
  int n = 0, m = 0;

#ifdef DEBUG_MALLOC
  fprintf(stderr,"entering filterDirectory: %lu\n",malloc_inuse(NULL));
#endif

  fw->n_bytes = 0;

  for (; m < fw->n_files; m++) {
    if (
	( !strcmp(oldfl[m]->name,".") && (type == Directories) ) ||
	( strcmp(oldfl[m]->name,".") && 
	 (
	  !strcmp(oldfl[m]->name,"..") ||
	  (
	   (fw->show_hidden || (oldfl[m]->name[0] != '.')) &&
	   (
	    (S_ISDIR(oldfl[m]->stats.st_mode) && (type != Files)) ||
	    (!S_ISDIR(oldfl[m]->stats.st_mode) && type != Directories)
	    )
	   )
	  )
	 )
	) {
      /* KMR now filter on dirFilter (I wouldn't dare mess with the above!) */
      /* AG modified to exclude folders from filtering */
      if ((type == Directories) || !fw->do_filter || 
	  S_ISDIR(oldfl[m]->stats.st_mode) ||
	  fnmatch(fw->dirFilter, oldfl[m]->name)) {
    fl = (FileList) XTREALLOC(fl, (n+1)*sizeof(FileRec *));
    fl[n] = oldfl[m];
	n++;
    fw->n_bytes += oldfl[m]->stats.st_size;
      }
      else
	XTFREE(oldfl[m]);
    }
    else
      XTFREE(oldfl[m]);
  }
  XTFREE(oldfl);
  
#ifdef DEBUG_MALLOC
  fprintf(stderr,"exiting filterDirectory: %lu\n",malloc_inuse(NULL));
#endif

  fw->n_files = n;
  fw->files = fl;
}  


/*-----------------------------------------------------------------------------
  Sort a directory according to the sort type and dfirst flag
-----------------------------------------------------------------------------*/

void sortDirectory(FileList fl, int n, SortType type, Boolean dfirst)
{
  sort_type = type;
  dirs_first = dfirst;
  qsort(fl, n, sizeof(FileRec *), (int (*)(const void *, const void *))comp);
}


/*-----------------------------------------------------------------------------
  Check permission for an operation, equivalent to UNIX access()
-----------------------------------------------------------------------------*/

int permission(struct stat *stats, int perms)
{
  int mode = stats->st_mode;
  int result = 0;

  if (user.uid == 0 || user.uid == stats->st_uid) {
    if (mode & S_IRUSR)
      result |= P_READ;
    if (mode & S_IWUSR)
      result |= P_WRITE;
    if (mode & S_IXUSR)
      result |= P_EXECUTE;
  } 

  else if (user.uid == 0 || user.gid == stats->st_gid) {
    if (mode & S_IRGRP)
      result |= P_READ;
    if (mode & S_IWGRP)
      result |= P_WRITE;
    if (mode & S_IXGRP)
      result |= P_EXECUTE;
  } 

  else {
    if (mode & S_IROTH)
      result |= P_READ;
    if (mode & S_IWOTH)
      result |= P_WRITE;
    if (mode & S_IXOTH)
      result |= P_EXECUTE;
  } 

  return (result & perms) == perms;
}

void makePermissionsString(char *s, int perms)
{
  s[0] = perms & S_IRUSR ? 'r' : '-'; 
  s[1] = perms & S_IWUSR ? 'w' : '-'; 
  s[2] = perms & S_IXUSR ? 'x' : '-'; 

  s[3] = perms & S_IRGRP ? 'r' : '-'; 
  s[4] = perms & S_IWGRP ? 'w' : '-'; 
  s[5] = perms & S_IXGRP ? 'x' : '-'; 

  s[6] = perms & S_IROTH ? 'r' : '-'; 
  s[7] = perms & S_IWOTH ? 'w' : '-'; 
  s[8] = perms & S_IXOTH ? 'x' : '-'; 
  
  s[9] = '\0';
}

/*---------------------------------------------------------------------------*/

void freeFileList(FileWindowRec *fw)
{
  int i;

  if (fw->files) {
    for (i = 0; i < fw->n_files; i++)
      XTFREE(fw->files[i]);
    XTFREE(fw->files);
    fw->files = NULL;
  }
  fw->n_files = 0;
  fw->n_bytes = 0;
}  

