/*---------------------------------------------------------------------------
  Module FmOps

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994

  Various file manipulation operations and other useful routines
---------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <X11/Intrinsic.h>

#include "Files.h"

#include "../common/common.h"

/* split a string into substrings delimited by a given character */

char *split(char *s, char c)
{
  static char *t;
  if (!s)
    s = t;
  t = s;
  if (t)
    while ((t = strchr(t, c)) && t>s && t[-1]=='\\') t++;
  if (t)
    *t++ = '\0';
  return s;
}

/*-------------------------------------------------------------------------*/

/* expand escapes in a string */

char *expand(char *s, char *t, char *c)
{
  char *s0 = s;
  for (; *t; t++) {
    if (strchr(c, *t))
      *s++ = '\\';
    *s++ = *t;
  }
  *s = '\0';
  return s0;
}

/*-------------------------------------------------------------------------*/

/* remove escapes from a string */

char *strparse(char *s, char *t, char *c)
{
  char *s0 = s;
  for (; *t; t++)
    if (*t != '\\')
      *s++ = *t;
    else if (*++t) {
      if (!strchr(c, *t))
	*s++ = '\\';
      *s++ = *t;
    } else
      break;
  *s = '\0';
  return s0;
}
      
/*-------------------------------------------------------------------------*/

/* expand filename */

char *fnexpand(char *fn)
{
  char s[MAXPATHLEN];
  int l;

  if (!fn || !user.home)
    return NULL;
  else if (fn[0] != '~' || (fn[1] != '\0' && fn[1] != '/'))
    return fn;

  l = strlen(user.home);

  if (l+strlen(fn)-1 >= MAXPATHLEN)
    return NULL;

  strcpy(s, user.home);
  strcpy(s+l, fn+1);
  return(strcpy(fn, s));
}

/*---------------------------------------------------------------------------*/

/* match a pattern with a filename, returning nonzero if the match was
   correct */

/* Currently only *, ? and [...] (character classes) are recognized, no curly
   braces. An escape mechanism for metacharacters is also missing. This could
   be implemented more efficiently, but the present simple backtracking
   routine does reasonably well for the usual kinds of patterns. -ag */

int fnmatch(char *pattern, char *fn)
{
  char *start;
  
  for (;; fn++, pattern++) {
    
    switch (*pattern) {
      
    case '?':
      if (!*fn)
	return 0;
      break;
      
    case '*':
      pattern++;
      do
	if (fnmatch(pattern,fn))
	  return 1;
      while (*fn++);
      return 0;
      
    case '[':
      start = pattern+1;
      do {
      next:
	pattern++;
	if (*pattern == ']')
	  return 0;
	else if (pattern[0] == '-' && pattern > start && pattern[1] != ']') {
	  if (pattern[-1] <= *fn && *fn <= pattern[1]) {
	    break;
	  } else {
	    start = (++pattern)+1;
	    goto next;
	  }
	}
      } while (*fn != *pattern);
      while (*pattern != ']') {
	if (!*pattern++)
	  return 0;
      }
      break;
      
    default:
      if (*fn != *pattern)
	return 0;
    }
    
    if (!*fn)
      return 1;
  };
}

/*-------------------------------------------------------------------------*/

/* check whether one path is the prefix of another */

int prefix(char *s, char *t)
{
  int l = strlen(s);

  return !strncmp(s, t, l) && (s[l-1] == '/' || t[l] == '\0' || t[l] == '/');
}

/*-------------------------------------------------------------------------*/

/* check whether a file exists */

int exists(char *path)
{
  struct stat stats;

  return (!lstat(path, &stats));
}

/*-------------------------------------------------------------------------*/

/* find file on given search path */

/* ugly hack by Ulric: always search in $(datadir)/common/bitmaps first */

char *searchPath(char *s1, char *p, char *s2)
{
  char           *s, *t;

  if (*s2 == '/' || !p)
    return (strcpy(s1, s2));

  sprintf(s1, "%s/common/bitmaps/%s", datadir, s2);
  if (exists(s1)) return s1;

  for (s = p; *s; s = t) {
    int l;
    if (!(t = strchr(s, ':')))
      t = strchr(s, 0);
    if (s == t) goto next;
    if (s[0] == '.') {
      if ((t = s+1)) s = t;
      else if (s[1] == '/') s += 2;
    }
    l = t-s;
    strncpy(s1, s, l);
    if (l > 0 && s1[l - 1] != '/')
      s1[l] = '/', l++;
    strcpy(s1+l, s2);
    if (exists(s1))
      return s1;
  next:
    if (*t) t++;
  }
  return (strcpy(s1, s2));
}

/* The following operations return zero on success and -1 on error, with errno
   set appropriately */

/*-------------------------------------------------------------------------*/

/* create a new file */

int create(char *path, mode_t mode)
{
  int file = open(path, O_WRONLY|O_CREAT|O_EXCL, mode);

  if (file == -1 || close(file))
    return -1;
  else
    return 0;
}

/*-------------------------------------------------------------------------*/

/* recursive copy operation */

static int copyfile(char *oldpath, char *newpath);
static int copydir(ino_t *inodes, int n_inodes, struct stat *oldstats,
		   char *oldpath, char *newpath);
static int copy(ino_t *inodes, int n_inodes, char *oldpath, char *newpath);

int rcopy(char *oldpath, char *newpath)
{
  return copy((ino_t *)NULL, 0, oldpath, newpath);
}

static int copyfile(char *oldpath, char *newpath)
{
  struct stat stats;
  int src = -1, dest = -1, n, errno_ret;
  char buf[BUFSIZ];

  if ((src = open(oldpath, O_RDONLY)) == -1 || stat(oldpath, &stats))
    goto err;
  else if ((dest = creat(newpath, stats.st_mode)) == -1)
    goto err;

  while ( (n = read(src, buf, BUFSIZ)) != 0)
    if ( n == -1 || write(dest, buf, n) != n )
      goto err;

  if (close(src)) {
    src = -1;
    goto err;
  } else
    return close(dest);

 err:

  errno_ret = errno;
  if (src != -1) close(src);
  if (dest != -1) close(dest);
  errno = errno_ret;
  return -1;
}

static int copydir(ino_t *inodes, int n_inodes, struct stat *oldstats,
		   char *oldpath, char *newpath)
{
  DIR *dir;
  struct dirent *entry;
  int i, ol = strlen(oldpath), nl = strlen(newpath);
  struct stat newstats;

  for (i = n_inodes-1; i >= 0; i--)
    if (inodes[i] == oldstats->st_ino) {
      errno = EINVAL;
      return -1;
    }

  if ((mkdir(newpath, user.umask & 0777) < 0 && errno != EEXIST) ||
       lstat(newpath, &newstats) ||
       !(dir = opendir(oldpath)))
    return -1;

  inodes = (ino_t *) XTREALLOC(inodes, (n_inodes+1)*sizeof(ino_t));
  inodes[n_inodes++] = newstats.st_ino;

  for(i = 0; (entry = readdir(dir)); i++)
    if (entry->d_name[0] != '.' || (entry->d_name[1] != '\0'
				    && (entry->d_name[1] != '.' ||
					entry->d_name[2] != '\0'))) {
      int ol1 = ol, nl1 = nl, l = strlen(entry->d_name);
      char *oldpath1 = (char *)alloca(ol1+l+2);
      char *newpath1 = (char *)alloca(nl1+l+2);

      strcpy(oldpath1, oldpath);
      strcpy(newpath1, newpath);
      if (oldpath1[ol1-1] != '/')
	oldpath1[ol1++] = '/';
      if (newpath1[nl1-1] != '/')
	newpath1[nl1++] = '/';
      strcpy(oldpath1+ol1, entry->d_name);
      strcpy(newpath1+nl1, entry->d_name);
      if (copy(inodes, n_inodes, oldpath1, newpath1)) {
	/* take care of recursive errors */
	char s[0xff];
	sprintf(s, "Error copying %s:", oldpath1);
	sysError(s);
      }
    }

  inodes = (ino_t *) XTREALLOC(inodes, (n_inodes-1)*sizeof(ino_t));
  return closedir(dir);
}

static int copy(ino_t *inodes, int n_inodes, char *oldpath, char *newpath)
{
  struct stat stats;

  if (lstat(oldpath, &stats))
    return -1;

  /* Directory: copy recursively */
  if (S_ISDIR(stats.st_mode))
    return copydir(inodes, n_inodes, &stats, oldpath, newpath);

  /* Regular file: copy block by block */
  else if (S_ISREG(stats.st_mode))
    return copyfile(oldpath, newpath);

  /* Fifo: make a new one */
  else if (S_ISFIFO(stats.st_mode))
    return mkfifo(newpath, user.umask & 0666);

  /* Device: make a new one */
  else if (S_ISBLK(stats.st_mode) || S_ISCHR(stats.st_mode) ||
	     S_ISSOCK(stats.st_mode))
    return mknod(newpath, user.umask & 0666, stats.st_rdev);

  /* Symbolic link: make a new one */
  else if (S_ISLNK(stats.st_mode)) {
    char lnk[MAXPATHLEN+1];
    int l = readlink(oldpath, lnk, MAXPATHLEN);

    if (l<0)
      return -1;
    lnk[l] = '\0';
    return(symlink(lnk, newpath));
  }

  /* This shouldn't happen */
  else {
    error("Unrecognized file type:", oldpath);
    return 0;
  }
}

/*-------------------------------------------------------------------------*/

/* recursive delete */

int rdel(char *path)
{
  struct stat stats;

  if (lstat(path, &stats))
    return -1;

  if (S_ISDIR(stats.st_mode)) {
    DIR *dir;
    struct dirent *entry;
    int i, pl = strlen(path);
  
    if (!(dir = opendir(path)))
      return -1;

    for(i = 0; (entry = readdir(dir)); i++)
      if (entry->d_name[0] != '.' || (entry->d_name[1] != '\0'
				      && (entry->d_name[1] != '.' ||
					  entry->d_name[2] != '\0'))) {
	int pl1 = pl, l = strlen(entry->d_name);
	char *path1 = (char *)alloca(pl1+l+2);

	strcpy(path1, path);
	if (path1[pl1-1] != '/')
	  path1[pl1++] = '/';
	strcpy(path1+pl1, entry->d_name);
	if (rdel(path1)) {
	  /* take care of recursive errors */
	  char s[0xff];
	  sprintf(s, "Error deleting %s:", path);
	  sysError(s);
	}
    }

    if (closedir(dir))
      return -1;
    else
      return rmdir(path);
  } else
    return unlink(path);
}
