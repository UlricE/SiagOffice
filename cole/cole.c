/*
   cole - A free C OLE library.
   Copyright 1998, 1999  Roberto Arturo Tena Sanchez

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
/*
   Arturo Tena <arturo@directmail.org>
 */

#if !(defined( __BORLANDC__ ) || defined( __WIN32__ ))
#include "config.h"
#include "cole.h"
#else
#include "cole.h.in"
#endif

#include "internal.h"
#include <stdlib.h>



/**
 * cole_perror:
 * @s: string to print before the error message. It can be NULL.
 * @colerrno: error value of which a message will be printed.
 *
 * Prints a message on the standard error output, describing the error
 * @colerrno, preceding it with the string @s, a semicolon and a space.
 * It handles COLE_EERRNO value too, calling perror(3).
 */
void
cole_perror (const char *s, COLERRNO colerrno)
{
	if (s != NULL) fprintf (stderr, "%s: ", s);
	switch (colerrno) {
	case COLE_EMEMORY:
	case COLE_ECLOSEFILE:
	case COLE_EWRITE:
	case COLE_EREMOVE:
	case COLE_ETMPNAM:
	case COLE_ESEEK:
	case COLE_EERRNO:
		perror ("cole");
		break;
	case COLE_EOPENFILE:
		fprintf (stderr, "cole - Cannot open the file\n");
		break;
	case COLE_ENOFILESYSTEM:
		fprintf (stderr, "cole - The file is not a OLE2 file\n");
		break;
	case COLE_EINVALIDFILESYSTEM:
		fprintf (stderr, "cole - The file has a short OLE2 header or it is not really an OLE2 file.\n");
		break;
	case COLE_EISNOTDIR:
		fprintf (stderr, "cole - The OLE2 entry is not a substorage object\n");
		break;
	case COLE_EISNOTFILE:
		fprintf (stderr, "cole - The substorage object is not valid\n");
		break;
	case COLE_EFILENOTFOUND:
		fprintf (stderr, "cole - OLE2 object not found\n");
		break;
	case COLE_EOF:
		fprintf (stderr, "cole - End of stream object has been reached\n");
		break;
	case COLE_EMEMBERISNOTDIR:
		fprintf (stderr, "cole - The OLE2 object searched for is not "
			 "a substorage object\n");
		break;
	case COLE_EBROKENFILENAME:
		fprintf (stderr, "cole - Illegal OLE object name\n");
		break;
	case COLE_EFILENAMEISNOTFILE:
		fprintf (stderr, "cole - The OLE2 object is not a stream\n");
		break;
	case COLE_EFSEEKDELTA:
		fprintf (stderr, "cole - The seek offset is an illegal value\n");
		break;
	case COLE_EFSEEKFLAG:
		fprintf (stderr, "cole - The Seek Flag is not valid\n");
		break;
	case COLE_EREAD:
		fprintf (stderr, "cole - Short read length returned...the file is probably corrupted\n");
		break;
	case COLE_EUNKNOWN:
		fprintf (stderr, "cole - An unknown error ocurred in the cole libary (might be a bug)\n");
		break;
	default:
		fprintf (stderr, "cole - An unknown error %d ocurred in the cole libabry (might be a bug)\n",
			 colerrno);
		break;
	}
}



/**
 * cole_mount:
 * @filename: name of the file with the filesystem.
 * @colerrno: error value (COLE_EMEMORY, COLE_EOPENFILE, COLE_ENOFILESYSTEM,
 * 			   COLE_EINVALIDFILESYSTEM, COLE_EUNKNOWN).
 *
 * Mounts the filesystem which is in @filename.
 *
 * Returns: a filesystem in success, or NULL in other case.
 */
COLEFS *
cole_mount (char *filename, COLERRNO *colerrno)
{
	COLEFS * ret;

	ret = malloc (sizeof (COLEFS));
	if (ret == NULL) {
		if (colerrno != NULL) *colerrno = COLE_EMEMORY;
		return NULL;
	}

	switch (__OLEdecode (filename, &ret->tree, &ret->root, &ret->BDepot,
				&ret->SDepot, &ret->sbfile, &ret->sbfilename,
				&ret->file, 0)) {
	case 0:
		/* success */
		break;
	case 10:
		if (colerrno != NULL) *colerrno = COLE_EMEMORY;
		free (ret);
		return NULL;
	case 7:
	case 4:
		if (colerrno != NULL) *colerrno = COLE_EOPENFILE;
		free (ret);
		return NULL;
	case 8:
	case 9:
		if (colerrno != NULL) *colerrno = COLE_ENOFILESYSTEM;
		free (ret);
		return NULL;
	case 5:
		if (colerrno != NULL) *colerrno = COLE_EINVALIDFILESYSTEM;
		free (ret);
		return NULL;
	default:
		if (colerrno != NULL) *colerrno = COLE_EUNKNOWN;
		free (ret);
		return NULL;
	}

	return ret;
}


/**
 * cole_umount:
 * @colefilesystem: filesystem to umount.
 * @colerrno: error value (COLE_ECLOSEFILE, COLE_EREMOVE).
 *
 * Umounts the filesystem @colefilesystem.
 *
 * Returns: zero in success, no zero in other case.
 */
int
cole_umount (COLEFS *colefilesystem, COLERRNO *colerrno)
{
	int ret;

	ret = 0;
	free (colefilesystem->BDepot);
	free (colefilesystem->tree);
	if (fclose (colefilesystem->file) && !ret) {
		if (colerrno != NULL) *colerrno = COLE_ECLOSEFILE;
		ret = 1;
	}
	if (colefilesystem->SDepot != NULL) {
		free (colefilesystem->SDepot);
		/* may no exist SDepot because there are not small files */
		/* assert (colefilesystem->sbfile != NULL); */
		/* assert (colefilesystem->sbfilename != NULL); */
		if (fclose (colefilesystem->sbfile) && !ret) {
			if (colerrno != NULL) *colerrno = COLE_ECLOSEFILE;
			ret = 1;
		}
		if (remove (colefilesystem->sbfilename) && !ret) {
			if (colerrno != NULL) *colerrno = COLE_EREMOVE;
			ret = 1;
		}
		free (colefilesystem->sbfilename);
	}
	free (colefilesystem);

	return ret;
}


/**
 * cole_print_tree:
 * @colefilesystem: filesystem of which the tree will be printed.
 * @colerrno: error value (errors from call cole_recurse_tree()).
 *
 * Prints on the standard output the tree of files and directories contained
 * in @colefilesystem.
 * Currently this call always success.
 *
 * Returns: zero in success, no zero in other case.
 */
static COLE_RECURSE_DIR_FUNC __cole_print_tree_indir;
static COLE_RECURSE_DIR_FUNC __cole_print_tree_outdir;
static COLE_RECURSE_DIR_FUNC __cole_print_tree_inroot;
static COLE_RECURSE_DIRENT_FUNC __cole_print_tree_indirentry;
int
cole_print_tree (COLEFS *colefilesystem, COLERRNO *colerrno)
{
	long level;

	level = 1;
	if (cole_recurse_tree (colefilesystem, &level,
			       __cole_print_tree_inroot,
			       __cole_print_tree_indirentry,
			       __cole_print_tree_indir,
			       __cole_print_tree_outdir, NULL, colerrno)) {
		return 1;
	}

	return 0;
}
int
__cole_print_tree_indir(COLEDIR *cd, void *info, COLERRNO *colerrno)
{
/*
 * ATTENTION: if you modify this function so it modifies colerrno:
 * 	Modify colerrno comment in the functions that call it,
 *	ie. cole_print_tree().
 */
	(*((long*)info))++;
	return 0;
}
int
__cole_print_tree_outdir(COLEDIR *cd, void *info, COLERRNO *colerrno)
{
/*
 * ATTENTION: if you modify this function so it modifies colerrno:
 * 	Modify colerrno comment in the functions that call it,
 *	ie. cole_print_tree().
 */
	(*((long*)info))--;
	return 0;
}
int
__cole_print_tree_inroot(COLEDIR *cd, void *info, COLERRNO *colerrno)
{
/*
 * ATTENTION: if you modify this function so it modifies colerrno:
 * 	Modify colerrno comment in the functions that call it,
 *	ie. cole_print_tree().
 */
	char *entry_name;

	printf ("DIR ");
	printf (" %7u", cole_dir_getsize (cd));
	printf (" %08lx-%08lx %08lx-%08lx",
		cole_dir_getdays1 (cd),
		cole_dir_getsec1 (cd),
		cole_dir_getdays2 (cd),
		cole_dir_getsec2 (cd));
	entry_name = cole_dir_getname (cd);
	if (!isprint ((int)entry_name[0]))
		printf (" '\\x%02x%s'\n", entry_name[0], entry_name+1);
	else
		printf (" '%s'\n", entry_name);

	return 0;
}
int
__cole_print_tree_indirentry(COLEDIRENT *cde, void *info, COLERRNO *colerrno)
{
/*
 * ATTENTION: if you modify this function so it modifies colerrno:
 * 	Modify colerrno comment in the functions that call it,
 *	ie. cole_print_tree().
 */
	char *entry_name;
	long level;
	long i;

	level = *((long*)info);
	for (i = 0; i < level; i++) {
		if (i == level - 1)
			printf ("\\--");
		else
			printf ("|  ");
	}

	if (cole_direntry_isdir (cde))
		printf ("DIR ");
	else if (cole_direntry_isfile (cde))
		printf ("FILE");
	else
		printf ("????");
	printf (" %7u", cole_direntry_getsize (cde));
	printf (" %08lx-%08lx %08lx-%08lx",
		cole_direntry_getdays1 (cde),
		cole_direntry_getsec1 (cde),
		cole_direntry_getdays2 (cde),
		cole_direntry_getsec2 (cde));
	entry_name = cole_direntry_getname (cde);
	if (!isprint ((int)entry_name[0]))
		printf (" '\\x%02x%s'\n", entry_name[0], entry_name+1);
	else
		printf (" '%s'\n", entry_name);

	return 0;
}


/**
 * cole_opendir_rootdir:
 * @colefilesystem: filesystem of which the root directory will be opened.
 * @colerrno: error value (COLE_EMEMORY).
 *
 * Opens the root directory of the filesystem @colefilesystem as directory.
 *
 * Returns: a directory in success, or NULL in other case.
 */
COLEDIR *
cole_opendir_rootdir (COLEFS *colefilesystem, COLERRNO *colerrno)
{
	COLEDIR *ret;

	ret = malloc (sizeof (COLEDIR));
	if (ret == NULL) {
		if (colerrno != NULL) *colerrno = COLE_EMEMORY;
		return NULL;
	}
	ret->fs = colefilesystem;
	ret->entry = ret->fs->root;
	ret->visited_entry.dir = ret;
	ret->visited_entry.entry = ret->fs->tree[ ret->entry ].dir;

	return ret;
}


/**
 * cole_opendir_direntry:
 * @coledirentry: directory entry to be opened as directory.
 * @colerrno: error value (COLE_EISNOTDIR, COLE_EMEMORY).
 *
 * Opens a directory entry as directory.
 *
 * Returns: a directory in success, or NULL in other case.
 */
COLEDIR *
cole_opendir_direntry (COLEDIRENT *coledirentry, COLERRNO *colerrno)
{
	COLEDIR *ret;

	if (!cole_direntry_isdir (coledirentry)) {
		if (colerrno != NULL) *colerrno = COLE_EISNOTDIR;
		return NULL;
	}

	ret = malloc (sizeof (COLEDIR));
	if (ret == NULL) {
		if (colerrno != NULL) *colerrno = COLE_EMEMORY;
		return NULL;
	}
	ret->fs = coledirentry->dir->fs;
	ret->entry = coledirentry->entry;
	ret->visited_entry.dir = ret;
	ret->visited_entry.entry = ret->fs->tree[ ret->entry ].dir;

	return ret;
}


/**
 * cole_closedir:
 * @coledir: directory to be closed.
 * @colerrno: error value ().
 *
 * Closes the directory @coledir.
 * Currently this call always success.
 *
 * Returns: zero in success, no zero in other case.
 */
int
cole_closedir (COLEDIR *coledir, COLERRNO *colerrno)
{
	free (coledir);

	return 0;
}


COLEDIRENT *
cole_visiteddirentry (COLEDIR *coledir)
{
	if (coledir->visited_entry.entry == 0xffffffffUL)
		return NULL;

	return &coledir->visited_entry;
}


COLEDIRENT *
cole_nextdirentry (COLEDIR *coledir)
{
	if (coledir->visited_entry.entry == 0xffffffffUL)
		return NULL;

	coledir->visited_entry.entry =
		coledir->fs->tree [ coledir->visited_entry.entry ].next;

	if (coledir->visited_entry.entry == 0xffffffffUL)
		return NULL;

	return &coledir->visited_entry;
}


/**
 * cole_direntry_isdir:
 * @coledirentry: directory entry to be tested.
 *
 * Tests if the directory entry @coledirentry is a directory.
 *
 * Returns: no zero if it is a directory, zero in other case.
 */
int
cole_direntry_isdir (COLEDIRENT *coledirentry)
{
	return coledirentry->dir->fs->tree[ coledirentry->entry ].type ==
	       _COLE_TYPE_DIR;
}


/**
 * cole_direntry_isfile:
 * @coledirentry: directory entry to be tested.
 *
 * Tests if the directory entry @coledirentry is a file.
 *
 * Returns: no zero if it is a directory, zero in other case.
 */
int
cole_direntry_isfile (COLEDIRENT *coledirentry)
{
	return coledirentry->dir->fs->tree[ coledirentry->entry ].type ==
	       _COLE_TYPE_FILE;
}


char *
cole_direntry_getname (COLEDIRENT *coledirentry)
{
	return coledirentry->dir->fs->tree[ coledirentry->entry ].name;
}


size_t
cole_direntry_getsize (COLEDIRENT *coledirentry)
{
	/* FIXME is it cast needed? */
	return coledirentry->dir->fs->tree[ coledirentry->entry ].size;
}


long
cole_direntry_getsec1 (COLEDIRENT *coledirentry)
{
	/* seconds1 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledirentry->dir->fs->tree[ coledirentry->entry ].seconds1;
}


long
cole_direntry_getsec2 (COLEDIRENT *coledirentry)
{
	/* seconds2 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledirentry->dir->fs->tree[ coledirentry->entry ].seconds2;
}


long
cole_direntry_getdays1 (COLEDIRENT *coledirentry)
{
	/* days1 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledirentry->dir->fs->tree[ coledirentry->entry ].days1;
}


long
cole_direntry_getdays2 (COLEDIRENT *coledirentry)
{
	/* days2 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledirentry->dir->fs->tree[ coledirentry->entry ].days2;
}


char *
cole_dir_getname (COLEDIR *coledir)
{
	return coledir->fs->tree[ coledir->entry ].name;
}


size_t
cole_dir_getsize (COLEDIR *coledir)
{
	/* FIXME is it cast needed? */
	return coledir->fs->tree[ coledir->entry ].size;
}


long
cole_dir_getsec1 (COLEDIR *coledir)
{
	/* seconds1 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledir->fs->tree[ coledir->entry ].seconds1;
}


long
cole_dir_getsec2 (COLEDIR *coledir)
{
	/* seconds2 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledir->fs->tree[ coledir->entry ].seconds2;
}


long
cole_dir_getdays1 (COLEDIR *coledir)
{
	/* days1 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledir->fs->tree[ coledir->entry ].days1;
}


long
cole_dir_getdays2 (COLEDIR *coledir)
{
	/* days2 is U32, long is at least U32 in all plattforms, isn't it? */
	return coledir->fs->tree[ coledir->entry ].days2;
}


/**
 * cole_fopen:
 * @colefilesystem: filesystem in which @filename is in.
 * @filename: name of the file to open.
 * @colerrno: error value (COLE_EFILENOTFOUND, errors from calls
 * 			   cole_opendir_rootdir(), cole_fopen_direntry() and
 *			   cole_locate_filename()).
 *
 * Opens the file with the name @filename in the filesystem @colefilesystem.
 * Currently, @filename must begin with a '/' character, it means @filename is
 * the absolute filename.
 *
 * Returns: a file in success, or NULL in other case.
 */
struct _cole_fopen_info {
	COLEFILE *file;
	int succ;
	COLERRNO colerrno;
};
static COLE_LOCATE_ACTION_FUNC _cole_fopen_action;
COLEFILE *
cole_fopen (COLEFS *colefilesystem, char *filename, COLERRNO *colerrno)
{
	struct _cole_fopen_info info;

	if (cole_locate_filename (colefilesystem, filename, &info,
				  _cole_fopen_action, colerrno)) {
		/* couldn't locate the filename */
		/* colerrno is set */
		return NULL;
	}

	if (info.succ)
		return info.file;

	if (colerrno != NULL) *colerrno = info.colerrno;
	return NULL;
}
void
_cole_fopen_action (COLEDIRENT *cde, void *_info)
{
	struct _cole_fopen_info *info;

	info = (struct _cole_fopen_info *)_info;

	if (!cole_direntry_isfile(cde)) {
		info->colerrno = COLE_EFILENAMEISNOTFILE;
		info->succ = 0;
		return;
	}

	info->file = cole_fopen_direntry (cde, &info->colerrno);
	if (info->file == NULL) {
		/* colerrno is set */
		info->succ = 0;
		return;
	}

	info->succ = 1;
}


/**
 * cole_fopen_direntry:
 * @coledirentry: directory entry to be opened as file.
 * @colerrno: error value (COLE_EISNOTFILE, COLE_EMEMORY, COLE_ETMPNAM,
 * 			   COLE_EOPENFILE, COLE_EINVALIDFILESYSTEM, COLE_EREAD,
 * 			   COLE_EWRITE, COLE_EUNKNOWN).
 *
 * Opens a directory entry as file.
 *
 * Returns: a file in success, or NULL in other case.
 */
COLEFILE *
cole_fopen_direntry (COLEDIRENT *coledirentry, COLERRNO *colerrno)
{
	COLEFILE *ret;

	if (!cole_direntry_isfile (coledirentry)) {
		if (colerrno != NULL) *colerrno = COLE_EISNOTFILE;
		return NULL;
	}

	ret = malloc (sizeof (COLEFILE));
	if (ret == NULL) {
		if (colerrno != NULL) *colerrno = COLE_EMEMORY;
		return NULL;
	}
	ret->fs = coledirentry->dir->fs;
	ret->entry = coledirentry->entry;
	switch (__cole_extract_file (&ret->file, &ret->filename,
				ret->fs->tree[ ret->entry ].size,
				ret->fs->tree[ ret->entry ].start,
				ret->fs->BDepot, ret->fs->SDepot,
				ret->fs->sbfile, ret->fs->file)) {
	case 0:
		/* success */
		break;
	case 1:
		if (colerrno != NULL) *colerrno = COLE_EMEMORY;
		free (ret);
		return NULL;
	case 2:
		if (colerrno != NULL) *colerrno = COLE_ETMPNAM;
		free (ret);
		return NULL;
	case 3:
		if (colerrno != NULL) *colerrno = COLE_EOPENFILE;
		free (ret);
		return NULL;
	case 4:
		if (colerrno != NULL) *colerrno = COLE_EINVALIDFILESYSTEM;
		free (ret);
		return NULL;
	case 5:
		if (colerrno != NULL) *colerrno = COLE_EREAD;
		free (ret);
		return NULL;
	case 6:
		if (colerrno != NULL) *colerrno = COLE_EWRITE;
		free (ret);
		return NULL;
	default:
		if (colerrno != NULL) *colerrno = COLE_EUNKNOWN;
		free (ret);
		return NULL;
	}
	/* because the original fopen(3) leaves the file pointer
	   in the beginning */
	rewind (ret->file);
	ret->pos = 0;
	ret->filesize = ret->fs->tree[ ret->entry ].size;

	return ret;
}


/**
 * cole_fclose:
 * @colefile: file to be closed.
 * @colerrno: error value (COLE_ECLOSEFILE, CLOSE_EREMOVE).
 *
 * Closes the file @colefile.
 *
 * Returns: zero in sucess, no zero in other case.
 */
int
cole_fclose (COLEFILE *colefile, COLERRNO *colerrno)
{
	int ret;

	ret = 0;
	if (fclose (colefile->file) && !ret) {
		if (colerrno != NULL) *colerrno = COLE_ECLOSEFILE;
		ret = 1;
	}
	if (remove (colefile->filename) && !ret) {
		if (colerrno != NULL) *colerrno = COLE_EREMOVE;
		ret = 1;
	}
	free (colefile->filename);
	free (colefile);

	return ret;
}


/**
 * cole_fread:
 * @colefile: file to be read.
 * @ptr: memory location where the bytes will be stored.
 * @size: how many bytes will be read.
 * @colerrno: error value (COLE_EOF, COLE_EREAD, COLE_ESEEK).
 *
 * Reads @size bytes from @colefile and store them in the location given
 * by @ptr. If not success, the file position indicator is not changed.
 *
 * Returns: in sucess the number of bytes actually readed (maximum @size)
 * 	    or zero in other case.
 */
size_t
cole_fread (COLEFILE *colefile, void *ptr, size_t size, COLERRNO *colerrno)
{
	size_t bytes_read, bytes_to_read;

	/* Check to see if going past end... */
	if ((colefile->pos + size) > colefile->filesize)
		bytes_to_read = colefile->filesize - colefile->pos;
	else
		bytes_to_read = size;
	if (bytes_to_read <= 0)
		return 0;
	bytes_read = fread (ptr, 1, bytes_to_read, colefile->file);
	/* assert (size && (colefile->pos + bytes_read > colefile->pos)); */
	colefile->pos += bytes_read;
	/* assert (!feof (colefile->file)
		   && ftell (colefile->file) == colefile->pos); */
	if (bytes_read != size) {
		/* Linux man page says that fread returns a `short item count
		   (or zero)' when an error of end of file ocurrs. A short count
		   OR zero? We are hopping that fread always returns zero. */
		if (feof (colefile->file)) {
			if (colerrno != NULL) *colerrno = COLE_EOF;
			return 0;
		} else if (ferror (colefile->file)) {
			if (colerrno != NULL) *colerrno = COLE_EREAD;
			return 0;
		}
	}
	/* assert (size != 0 && bytes_read != 0); */
	/* assert (bytes_read <= size); */

	return bytes_read;
}


/**
 * cole_ftell:
 * @colefile: file of which the file position indicator will be get.
 *
 * Get the current value of the file position indicator for the file
 * @colefile.
 *
 * Returns: The file position.
 */
size_t
cole_ftell (COLEFILE *colefile)
{
	return colefile->pos;
}


/**
 * cole_fseek:
 * @colefile: file of which its file position indicator will be set.
 * @delta: number of bytes that the file position indicator will be moved.
 * @direction: from where start to count the @delta bytes.
 * @colerrno: error value (COLE_EFSEEKDELTA).
 *
 * Sets the value of the file position indicator for the file @colefile
 * @delta bytes from the beginning of the file, forward from the current
 * position, backward from the current position, or from the end of the
 * file, if @direction is COLE_SEEK_SET, COLE_SEEK_BACKWARD,
 * COLE_SEEK_FORWARD or COLE_SEEK_END, respectively.
 * The file position indicator will always be <= @colefile->filesize.
 * If you @delta is such that the previous line would not true, cole_fseek
 * fails.
 *
 * Returns: zero in success, no zero in other case.
 */
int
cole_fseek (COLEFILE *colefile, size_t delta, COLE_SEEK_FLAG direction,
	    COLERRNO *colerrno)
{
	if ((int)delta < 0) {
		if (colerrno != NULL) *colerrno = COLE_EFSEEKDELTA;
		return 1;
	}

	switch (direction) {
	case COLE_SEEK_SET:
		if (delta <= colefile->filesize) {
			colefile->pos = delta;
			return 0;
		} else {
			if (colerrno != NULL) *colerrno = COLE_EFSEEKDELTA;
			return 1;
		}
	case COLE_SEEK_END:
		if (delta <= colefile->filesize) {
			colefile->pos = colefile->filesize - delta;
			return 0;
		} else {
			if (colerrno != NULL) *colerrno = COLE_EFSEEKDELTA;
			return 1;
		}
	case COLE_SEEK_BACKWARD:
		if (delta <= colefile->pos) {
			colefile->pos = colefile->pos - delta;
			return 0;
		} else {
			if (colerrno != NULL) *colerrno = COLE_EFSEEKDELTA;
			return 1;
		}
	case COLE_SEEK_FORWARD:
		if (delta <= colefile->filesize - colefile->pos) {
			colefile->pos = colefile->pos + delta;
			return 0;
		} else {
			if (colerrno != NULL) *colerrno = COLE_EFSEEKDELTA;
			return 1;
		}
	default:
		if (colerrno != NULL) *colerrno = COLE_EFSEEKFLAG;
		return 1;
	}
}


/**
 * cole_frewind:
 * @colefile: file of which its file position indicator will be rewind.
 * @colerrno: error value (error from call cole_fseek()).
 * 
 * Sets the value of the file position indicator for the file @colefile
 * in the beginning of the file.
 *
 * Returns: zero in success, no zero in other case.
 */
int
cole_frewind (COLEFILE *colefile, COLERRNO *colerrno)
{
	if (cole_fseek (colefile, 0, COLE_SEEK_SET, colerrno))
		return 1;

	return 0;
}


/**
 * cole_fsize:
 * @colefile: file of which its size will be returned.
 * 
 * Returns the size in bytes of the file @colefile.
 */
size_t
cole_fsize (COLEFILE *colefile)
{
	return colefile->filesize;
}


/**
 * cole_feof:
 * @colefile: file to be tested.
 *
 * Tests if the end of file has been reached in @colefile.
 *
 * Returns: no zero if the end of file has been reached, zero in other case.
 */
int
cole_feof (COLEFILE *colefile)
{
	/* assert ((colefile->pos == colefile->fs->tree[ colefile->entry ].size)
	        && feof (colefile->file)); */
	return (colefile->pos == colefile->filesize);
}


/**
 * cole_recurse_tree:
 * @colefilesystem: filesystem to recurse.
 * @info: arbitrary pointer passed to the functions.
 * @inroot: pointer to the function that is called when start visiting root
 *          directory. It can be NULL.
 * @indirentry: pointer to the function that is called when start visiting any
 * 		directory entry (file or directory). It can be NULL.
 * @indir: pointer to the function that is called when start visiting a
 * 	   directory. It can be NULL.
 * @outdir: pointer to the function that is called when end visiting a
 * 	    directory. It can be NULL.
 * @visitdir: pointer to the function that is called to know if visit a
 * 	      directory. It can be NULL.
 * @colerrno: error value (errors from calls cole_opendir_rootdir(),
 * 			   cole_opendir_direntry(), cole_closedir() and
 *			   inroot, indirentry, indir, and outdir functions).
 *
 * Recurse the filesystem @colefilesystem, calling the functions pointed by
 * @inroot, @indirentry, @indir and @outdirectory when start visiting
 * root directory, start visiting any directory entry (file or directory),
 * start visiting a directory or end visiting a directory, respectively.
 * If @visitdir returns no zero or it's NULL, the directory is visited,
 * otherwise is not visited.
 * @info is a arbitrary pointer which is passed to the functions pointed by
 * @inroot, @indirentry, @indir and @outdirectory: it may be used to share
 * arbitrary information between them.
 *
 * Returns: zero if recursed all the tree, no zero in other case.
 */
static int
__cole_recurse_tree (COLEDIR *_cd, long level, void *info,
		     COLE_RECURSE_DIR_FUNC *inroot,
		     COLE_RECURSE_DIRENT_FUNC *indirentry,
		     COLE_RECURSE_DIR_FUNC *indir,
			COLE_RECURSE_DIR_FUNC *outdir,
		     COLE_RECURSE_VISIT_DIR_FUNC *visitdir,
		     COLERRNO *colerrno);
int
cole_recurse_tree (COLEFS *colefilesystem, void *info,
		     COLE_RECURSE_DIR_FUNC *inroot,
		     COLE_RECURSE_DIRENT_FUNC *indirentry,
		     COLE_RECURSE_DIR_FUNC *indir,
		     COLE_RECURSE_DIR_FUNC *outdir,
		     COLE_RECURSE_VISIT_DIR_FUNC *visitdir,
		     COLERRNO *colerrno)
{
	COLEDIR * cd;

	cd = cole_opendir_rootdir (colefilesystem, colerrno);
	if (cd == NULL)
		return 1;

	if (__cole_recurse_tree (cd, 1, info, inroot, indirentry, indir,
				 outdir, visitdir, colerrno)) {
		cole_closedir (cd, NULL);
		/* colerrno is set */
		return 1;
	}

	if (cole_closedir (cd, colerrno)) {
		/* colerrno is set */
		return 1;
	}

	return 0;
}
int
__cole_recurse_tree (COLEDIR *_cd, long level, void *info,
		     COLE_RECURSE_DIR_FUNC *inroot,
		     COLE_RECURSE_DIRENT_FUNC *indirentry,
		     COLE_RECURSE_DIR_FUNC *indir,
			COLE_RECURSE_DIR_FUNC *outdir,
		     COLE_RECURSE_VISIT_DIR_FUNC *visitdir,
		     COLERRNO *colerrno)
{
/*
 * ATTENTION: if you modify __cole_recurse_tree() so it modifies colerrno
 * besides in calling inroot, indirentry, indir, outdir, cole_opendir_direntry
 * or cole_closedir:
 * 	Modify colerrno comment in the functions that call it,
 *	ie. cole_recurse_tree().
 */

	/* ATTENTION: This is a recursive function */
	COLEDIRENT * cde;
	COLEDIR * cd;

	if (level == 1) {
		/* The following lines are only executed on Root Entry */
		if (inroot != NULL) {
			if ( (*inroot) (_cd, info, colerrno) ) {
				/* colerrno is set */
				return 1;
			}
		}
	}

	/* Iterate through childrens */
	for (cde = cole_visiteddirentry (_cd); cde != NULL;
	     cde = cole_nextdirentry (_cd)) {
		if (indirentry != NULL) {
			if ( (*indirentry) (cde, info, colerrno) ) {
				/* colerrno is set */
				return 1;
			}
		}

		/* RECURSIVE CALL */
		if (cole_direntry_isdir (cde)) {
			cd = cole_opendir_direntry (cde, colerrno);
			if (cd == NULL) {
				/* colerrno is set */
				return 1;
			}

			if (indir != NULL) {
				if ( (*indir) (cd, info, colerrno) ) {
					/* colerrno is set */
					cole_closedir (cd, NULL);
					return 1;
				}
			}

			if ( (visitdir == NULL) || 
			     ((*visitdir)(cd, info)) ) {
				if (__cole_recurse_tree (cd, level + 1, info,
							 inroot, indirentry,
							 indir, outdir,
							 visitdir, colerrno)) {
					/* colerrno is set */
					cole_closedir (cd, NULL);
					return 1;
				}
			}

			if (outdir != NULL) {
				if ( (*outdir) (cd, info, colerrno) ) {
					/* colerrno is set */
					cole_closedir (cd, NULL);
					return 1;
				}
			}

			if (cole_closedir (cd, colerrno)) {
				/* colerrno is set */
				return 1;
			}
		}
	}

	return 0;
}


/**
 * cole_locate_filename:
 * @colefilesystem: filesystem where to locate @filename.
 * @filename: name of the file or directory to be located.
 * @info: arbitrary pointer passed to @action.
 * @action: pointer to the function that is called when founding @filename.
 * @colerrno: error value (COLE_EUNKNOWN, COLE_EMEMBERISNOTDIR,
 * 			   COLE_EFILENOTFOUND, COLE_EBROKENFILENAME, errors
 * 			   from call cole_recurse_tree()).
 *
 * Locate the @filename in the filesystem @colefilesystem, calling @action when
 * it's found. @info is arbitrary pointer passed to @action.
 * Currently, @filename must begin with a '/' character, it means @filename is
 * the absolute filename.
 *
 * Returns: zero in success, 1  in other case.
 */
struct __cole_locate_filenameinfo {
	COLE_LOCATE_ACTION_FUNC *action;
	void *info;
	char *filename;
	char *current;
	int visitdir;
};
static COLE_RECURSE_DIRENT_FUNC __cole_locate_filename_indirentry;
static COLE_RECURSE_VISIT_DIR_FUNC __cole_locate_filename_visitdir;
int
cole_locate_filename (COLEFS *colefilesystem, char *filename,
		      void *info,
		      COLE_LOCATE_ACTION_FUNC *action,
		      COLERRNO *colerrno)
{
	struct __cole_locate_filenameinfo _info;
	COLERRNO _colerrno;

	/* FIXME allow no absolute paths */
	if (filename[0] != '/') {
		if (colerrno != NULL) *colerrno = COLE_EBROKENFILENAME;
		return 1;
	}

	_info.action = action;
	_info.info = info;
	_info.filename = filename;
	_info.current = filename + 1;

	if (cole_recurse_tree (colefilesystem, &_info, NULL,
			   __cole_locate_filename_indirentry, NULL, NULL,
			   __cole_locate_filename_visitdir,
			   &_colerrno)) {
		if (_colerrno == COLE_ELAST+1) {
			/* file was found */
			return 0;
		}
		if (colerrno != NULL) *colerrno = _colerrno;
		return 1;
	}

	if (colerrno != NULL) *colerrno = COLE_EFILENOTFOUND;
	return 1;
}
int
__cole_locate_filename_visitdir (COLEDIR *cd, void *info)
{
	return ((struct __cole_locate_filenameinfo *)info)->visitdir;
}
int
__cole_locate_filename_indirentry (COLEDIRENT *cde, void *_info,
				COLERRNO *colerrno)
{
	char *entry_name;
	struct __cole_locate_filenameinfo *info;
	char *pcurrent;
	char *pentry_name;

	info = (struct __cole_locate_filenameinfo *)_info;
	entry_name = cole_direntry_getname (cde);
	for (pcurrent = info->current, pentry_name = entry_name;
	     *pcurrent && *pentry_name && *pcurrent != '/';
	     pcurrent++, pentry_name++) {
		if (*pcurrent != *pentry_name) {
			info->visitdir = 0;	/* don't visit this directory */
			return 0;		/* don't break recurse */
		}
	}
	switch (*pentry_name) {
	case 0:
		switch (*pcurrent) {
		case '/':
			if (!cole_direntry_isdir (cde)) {
				if (colerrno != NULL)
					*colerrno = COLE_EMEMBERISNOTDIR;
				return 1;	/* break recurse */
			}
			pcurrent++;		/* jump the '/' character */
			info->current = pcurrent;

			/* check if it's the last component of filename */
			if (!(*info->current)) {
				/* last component of filename reached */
				if (info->action != NULL) {
					(*(info->action)) (cde, info->info);
				}

				if (colerrno != NULL) *colerrno = COLE_ELAST+1;
				return 1;               /* break recurse */
			}
			info->visitdir = 1;	/* visit this directory */
			return 0;		/* don't break recurse */
		case 0:
			/* last component of filename reached */
                        if (info->action != NULL) {
				(*(info->action)) (cde, info->info);
                        }

			if (colerrno != NULL) *colerrno = COLE_ELAST+1;
			return 1;		/* break recurse */
		default:
			info->visitdir = 0;	/* don't visit this directory */
			return 0;		/* don't break recurse */
		}
	default:
		switch (*pcurrent) {
		case 0:
			info->visitdir = 0;	/* don't visit this directory */
			return 0;		/* don't break recurse */
		case '/':
			info->visitdir = 0;	/* don't visit this directory */
			return 0;		/* don't break recurse */
		default:
			if (colerrno != NULL) *colerrno = COLE_EUNKNOWN;
			return 1;		/* break recurse */
		}
	}

}

