/*
   OLEdecode - Decode Microsoft OLE files into its components.
   Copyright (C) 1998, 1999  Andrew Scriven

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
   Released under GPL, written by 
   Andrew Scriven <andy.scriven@research.natpower.co.uk>

   Copyright (C) 1998, 1999
   Andrew Scriven
 */
/*
   -----------------------------------------------------------------------
   Andrew Scriven
   Research and Engineering
   Electron Building, Windmill Hill, Whitehill Way, Swindon, SN5 6PB, UK
   Phone (44) 1793 896206, Fax (44) 1793 896251
   -----------------------------------------------------------------------
 */
/*
   *Extremely* modified by Arturo Tena <arturo@directmail.org>
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <assert.h>

#if !(defined( __BORLANDC__ ) || defined( __WIN32__ ))
#include "cole.h"
#include "config.h"
#include <unistd.h>		/* for unlink() */
#else
#include "cole.h.in"
#endif
/* FIXME: replace all VERBOSE with COLE_VERBOSE */
#ifdef COLE_VERBOSE
#define VERBOSE
#else
#undef VERBOSE
#endif
#include "support.h"
#include "internal.h"


#define ENTRYCHUNK 20		/* number of entries in root_list and sbd_list
				   will be added each time. must be at least 1 */

#define MIN(a,b) ((a)<(b) ? (a) : (b))


	/* reorder pps tree, from tree structure to a linear one,
	   and write the level numbers, returns zero if OLE format fails,
	   returns no zero if success */
static int reorder_pps_tree (pps_entry * root_pps, U16 level);
	/* free memory used (except the pps tree) */
static void ends (void);
	/* close and remove files in the tree */
/* closeOLEtreefiles --- outdated because not to generate the
   real files by now --- cole 2.0.0 */
/*
  static void closeOLEtreefiles (pps_entry * tree, U32 root);
 */
/*
   Verbose pps tree.
   Input: pps_list: stream list.
          root_pps: root pps.
          level:    how much levels will be extracted.
   Output: none.
 */
static void verbosePPSTree (pps_entry * pps_list, U32 root_pps, int level);


static FILE *input;
static U8 *Block;
static U8 *Blockx;
static U8 *BDepot, *SDepot, *Root;
static pps_entry *pps_list;
static U32 num_of_pps;
static FILE *sbfile;
/* sbfilename is stored in *_sbfilename instead -- cole 2.0.0 */
/* static char sbfilename[L_tmpnam]; */
static U32 *sbd_list;
static U32 *root_list;


int __OLEdecode (char *OLEfilename, pps_entry ** stream_list, U32 * root,
	     U8 **_BDepot, U8 **_SDepot, FILE **_sbfile, char **_sbfilename,
	     FILE **_input,
	     U16 max_level)
{
  int c;
  U32 num_bbd_blocks;
  U32 num_xbbd_blocks;
  U32 bl;
  U32 i, j, len;
  U8 *s, *p, *t;
  long FilePos;
  /* FilePos is long, not U32, because second argument of fseek is long */

  /* initialize static variables */
  input = sbfile = NULL;
  Block = Blockx = BDepot = SDepot = Root = NULL;
  pps_list = NULL;
  num_of_pps = 0;
/* sbfilename is stored in *_sbfilename instead -- cole 2.0.0 */
/*  sbfilename[0] = 0; */
  root_list = sbd_list = NULL;
  /* initalize return parameters */
  *stream_list = NULL;

  /* open input file */
  verbose ("open input file");
  input = fopen (OLEfilename, "rb");
  test_exitf (input != NULL, 4, ends ());
  *_input = input;

  /* fast check type of file */
  verbose ("fast testing type of file");
  test_exitf ((c = getc (input)) != EOF, 5, ends ());
  test_exitf (ungetc (c, input) != EOF, 5, ends ());
/* test_exitf (!isprint (c), 8, ends ()); OpenBSD suggestion to comment this out */
  test_exitf (c == 0xd0, 9, ends ());

  /* read header block */
  verbose ("read header block");
  Block = (U8 *) malloc (0x0200);
  test_exitf (Block != NULL, 10, ends ());
  fread (Block, 0x0200, 1, input);
  test_exitf (!ferror (input), 5, ends ());

  /* really check type of file */
  rewind (input);
  verbose ("testing type of file");
  test_exitf (fil_sreadU32 (Block) != 0xd0cf11e0UL, 9, ends ());
  test_exitf (fil_sreadU32 (Block + 0x04) != 0xa1b11ae1UL, 9, ends ());


  /* read big block depot */
  verbose ("read big block depot (bbd)");
  num_bbd_blocks = fil_sreadU32 (Block + 0x2c);
  num_xbbd_blocks = fil_sreadU32 (Block + 0x48);
  verboseU32 (num_bbd_blocks);
  verboseU32 (num_xbbd_blocks);
  BDepot = malloc (0x0200 * (num_bbd_blocks + num_xbbd_blocks));
  test_exitf (BDepot != NULL, 10, ends ());
  *_BDepot = BDepot;
  s = BDepot;
  assert (num_bbd_blocks <=  (0x0200 / 4 - 1) * num_xbbd_blocks +
			     (0x0200 / 4) - 19);
  /* the first 19 U32 in header does not belong to bbd_list */
  for (i = 0; i < MIN (num_bbd_blocks, 0x0200 / 4 - 19); i++)
    {
      /* note: next line may be needed to be cast to long in right side */
      FilePos = 0x0200 * (1 + fil_sreadU32 (Block + 0x4c + (i * 4)));
      assert (FilePos >= 0);
      test_exitf (!fseek (input, FilePos, SEEK_SET), 5, ends ());
      fread (s, 0x0200, 1, input);
      test_exitf (!ferror (input), 5, ends ());
      s += 0x0200;
    }

  Blockx = (U8 *) malloc (0x0200);
  test_exitf (Blockx != NULL, 10, ends ());
  bl = fil_sreadU32 (Block + 0x44);
  for (i = 0; i < num_xbbd_blocks; i++)
  {
      FilePos = 0x0200 * (1 + bl);
	 assert (FilePos >= 0);
      test_exitf (!fseek (input, FilePos, SEEK_SET), 5, ends ());
      fread (Blockx, 0x0200, 1, input);
      test_exitf (!ferror (input), 5, ends ());

    for (j=0; j < 0x0200 / 4 - 1;j++)
                             /* last U32 is for the next bl */
    {
      if (fil_sreadU32 (Blockx + (j * 4)) == 0xfffffffeUL ||
          fil_sreadU32 (Blockx + (j * 4)) == 0xfffffffdUL ||
          fil_sreadU32 (Blockx + (j * 4)) == 0xffffffffUL)
	break;
      /* note: next line may be needed to be cast to long in right side */
      FilePos = 0x0200 * (1 + fil_sreadU32 (Blockx + (j * 4)));
      assert (FilePos >= 0);
      test_exitf (!fseek (input, FilePos, SEEK_SET), 5, ends ());
      fread (s, 0x0200, 1, input);
      test_exitf (!ferror (input), 5, ends ());
      s += 0x0200;
    }

    bl = fil_sreadU32 (Blockx + 0x0200 - 4);
  }
  verboseU8Array (BDepot, (num_bbd_blocks+num_xbbd_blocks), 0x0200);


  /* extract the sbd block list */
  verbose ("extract small block depot (sbd) block list");
  sbd_list = malloc (ENTRYCHUNK * 4);
  test_exitf (sbd_list != NULL, 10, ends ());
  sbd_list[0] = fil_sreadU32 (Block + 0x3c);
  /* -2 signed long int == 0xfffffffe unsinged long int */
  for (len = 1; sbd_list[len - 1] != 0xfffffffeUL; len++)
    {
      test_exitf (len != 0, 5, ends ());	/* means file is too big */
      /* if memory allocated in sbd_list is all used, allocate more memory */
      if (!(len % ENTRYCHUNK))
	{
	  U32 *newspace;
	  newspace = realloc (sbd_list,
			      (1 + len / ENTRYCHUNK) * ENTRYCHUNK * 4);
	  test_exitf (newspace != NULL, 10, ends ());
	  sbd_list = newspace;
	}
	 sbd_list[len] = fil_sreadU32 (BDepot + (sbd_list[len - 1] * 4));
      /*verboseU32 (len);*/
      /*verboseU32 (sbd_list[0]);*/
      /*verboseU32 (sbd_list[1]);*/
      if (sbd_list[len] != 0xfffffffeUL)
	test_exitf (sbd_list[len] <= num_bbd_blocks * 0x0200 - 4, 5, ends ());
      test_exitf (sbd_list[len] != 0xfffffffdUL &&
		  sbd_list[len] != 0xffffffffUL,
		  5, ends ());
    }
  len--;
  verboseU32Array (sbd_list, len+1);
  /* read in small block depot, if there's any small block */
  if (len == 0)
    {
      SDepot = NULL;
      verbose ("not read small block depot (sbd): there's no small blocks");
    }
  else
    {
      verbose ("read small block depot (sbd)");
      SDepot = malloc (0x0200 * len);
	 test_exitf (SDepot != NULL, 10, ends ());
      s = SDepot;
      for (i = 0; i < len; i++)
	{
	  FilePos = 0x0200 * (1 + sbd_list[i]);
	  assert (FilePos >= 0);
	  test_exitf (!fseek (input, FilePos, SEEK_SET), 5, ends ());
	  fread (s, 0x0200, 1, input);
	  test_exitf (!ferror (input), 5, ends ());
	  s += 0x200;
	}
      verboseU8Array (SDepot, len, 0x0200);
    }
  *_SDepot = SDepot;


  /* extract the root block list */
  verbose ("extract root block depot (root) block list");
  root_list = malloc (ENTRYCHUNK * 4);
  test_exitf (root_list != NULL, 10, ends ());
  root_list[0] = fil_sreadU32 (Block + 0x30);
  for (len = 1; root_list[len - 1] != 0xfffffffeUL; len++)
    {
      test_exitf (len != 0, 5, ends ());	/* means file is too long */
      /* if memory allocated in root_list is all used, allocate more memory */
      if (!(len % ENTRYCHUNK))
	{
	  U32 *newspace;
	  newspace = realloc (root_list,
			      (1 + len / ENTRYCHUNK) * ENTRYCHUNK * 4);
	  test_exitf (newspace != NULL, 10, ends ());
	  root_list = newspace;
	}
      root_list[len] = fil_sreadU32 (BDepot + (root_list[len - 1] * 4));
      test_exitf (root_list[len] != 0xfffffffdUL && root_list[len] !=
		  0xffffffffUL, 5, ends ());
    }
  len--;
  verboseU32Array (root_list, len+1);
  /* read in root block depot */
  verbose ("read in root block depot (Root)");
  Root = malloc (0x0200 * len);
  test_exitf (Root != NULL, 10, ends ());
  s = Root;
  for (i = 0; i < len; i++)
    {
      FilePos = 0x0200 * (root_list[i] + 1);
      assert (FilePos >= 0);
      test_exitf (!fseek (input, FilePos, SEEK_SET), 5, ends ());
      fread (s, 0x0200, 1, input);
      test_exitf (!ferror (input), 5, ends ());
      s += 0x200;
    }
  verboseU8Array (Root, len, 0x0200);


  /* assign space for pps list */
  verbose ("read pps list");
  num_of_pps = len * 4;		/* each sbd block have 4 pps */
  *stream_list = pps_list = malloc (num_of_pps * sizeof (pps_entry));
  test_exitf (pps_list != NULL, 10, ends ());
  /* read pss entry details and look out for "Root Entry" */
  verbose ("read pps entry details");
  for (i = 0; i < num_of_pps; i++)
    {
	 U16 size_of_name;

      s = Root + (i * 0x80);

      /* read the number */
      pps_list[i].ppsnumber = i;

      /* read the name */
      size_of_name = (U16)MIN (0x40, fil_sreadU16 (s + 0x40));
	 pps_list[i].name[0] = 0;
      if (size_of_name == 0)
	continue;
      for (p = (U8 *) pps_list[i].name, t = s;
	   t < s + size_of_name; t++)
	*p++ = *t++;
      /* makes visible the non printable first character */
      /* if (!isprint (pps_list[i].name[0]) && pps_list[i].name[0])
	pps_list[i].name[0] += 'a'; */

	 /* read the pps type */
	 pps_list[i].type = *(s + 0x42);
	 if (pps_list[i].type == 5)
	{
	  assert (i == 0);
	  *root = i;		/* this pps is the root */
	}

	 /* read the others fields */
	 pps_list[i].previous = fil_sreadU32 (s + 0x44);
	 pps_list[i].next = fil_sreadU32 (s + 0x48);
	 pps_list[i].dir = fil_sreadU32 (s + 0x4c);
	 pps_list[i].start = fil_sreadU32 (s + 0x74);
	 pps_list[i].size = fil_sreadU32 (s + 0x78);
	 pps_list[i].seconds1 = fil_sreadU32 (s + 0x64);
	 pps_list[i].seconds2 = fil_sreadU32 (s + 0x6c);
	 pps_list[i].days1 = fil_sreadU32 (s + 0x68);
	 pps_list[i].days2 = fil_sreadU32 (s + 0x70);
    }

  /* NEXT IS VERBOSE verbose */
#ifdef VERBOSE
  {
    U32 i;
    printf ("before reorder pps tree\n");
    printf ("pps    type    prev     next      dir start   level size     name\n");
    for (i = 0; i < num_of_pps; i++)
	 {
	if (!pps_list[i].name[0])
	{
	  printf (" -\n");
	  continue;
	}
	printf ("%08lx ", pps_list[i].ppsnumber);
	printf ("%d ", pps_list[i].type);
	printf ("%08lx ", pps_list[i].previous);
	printf ("%08lx ", pps_list[i].next);
	printf ("%08lx ", pps_list[i].dir);
	printf ("%08lx ", pps_list[i].start);
	printf ("%04x ", pps_list[i].level);
	printf ("%08lx ", pps_list[i].size);
	printf ("'%c", !isprint (pps_list[i].name[0]) ? ' ' : pps_list[i].name[0]);
	printf ("%s'\n", pps_list[i].name+1);
      }
  }
#endif

  /* go through the tree made with pps entries, and reorder it so only the
     next link is used (move the previous-link-children to the last visited
     next-link-children) */
  test_exitf (reorder_pps_tree (&pps_list[*root], 0), 9, ends ());

  /* NEXT IS VERBOSE verbose */
#ifdef VERBOSE
  {
    U32 i;
    printf ("after reorder pps tree\n");
    printf ("pps    type    prev     next      dir start   level size     name\n");
    for (i = 0; i < num_of_pps; i++)
      {
        if (!pps_list[i].name[0])
	   {
          printf (" -\n");
          continue;
	   }
	printf ("%08lx ", pps_list[i].ppsnumber);
	printf ("%d ", pps_list[i].type);
	printf ("%08lx ", pps_list[i].previous);
	printf ("%08lx ", pps_list[i].next);
	printf ("%08lx ", pps_list[i].dir);
	printf ("%08lx ", pps_list[i].start);
	printf ("%04x ", pps_list[i].level);
	printf ("%08lx ", pps_list[i].size);
	printf ("'%c", !isprint (pps_list[i].name[0]) ? ' ' : pps_list[i].name[0]);
	printf ("%s\n", pps_list[i].name+1);
	 }
  }

  /* NEXT IS VERBOSE verbose */
  verbosePPSTree (pps_list, *root, 0);
#endif


  /* generates pps real files */
  /* NOTE: by this moment, the pps tree,
	wich is made with pps_list entries, is reordered */
  verbose ("create pps files");
  {
    U8 *Depot;
    FILE *OLEfile, *infile;
    U16 BlockSize, Offset;
    size_t bytes_to_read;
    U32 pps_size, pps_start;

    assert (num_of_pps >= 1);
    /* i < 1 --- before i < num_of_pps --- changed so not to generate the
	  real files by now --- cole 2.0.0 */
    /* may be later we can rewrite this code in order to only extract the
	  sbfile, may be using __cole_extract_file call to avoid duplicated
	  code --- cole 2.0.0 */
    for (i = 0; i < 1; i++)
	 {
	pps_list[i].filename[0] = 0;

	/* storage pps and non-valid-pps (except root) does not need files */
	/* because FlashPix file format have a root of type 5 but with no name,
	   we must to check if the non-valid-pps is root */
	if (pps_list[i].type == 1
	    || (!pps_list[i].name[0] && pps_list[i].type != 5))
	    continue;
	/* pps that have level > max_level will not be extracted */
	if (max_level != 0 && pps_list[i].level > max_level)
	    continue;

	pps_size = pps_list[i].size;
	pps_start = pps_list[i].start;
/* FIXME MARK 2 */
	/* How we get sure pps_start doesn't point to a block bigger than the
	   real file (input of sbfile) have? */

	/* create the new file */
	if (pps_list[i].type == 5)
	/* root entry, sbfile must be generated */
	{
	    if (SDepot == NULL) {
		/* if there are not small blocks, not generate sbfile */
		*_sbfilename = NULL;
		*_sbfile = NULL;
		break;
	    }
	    assert (i == *root);
	    assert (i == 0);
	    /* tmpnam (sbfile) and next calls --- commented out so not to
		  generate the real files by now --- sbfilename is stored in
		  *_sbfilename instead --- cole 2.0.0
		*/
	    /*
		 tmpnam (sbfilename);
		 test_exitf (sbfilename[0], 7, ends ());
		 sbfile = OLEfile = fopen (sbfilename, "wb+");
		 test_exitf (OLEfile != NULL, 7, ends ());
		 verboseS (sbfilename);
		*/
#if defined( __WIN32__ ) || defined( __BORLANDC__ )
	    *_sbfilename = malloc (TMPNAM_LEN);
	    test_exitf (*_sbfilename != NULL, 10, ends ());
	    tmpnam (*_sbfilename);
	    test_exitf (*_sbfilename[0], 7, ends ());
	    sbfile = OLEfile = fopen (*_sbfilename, "w+b");
#else
	{
		int ret;

		*_sbfilename = malloc (TMPNAM_LEN);
		test_exitf (*_sbfilename != NULL, 10, ends ());

		strcpy(*_sbfilename, "/tmp/xlHtmlXXXXXX");
		ret = mkstemp(*_sbfilename);
		if (ret == -1)	{
			free(*_sbfilename);
			test_exitf (ret == -1, 7, ends ());
		}

		sbfile = OLEfile = fdopen(ret, "w+b");
		/* unlink() is called so this file deletes when we are done with it */
		unlink(*_sbfilename);
	}
#endif
	    *_sbfile = sbfile;
	    test_exitf (OLEfile != NULL, 7, ends ());
	    verboseS (*_sbfilename);
	}
	else
	/* other entry, save in a file */
	{
	    /* this branch is never executed now */
#if defined( __WIN32__ ) || defined( __BORLANDC__ )
		tmpnam (pps_list[i].filename);
		test_exitf (pps_list[i].filename[0], 7, ends ());
		verbose(pps_list[i].name + (!isprint(pps_list[i].name[0]) ? 1 : 0));
		OLEfile = fopen (pps_list[i].filename, "wb");
#else
		int ret;

		strcpy(pps_list[i].filename, "/tmp/xlHtmlXXXXXX");
		ret = mkstemp(pps_list[i].filename);
		test_exitf (ret == -1, 7, ends ());
		OLEfile = fdopen(ret, "wb");
#endif
		test_exitf (OLEfile != NULL, 7, ends ());
		verbose (pps_list[i].filename);
	}

	if (pps_size >= 0x1000 /*is in bbd */  ||
	    OLEfile == sbfile /*is root */ )
	{
	    /* read from big block depot */
	    Offset = 1;
	    BlockSize = 0x0200;
	    assert (input != NULL);
	    assert (BDepot != NULL);
	    infile = input;
	    Depot = BDepot;
	}
	else
	{
	    /* read from small block file */
	    Offset = 0;
	    BlockSize = 0x40;
	    assert (sbfile != NULL);
	    assert (SDepot != NULL);
	    infile = sbfile;
	    Depot = SDepot;
	}

	/* -2 signed long int == 0xfffffffe unsinged long int */
	while (pps_start != 0xfffffffeUL)
	{
#ifdef VERBOSE
	    printf ("reading pps %08lx block %08lx from %s\n",
		    pps_list[i].ppsnumber, pps_start,
		 Depot == BDepot ? "big block depot" : "small block depot");
#endif
	    FilePos = (pps_start + Offset) * BlockSize;
	    assert (FilePos >= 0);
	    bytes_to_read = MIN ((U32)BlockSize, pps_size);
	    fseek (infile, FilePos, SEEK_SET);
	    fread (Block, bytes_to_read, 1, infile);
	    test_exitf (!ferror (infile), 5, ends ());
	    fwrite (Block, bytes_to_read, 1, OLEfile);
	    test_exitf (!ferror (infile), 5, ends ());
	    pps_start = fil_sreadU32 (Depot + (pps_start * 4));
	    pps_size -= MIN ((U32)BlockSize, pps_size);
	    if (pps_size == 0)
		 pps_start = 0xfffffffeUL;
	}
	if (OLEfile == sbfile)
	  /* if small block file generated */
	  rewind (OLEfile);	/* rewind because we will read it later */
	/*else if (!fclose (OLEfile))*/	/* close the pps file */
	  /* don't know what to do here */
	  /*;*/
	}
    /* if (sbfile != NULL) --- commented out so conservate sbfile
	  open --- cole 2.0.0 */
    /*
    if (sbfile != NULL)
	 {
	fclose (sbfile);
	  if (!remove (sbfilename)) ;
	sbfile = NULL;
	 }
	*/
  }
  ends ();
  return 0;
}


/* reorder pps tree and write levels */
/* not sure if it is safe declare last_next_link_visited
   inside reorder_pps_tree function */
static U32 *last_next_link_visited;
static int reorder_pps_tree (pps_entry * node, U16 level)
{
  /* NOTE: in next, previous and dir link,
	0xffffffff means point to nowhere (NULL) */

  node->level = level;

  /* reorder subtrees, if there's any */
  if (node->dir != 0xffffffffUL)
  {
    if (node->dir > num_of_pps || !pps_list[node->dir].name[0])
	 return 0;
    else if (!reorder_pps_tree (&pps_list[node->dir], (U16)(level + 1)))
	 return 0;
  }

  /* reorder next-link subtree, saving the most next link visited */
  if (node->next != 0xffffffffUL)
    {
	 if (node->next > num_of_pps || !pps_list[node->next].name[0])
	return 0;
	 else if (!reorder_pps_tree (&pps_list[node->next], level))
	return 0;
    }
  else
    last_next_link_visited = &node->next;

  /* move the prev child to the next link and reorder it, if there's any */
  if (node->previous != 0xffffffffUL)
  {
    if (node->previous > num_of_pps || !pps_list[node->previous].name[0])
	 return 0;
    else
	 {
	*last_next_link_visited = node->previous;
	node->previous = 0xffffffffUL;
	if (!reorder_pps_tree (&pps_list[*last_next_link_visited], level))
	  return 0;
	 }
  }
  return 1;
}


/* verbose pps tree */
void verbosePPSTree (pps_entry * pps_list, U32 start_entry, int level)
{
  U32 entry;
  int i;
#if __GNUC__ == 2
static char cff[] = "cole235711";
#define nextff(var) static void * nextff_var = (&nextff_var); \
nextff_var=&var;
nextff (cff);
#endif


  for (entry = start_entry; entry != 0xffffffffUL; entry = pps_list[entry].next)
    {
	 if (pps_list[entry].type == 2)
	{
	  for (i = 0; i < level * 3; i++)
	    printf (" ");
	  printf ("FILE %02lx %8ld '%c%s'\n", pps_list[entry].ppsnumber,
		  pps_list[entry].size, !isprint((int)pps_list[entry].name[0]) ? ' ' : pps_list[entry].name[0], pps_list[entry].name+1);
	}
	 else
	{
	  for (i = 0; i < level * 3; i++)
	    printf (" ");
	  printf ("DIR  %02lx '%c%s'\n", pps_list[entry].ppsnumber,
		  !isprint((int)pps_list[entry].name[0]) ? ' ' : pps_list[entry].name[0], pps_list[entry].name+1);
	  verbosePPSTree (pps_list, pps_list[entry].dir, level + 1);
	}
    }
}


/* closeOLEtreefiles --- outdated because not to generate the
   real files by now --- cole 2.0.0 */
/*
#define freeNoNULL(x) { if ((x) != NULL) free (x); }
static int errorClosingTreeFiles;
void
closeOLEtreefiles (pps_entry * tree, U32 root)
{
#ifdef VERBOSE
  printf ("Visiting entry 0x%08lx to erase file\n",
	  tree[root].ppsnumber);
#endif
  if (tree[root].previous != 0xffffffffUL)
    closeOLEtreefiles (tree, tree[root].previous);
  if (tree[root].next != 0xffffffffUL)
    closeOLEtreefiles (tree, tree[root].next);
  if (tree[root].type != 2 && tree[root].dir != 0xffffffffUL)
    {
#ifdef VERBOSE
	 printf ("Going down to directory 0x%08lx to erase files\n",
		 tree[root].dir);
#endif
	 closeOLEtreefiles (tree, tree[root].dir);
    }
  else if (tree[root].type == 2)
    if (!remove (tree[root].filename))
    {
#ifdef VERBOSE
	 printf ("Success removing %s\n", tree[root].filename);
#endif
    }
  else
    {
#ifdef VERBOSE
	 printf ("Failed to remove %s\n", tree[root].filename);
#endif
	 errorClosingTreeFiles = 1;
    }
}
*/

/* freeOLEtree is now useless: it only free tree -- cole 2.0.0 */
/*
   Free all the memory allocated in the tree.
   Output: 0 = Sucess.
   .       6 = Error removing some temporal stream files.
 */
/*
int freeOLEtree (pps_entry * tree);
*/
/*
int
freeOLEtree (pps_entry * tree)
{
*/
  /* errorCloseingTreeFiles --- doesn't exists because closeOLEtreefiles
	was outdated because not to generate the real files by
	now --- cole 2.0.0 */
  /*
    errorClosingTreeFiles = 0;
  */
/*
  if (tree != NULL)
    {
*/
	 /* closeOLEtreefiles --- outdated because not to generate the
	    real files by now --- cole 2.0.0 */
	 /*
	   closeOLEtreefiles (tree, 0);
	  */
/*
	 free (tree);
    }
 */
  /* errorCloseingTreeFiles --- doesn't exists because closeOLEtreefiles
	was outdated because not to generate the real files by
	now --- cole 2.0.0 */
  /*return*/ /* errorClosingTreeFiles ? 6 : */ /*0;*/
/*}*/


/* free memory used (except the pps tree) */
#define freeNoNULL(x) { if ((x) != NULL) free (x); }
static void ends (void)
{
  /* if (input != NULL) and next lines --- commented out so conservate input
	file open --- cole 2.0.0 */
  /*
  if (input != NULL)
    fclose (input);
  */
  freeNoNULL (Block);
  freeNoNULL (Blockx);
  /* freeNoNULL (BDepot) and next line --- commented out so conservate
	depots --- cole 2.0.0 */
  /*
    freeNoNULL (BDepot);
    freeNoNULL (SDepot);
  */
  freeNoNULL (Root);
  freeNoNULL (sbd_list);
  freeNoNULL (root_list);
  /* if (sbfile != NULL) and next lines --- commented out so conservate sbfile
	open --- cole 2.0.0 */
  /*
  if (sbfile != NULL)
    {
	 fclose (sbfile);
	 if (!remove (sbfilename)) ;
    }
   */
}

#undef VERBOSE

