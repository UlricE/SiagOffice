/*
   Support - Provides some big and little endian abstraction functions,
             besides another things.
   Copyright (C) 1999  Roberto Arturo Tena Sanchez 

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */
/*
   Arturo Tena <arturo@directmail.org>
 */
/*
  Some code was from Caolan, but I have replaced all the code,
  now all code here is mine, so I changed copyright announce in cole-1.0.0.
     Arturo Tena
 */



#include "support.h"


U16
fil_sreadU16 (U8 * in)
{
#ifdef WORDS_BIGENDIAN
  U16 ret;
  *(  (U8*)(&ret)      ) = *(in+1);
  *( ((U8*)(&ret)) + 1 ) = *in;
  return ret;
#else /* !WORDS_BIGENDIAN */
  return *((U16 *)in);
#endif
}


U32
fil_sreadU32 (U8 * in)
{
#ifdef WORDS_BIGENDIAN
  U32 ret;
  *( ((U8*)(&ret)) + 3 ) = *in;
  *( ((U8*)(&ret)) + 2 ) = *(in+1);
  *( ((U8*)(&ret)) + 1 ) = *(in+2);
  *(  (U8*)(&ret)      ) = *(in+3);
  return ret;
#else /* !WORDS_BIGENDIAN */
  return *((U32 *)in);
#endif
}


F64
fil_sreadF64 (U8 * in)
{
#ifdef WORDS_BIGENDIAN
  F64 ret;
  *( ((U8*)(&ret)) + 7 ) = *in;
  *( ((U8*)(&ret)) + 6 ) = *(in+1);
  *( ((U8*)(&ret)) + 5 ) = *(in+2);
  *( ((U8*)(&ret)) + 4 ) = *(in+3);
  *( ((U8*)(&ret)) + 3 ) = *(in+4);
  *( ((U8*)(&ret)) + 2 ) = *(in+5);
  *( ((U8*)(&ret)) + 1 ) = *(in+6);
  *(  (U8*)(&ret)      ) = *(in+7);
  return ret;
#else /* !WORDS_BIGENDIAN */
  return *((F64 *)in);
#endif
}


void
fil_swriteU16 (U8 * dest, U16 * src)
{
#ifdef WORDS_BIGENDIAN
  *(dest) = *(((U8*)src)+1);
  *(dest+1) = *((U8*)src);
#else /* !WORDS_BIGENDIAN */
  *(dest) = *((U8*)src);
  *(dest+1) = *(((U8*)src)+1);
#endif
}


void
fil_swriteU32 (U8 * dest, U32 * src)
{
#ifdef WORDS_BIGENDIAN
  fil_swriteU16 (dest, ( U16* )( ((U8*)src)+2 ) );
  fil_swriteU16 (dest+2, (U16*)src);
#else /* !WORDS_BIGENDIAN */
  fil_swriteU16 (dest, (U16*)src);
  fil_swriteU16 (dest+2, ( U16* )( ((U8*)src)+2 )  );
#endif
}


/*-*
	@func (void) __cole_dump dump the content of memory
	@param (void *) m memory position which content will be dumped
	@param (void *) start memory position from where calculate
				offset
	@param (int) length size in bytes of the dumped memory
	@param (char *) msg optional message, can be NULL
 *-*/
void
__cole_dump (void *_m, void *_start, int length, char *msg)
{
	unsigned char * pm;
	char buff[18];
	long achar;
	unsigned char * m;
	unsigned char * start;

	if (_m == NULL) {
		printf ("VERBOSE: can't dump because m is NULL\n");
		return;
	}
	if (_start == NULL) {
		printf ("VERBOSE: can't dump because start is NULL\n");
		return;
	}

	m = (unsigned char *)_m;
	start = (unsigned char *)_start;
	buff[8] = '-';
	buff[17] = 0;
	if (msg != NULL)
		printf ("VERBOSE: %s (from 0x%08x length 0x%08x (%d)):\n",
			msg, m - start, length, length);
	for (pm = m; pm - m < length; pm++) {
		achar = (pm - m) % 16;
		/* print offset */
		if (achar == 0)
			printf ("%08x  ", (pm - m) + (m - start));
		/* write char in the right column buffer */
		buff[achar + (achar < 8 ? 0 : 1)] = (char)((isprint (*pm) ? *pm : '.'));
		/* print next char */
		if (! ((pm - m + 1) % 16))
			/* print right column */
			printf ("%02x  %s\n", *pm, buff);
		else if (! ((pm - m + 1) % 8))
			printf ("%02x-", *pm);
		else
			printf ("%02x ", *pm);
	}
	achar = (pm - m) % 16;
	if (achar) {
		int i;
		for (i = 0; i < (16 - achar) * 3 - 1; i++)
			printf (" ");
		if (achar != 8)
			buff [achar] = 0;
		printf ("  %s\n", buff);
	}
}

