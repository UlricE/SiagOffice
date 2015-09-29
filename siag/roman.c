/*
 * Roman v2.0 - roman numeral <-> decimal converter.
 * Copyright (c) 1998 Jean-Pierre Demailly
 * Copying:  GNU GPL v2.0 or at your choice a later version.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, 
 * MA  02111-1307, USA.
 * 
 * 
 * *******************************************************
 * *******************************************************
 * Compile:  gcc -o roman -Wall -pedantic -ansi -O roman.c
 * 
 * Option -a stands for additive mode,  e.g. 4=IIII, 9=VIIII
 * Option -s stands for subtractive mode, e.g. 4=IV, 9=IX
 *
 * Notes:  for roman numerals like V^{bar} meaning 5000 the following 
 *         key is used:
 * 
 * I = 1      \
 * V = 5      |
 * X = 10     |
 * L = 50     |-- normal symbols
 * C = 100    |
 * D = 500    |
 * M = 1000   /
 * V^ = 5000      (should be V^bar)
 * X^ = 10000     (should be X^bar)
 * L^ = 50000     (should be L^bar)
 * C^ = 100000    (should be C^bar)
 * D^ = 500000    (should be D^bar)
 * M^ = 1000000   (should be M^bar)
 * V^^ = 5000000  (should be V^bar^bar)
 * etc...
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define VERSION "2.0"

/*
 * prototypes
 */
static void to_rom(char *d, char *r);
static void to_dec(char *r, char *d);
int do_roman(char *from, char *to);

/*
 * globals
 */
static int pr_add=0;
static char roman[20][6]={"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
              "", "I", "II", "III", "IIII", "V", "VI", "VII", "VIII", "VIIII"};
static char rom_symb[8]="IVXLCDM";

#ifndef SIAG
static void usage( char *argv);
int main(int argc, char **argv)
{
char c;
int i;
char string[1024];

   for (i=1 ; i<argc ; i++) 
     {

       c=argv[i][0];

       if(c=='-')
         {
           if (strlen(argv[i])==1) c='h'; else c=argv[i][1];
           if (c=='a') pr_add=10; else
           if (c=='s') pr_add=0; else usage( *argv );
           c='-';
	 }

       if (do_roman(argv[i], string))
           printf("%s\n", string);
     }

   return(0);
}

/********  functions **************/

static void usage( char *argv )
{
   printf("\nRoman v%s.  Copyright (c) 1998 Simon Kittle\n", VERSION);
   printf("Copying policy: GNU GPL v2.0 or later.  Compiled at " __TIME__" on " __DATE__ "\n\n");
   printf( "Usage: %s <roman number>              (to be converted to decimal number)\n", argv );
   printf( "       %s [-a] [-s] <decimal number>  (to be converted to roman number)\n", argv );
   printf( "\nOption -a stands for additive mode,  e.g. 4 = IIII, 9 = VIIII\n");
   printf( "Option -s stands for subtractive mode, e.g. 4 = IV, 9 = IX\n");

   printf( "\nOnly positive integers are allowed.\n");
   printf( "Roman takes any number of arguments in any order.\n");

   printf( "\nEncoding of roman symbols:\n\n");
   printf("I = 1      \\\nV = 5       |\nX = 10      |\nL = 50      |-- normal symbols\nC = 100     |\nD = 500     |\nM = 1000   /\nV^ = 5000      (should be V^bar)\nX^ = 10000     (should be X^bar) \nL^ = 50000     (should be L^bar)\nC^ = 100000    (should be C^bar)\nD^ = 500000    (should be D^bar)\nM^ = 1000000   (should be M^bar)\nV^^ = 5000000  (should be V^bar^bar)\netc...\n\n");
}
#endif

static void init_roman(void)
{
   int c, i, j;
   static int done = 0;

   if (done) return;
   for (i=0 ; i<=19 ; i++)
     {
       for (j=0 ; j<strlen(roman[i]) ; j++)
	 {
           c=roman[i][j];
           if (c=='I') roman[i][j]=1;
           if (c=='V') roman[i][j]=2;
           if (c=='X') roman[i][j]=3;
	 }
     }
}

/*
 * decimal TO roman function
 */
void to_rom(char *d, char *r)
{
int i, j, k, l, length, digit, ind;
char rom[256];

  init_roman();

  length = strlen(d);
  
  k=0;

  for (i=0 ; i<length; i++)
    {
      digit=d[i]-48;
      if ((digit<0) || (digit>9))
        {
          strcpy(r, "<invalid>") ;
          return ;
        }
      strcpy(rom, roman[digit+pr_add]);
      for (j=0 ; j<strlen(rom); j++) 
        {
          ind=rom[j]+2*(length-i)-3;
          if (ind<=6) 
            {
              r[k]=rom_symb[ind]; ++k;
  	    }
          else
            {
              r[k]=rom_symb[(ind-1)%6+1]; ++k;
              for (l=0; l<(ind-1)/6; l++) { r[k]='^'; ++k; }
  	    }
        }
    }

  r[k]=0;
}

/******************************************************/
/*
 * roman TO decimal function
 */
void to_dec(char *r, char *d)
{
int i, j, k, length, iter=0;
char string1[1024], string2[1024];;

  init_roman();

  length=strlen(r);

  j=0;

  for (i=0 ; i<length; i++)
    {
      if ((r[i]>='a') && (r[i]<='z')) r[i]-=32;

      switch(r[i])
        {
          case 'I': { string1[j]=1; ++j; break; }
          case 'V': { string1[j]=2; ++j; break; }
          case 'X': { string1[j]=3; ++j; break; }
          case 'L': { string1[j]=4; ++j; break; }
          case 'C': { string1[j]=5; ++j; break; }
          case 'D': { string1[j]=6; ++j; break; }
          case 'M': { string1[j]=7; ++j; break; }
          case '^': { string1[j-1]+=6; break; }
          default : { strcpy(d, "<invalid>"); return; break; }
	}
    }

  length=j; string1[length]=0;

  string2[0]=string1[0];
  i=1; j=1;

  while (i<length)
    {
       if ((string1[i]==string1[i-1]+1) && (string1[i]%2==0))
	 {
           --j;
 	   for (k=0; k<4; k++) { string2[j]=string1[i-1] ; j++ ; } ;
           ++i ;
	 }
       else
       if ((string1[i]==string1[i-1]+2) && (string1[i]%2==1))
	 {
           string2[j-1]=string1[i]-1;
 	   for (k=0; k<4; k++) { string2[j]=string1[i-1] ; j++ ; } ;
           ++i ;
	 }
       else
	 {
           string2[j]=string1[i];
           ++i; ++j;
	 }
    }

  length=j;
  string2[length]=0;

  for (i=1; i<length; i++)
    {
      k=string2[i]-string2[i-1];
      if (k>0)
        { strcpy(d, "<invalid>"); return; }
      if (k<0) iter=0;
      if (k==0) ++iter;
      if (iter>=4)
        { strcpy(d, "<invalid>"); return; }
    }

  k=1+(string2[0]-1)/2;

  for (i=0; i<k; i++) d[i]='0'; 
  d[k]=0;

  for (i=0; i<length; i++)
    {
      j=k-(string2[i]+1)/2;
      if (string2[i]%2) d[j]+=1; else d[j]+=5;
      if (d[j]>'9') 
        { strcpy(d, "<invalid>"); return; }
    }

}


int do_roman(char *from, char *to)
{
	int c = *from;

	if (isdigit(c)) {  
		to_rom( from, to );
	} else if (isalpha(c)) {
		to_dec( from, to );
	} else return 0;
	return 1;
}
