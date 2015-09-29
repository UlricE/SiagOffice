/*
Copyright (C) 1996-2001  Ulric Eriksson <ulric@siag.nu>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the Licence, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include "MwUtils.h"

/*
A set of functions to allocate and deallocate memory with a little
record keeping and error checking.
*/

/* ---
Allocate memory and check if anything was actually allocated.
If not, print an error message or do something else
*/

static void std_alloc_fail(void)
{
        fprintf(stderr, "Memory allocation failure\n");
        exit(EXIT_FAILURE);
}

static void (*alloc_fail)(void) = std_alloc_fail;
/* Also does a few checks that the library functions should already do */

static int paranoia = 1;

typedef struct malloc_node {
        void *p;                        /* allocated memory */
        struct malloc_node *next;       /* next in chain */
} malloc_node;

static malloc_node *nodes = NULL;

void MwMallocStats(void)
{
        malloc_node *m = nodes;
        while (m) {
                fprintf(stderr, "%p ", m->p);
                m = m->next;
        }
        fprintf(stderr, "\n");
}

static void remove_node(void *p)
{
        malloc_node *m, *n;

        if (!p) return;         /* deallocating NULL is fine */

        n = nodes;
        if (!n) {
                if (paranoia == 1) return;
                fprintf(stderr, "Deallocating %p when nothing allocated\n", p);
                if (paranoia == 2) return;
                alloc_fail();
        }

        if (n->p == p) {
                nodes = n->next;
                free(n);
                return;
        }

        for (m = n->next; m; m = m->next) {
                if (m->p == p) {
                        n->next = m->next;
                        free(m);
                        return;
                }
                n = m;
        }

        if (paranoia == 1) return;
        fprintf(stderr, "Deallocating %p which was not allocated\n", p);
        if (paranoia == 2) return;
        alloc_fail();
}

static void insert_node(void *p)
{
        malloc_node *n;

        if (!p) return;         /* we don't need to remember NULL */

        n = malloc(sizeof(malloc_node));
        if (n == NULL) alloc_fail();
        n->p = p;
        n->next = nodes;
        nodes = n;
}

/* ---
Set up a new failure handler and paranoia level.

        0 = don't check at all
        1 = try to fix errors without telling
        2 = warn about errors and try to fix them
        3 = warn about errors and fail
*/

void MwMallocInit(void (*new_fail)(void), int level)
{
        if (new_fail) alloc_fail = new_fail;
        else alloc_fail = std_alloc_fail;
        paranoia = level;
}

/* ---
Check for memory leaks.
*/

void MwMallocExit(void)
{
        malloc_node *n;

        if (paranoia == 0) return;

        for (n = nodes; n; n = n->next) {
                if (paranoia >= 2) {
                        fprintf(stderr, "Didn't deallocate %p\n", n->p);
			return;
		}
                if (paranoia == 3) alloc_fail();
        }
}

/* ---
Allocate memory with a bit of record keeping and error checking.
*/

void *MwMalloc(size_t size)
{
        void *p;

        p = malloc(size);
        if (p == NULL) alloc_fail();
        if (paranoia) insert_node(p);

        /* Courtesy of Youki Kadobayashi <youki-k@is.aist-nara.ac.jp> */
        memset(p, 1, size);

        return p;
}

/* ---
Reallocate memory.
*/

void *MwRealloc(void *ptr, size_t size)
{
        void *p;

        if (paranoia) remove_node(ptr);
        p = realloc(ptr, size);
        if (p == NULL) alloc_fail();
        if (paranoia) insert_node(p);
        return p;
}

/* ---
Allocate memory and initialize it to all zero.
*/

void *MwCalloc(size_t nmemb, size_t size)
{
        void *p;

        p = calloc(nmemb, size);
        if (p == NULL) alloc_fail();
        if (paranoia) insert_node(p);
        return p;
}

/* ---
Allocate a duplicate of a string.
*/

char *MwStrdup(const char *s)
{
        char *p;

        if (s) p = malloc(strlen(s)+1);
        else p = NULL;
        if (p == NULL) alloc_fail();
        else strcpy(p, s);
        if (paranoia) insert_node(p);
        return p;
}

/* ---
Free a block of memory.
*/

void MwFree(void *ptr)
{
        if (paranoia) remove_node(ptr);
        free(ptr);
}


/* ---
Strip trailing newline, if there is one
*/

void MwChomp(char *p)
{
	if ((p = strchr(p, '\n'))) *p = '\0';
}


/* translation stuff from Siag Office */

static struct dictstruct {
        char *key, *xl;
} *dict = NULL;

static long nw = 0;

/* ---
Replace \n with real newlines and \t with real tabs.
Other escaped characters are replaced with themselves.
*/

static void unescape(char *p)
{
        char *q = p;
        int c, state = 0;
        while ((c = *p++)) {
                switch (state) {
                case 0:
                        if (c == '\\') state = 1;
                        else *q++ = c;
                        break;
                case 1:
                        if (c == 'n') *q++ = '\n';
                        else if (c == 'r') *q++ = '\r';
                        else if (c == 'b') *q++ = '\b';
                        else if (c == 't') *q++ = '\t';
                        else *q++ = c;
                        state = 0;
                        break;
                }
        }
        *q = '\0';
}

static int compar(const void *p, const void *q)
{
        struct dictstruct *p1 = (struct dictstruct *)p;
        struct dictstruct *q1 = (struct dictstruct *)q;
        return strcmp(p1->key, q1->key);
}

/* ---
Load the message string dictionary from $(SIAGHOME)/common/dictionary.$(LANG)
*/

void MwLoadDictionary(char *fn)
{
        FILE *fp;
        char b[10000], *p, *q;

        /* convert everything after language code to upper case */
        p = strchr(fn, '_');
        if (p) {
                q = p;
                while (*q) {
                        *q = toupper(*q);
                        q++;
                }
        }

        if (p) {
                q = strchr(p, '.');
        } else {
                q = NULL;
        }

        fp = fopen(fn, "r");
        
        if (!fp && q) {
                /* try abridged file name */
                *q = '\0';
                fp = fopen(fn, "r");
        }
        if (!fp && p) {
                /* try abridged file name */
                *p = '\0';
                fp = fopen(fn, "r");
        }
        if (!fp) {
#ifdef DEBUG
                fprintf(stderr, "Can't open dictionary\n");
#endif
                return;
        }
        while (fgets(b, sizeof b, fp)) {
                if (b[0] == '#') continue;
                MwChomp(b);
                p = strchr(b, '\t');
                if (!p) continue;
                *p++ = 0;
                p += strspn(p, "\t");
                dict = MwRealloc(dict, (nw+1)*(sizeof dict[0]));
                unescape(b);
                unescape(p);
                dict[nw].key = MwStrdup(b);
                dict[nw++].xl = MwStrdup(p);
        }
        qsort(dict, nw, sizeof dict[0], compar);
}

/* ---
Translate a message string. If no match is found, the key is returned.
By using English keys, an application can keep working even when the
dictionary is incomplete.
990113: tree search through sorted list
*/

char *MwTranslate(char *key)
{
        long lower = 0, upper = nw-1;
        long i;
        int d;

        while (lower <= upper) {
                i = (lower+upper)/2;
                d = strcmp(key, dict[i].key);
                if (d == 0) return dict[i].xl;
                else if (d > 0) lower = i+1;
                else upper = i-1;
        }
        return key;
}

/* translation stuff done */

/* copy from q to p, quoting any character in b */                              
void MwQuotecpy(char *p, char *q, char *b)                                        
{                                                                               
        int c;                                                                  
                                                                                
        while ((c = *q++)) {                                                    
                if (strchr(b, c)) *p++ = '\\';                                  
                *p++ = c;                                                       
        }                                                                       
        *p = '\0';                                                              
}

pid_t MwSpawn(const char *command)
{
        char *argv[20], *p, cmd[1024];
        int argc = 0;
        pid_t pid;

        strncpy(cmd, command, sizeof cmd);

        for (p = strtok(cmd, " \t\r\n");
             p && argc < 20;
             p = strtok(NULL, " \t\r\n")) {
                argv[argc++] = p;
        }
        argv[argc] = NULL;
        pid = fork();
        if (!pid) {
                /* this is the child */

                /*close(2);*/   /* don't display the child's messages */

                execvp(argv[0], argv);
                exit(0);
        }
        return pid;
}

void MwHelp(char *p)
{
	char b[1024];
	char *browser = getenv("BROWSER");

	if (browser == NULL) browser = "netscape";
	sprintf(b, "%s /usr/local/doc/Mowitz/%s", browser, p);
	MwSpawn(b);
}

static struct {
        char *name;
        int value;
} cchar[] = {
        {"quot", '"'}, {"amp", '&'}, {"lt", '<'}, {"gt", '>'},
        {"nbsp", 160}, {"iexcl", 161}, {"cent", 162}, {"pound", 163},
        {"curren", 164}, {"yen", 165}, {"brvbar", 166}, {"sect", 167},
        {"uml", 168}, {"copy", 169}, {"ordf", 170}, {"laquo", 171},
        {"not", 172}, {"shy", 173}, {"reg", 174}, {"macr", 175},
        {"deg", 176}, {"plusmn", 177}, {"sup2", 178}, {"sup3", 179},
        {"acute", 180}, {"micro", 181}, {"para", 182}, {"middot", 183},
        {"cedil", 184}, {"sup1", 185}, {"ordm", 186}, {"raquo", 187},
        {"frac14", 188}, {"frac12", 189}, {"frac34", 190}, {"iquest", 191},
        {"Agrave", 192}, {"Aacute", 193}, {"Acirc", 194}, {"Atilde", 195},
        {"Auml", 196}, {"Aring", 197}, {"AElig", 198}, {"Ccedil", 199},
        {"Egrave", 200}, {"Eacute", 201}, {"Ecirc", 202}, {"Euml", 203},
        {"Igrave", 204}, {"Iacute", 205}, {"Icirc", 206}, {"Euml", 207},
        {"ETH", 208}, {"Ntilde", 209}, {"Ograve", 210}, {"Oacute", 211},
        {"Ocirc", 212}, {"Otilde", 213}, {"Ouml", 214}, {"times", 215},
        {"Oslash", 216}, {"Ugrave", 217}, {"Uacute", 218}, {"Ucirc", 219},
        {"Uuml", 220}, {"Yacute", 221}, {"THORN", 222}, {"szlig", 223},
        {"agrave", 224}, {"aacute", 225}, {"acirc", 226}, {"atilde", 227},
        {"auml", 228}, {"aring", 229}, {"aelig", 230}, {"ccedil", 231},
        {"egrave", 232}, {"eacute", 233}, {"ecirc", 234}, {"euml", 235},
        {"igrave", 236}, {"iacute", 237}, {"icirc", 238}, {"iuml", 239},
        {"eth", 240}, {"ntilde", 241}, {"ograve", 242}, {"oacute", 243},
        {"ocirc", 244}, {"otilde", 245}, {"ouml", 246}, {"divide", 247},
        {"slash", 248}, {"ugrave", 249}, {"uacute", 250}, {"ucirc", 251},
        {"uuml", 252}, {"yacute", 253}, {"thorn", 254}, {"yuml", 255},
        {NULL, 0}
};

/* convert Auml to Ä and #33 to ! */
int MwFromCchar(char *from)
{
        int i;

        if (from[0] == '#') {
                i = atoi(from+1);
                if (i >= ' ' && i <= 255) return i;
                return -1;
        }
        for (i = 0; cchar[i].name; i++) {
                if (!strcmp(cchar[i].name, from)) return cchar[i].value;
        }
        return -1;
}

/* convert Ä to &Auml; and so on */
void MwToCchar(char *to, int from)
{
        int i;

        for (i = 0; cchar[i].value; i++) {
                if (from == cchar[i].value) {
                        sprintf(to, "&%s;", cchar[i].name);
                        return;
                }
        }
        to[0] = from;
        to[1] = '\0';
}

/* ---
strcmp which is immune to libc silliness
*/
int MwStrcmp(const char *p, const char *q)
{
        int c;

        while (!(c = *p-*q) && *p) {
                p++;
                q++;
        }
        return c;
}

/* ---
Case insensitive compare
*/

int MwStrcasecmp(const char *p, const char *q)
{
        int c;

        while (!(c = toupper(*p)-toupper(*q)) && *p) {
                p++;
                q++;
        }
        return c;
}

/* ---
Case insensitive compare, length limited
*/

int MwStrncasecmp(const char *p, const char *q, size_t n)
{
        size_t i = 0;
        int c = 0;

        while ((i < n) && !(c = toupper(*p)-toupper(*q)) && *p) {
                p++;
                q++;
                i++;
        }
        return c;
}

int MwSnprintf(char *str, size_t n, const char *format, ...)
{
        int m;
        va_list ap;
        va_start(ap, format);
#ifdef HAVE_VSNPRINTF
        m = vsnprintf(str, n, format, ap);
#else
        m = vsprintf(str, format, ap);
#endif
        va_end(ap);
        return m;
}

