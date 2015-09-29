/*
   Siag, Scheme In A Grid
   Copyright (C) 2000-2002  Ulric Eriksson <ulric@siag.nu>
 
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.
 */
 
#include "../config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
 
#ifdef HAVE_LIBSDB
#include <sdb.h>
#endif

#include "../common/common.h"
#include "../siod/siod.h"
#include "calc.h"
#include <Mowitz/MwUtils.h>

struct db_info {
	buffer *buf;
	int sht, row, col;
	LISP res;
};

static int db_callback(int n, char **p, void *closure)
{
	cval val;
	int i, t;
	char *e;
	struct db_info *dbi = (struct db_info *)closure;

	if (!n) return 0;
	for (i = 0; i < n; i++) {
		val.number = strtod(p[i], &e);
		if (*e == '\0') t = MNUMBER;
		else t = MTEXT;
		if (dbi->res == NIL) {
			if (t == MTEXT) {
				dbi->res = strcons(strlen(p[i]), p[i]);
			} else {
				dbi->res = flocons(val.number);
			}
		} else {
			ins_data(dbi->buf, siod_interpreter,
				p[i], val, t,
				dbi->sht, dbi->row, dbi->col+i);
		}
	}
	dbi->row += 1;
	return 0;
}

static int error(char *msg)
{
        perror(msg);
        return -1;
}

static int readdata(int fd, char *b, int n)
{
        int i, j;
        for (i = 0; i < n; i += j) {
                j = read(fd, b+i, n-i);
                if (j < 0) return error("Error reading data");
        }
	return 0;
}

static int writedata(int fd, char *b, int n)
{
        int i, j;
        for (i = 0; i < n; i += j) {
                j = write(fd, b+i, n-i);
                if (j < 0) return error("Error writing data");
        }
	return 0;
}

static int writestring(int fd, char *b)
{
        return writedata(fd, b, strlen(b));
}

/* read a single char */
static int readchar(int fd)
{
        char b[1];
        if (readdata(fd, b, 1) < 0) return -1;
        return b[0];
}

/* read a number terminated by non-digit */
static int readno(int fd)
{
        int c, no = 0;

        for (;;) {
                c = readchar(fd);
		if (c < 0) return -1;
                if (!isdigit(c)) break;
                no = 10*no + c-'0';
        }
        return no;
}

/*
Url looks like 'sdbd:host:port:url=target url'
Example: sdbd:localhost:2222:url=postgres:host=localhost:db=testdb
*/
static int proxied_query(char *url, char *query,
		int (*cb)(int, char **, void *), void *closure)
{
	int i, sockfd, port, ncol, colsize;
	struct sockaddr_in serv_addr;
	struct hostent *server;
	char b[4096], host[1024];
	char *p;
	char **coldata;

	p = strstr(url, ":url=");
	if (p == NULL) {
		fprintf(stderr, "No url\n");
		return -1;
	}
	strncpy(host, url, p-url);
	host[p-url] = '\0';
	url = p+5;
	p = strchr(host, ':');
	if (p == NULL) {
		fprintf(stderr, "No url\n");
		return -1;
	}
	strncpy(host, url, p-url);
	host[p-url] = '\0';
	url = p+5;
	p = strchr(host, ':');
	if (p == NULL) {
		fprintf(stderr, "No port\n");
		return -1;
	}
	*p = '\0';
	port = atoi(p+1);

	sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (sockfd < 0) return error("Error opening socket");
	server = gethostbyname(host);
	if (server == NULL) {
		fprintf(stderr, "Error, no such host '%s'\n", host);
		return -1;
	}
	memset((char *) &serv_addr, 0, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	memcpy((char *) &serv_addr.sin_addr.s_addr,
	      (char *) server->h_addr, server->h_length);
	serv_addr.sin_port = htons(port);
	if (connect(sockfd, (struct sockaddr *)&serv_addr,
		    sizeof(serv_addr)) < 0) {
		return error("Error connecting");
	}
	writestring(sockfd, url);
	readchar(sockfd);
	writestring(sockfd, query);
	while ((ncol = readno(sockfd)) > 0) {
		coldata = MwMalloc(ncol*sizeof *coldata);
		for (i = 0; i < ncol; i++) {
			colsize = readno(sockfd);
			memset(b, 0, sizeof b);
			readdata(sockfd, b, colsize);
			coldata[i] = MwStrdup(b);
		}
		(*cb)(ncol, coldata, closure);
		for (i = 0; i < ncol; i++) MwFree(coldata[i]);
		MwFree(coldata);
	}

	return 0;
}

/*
;@db_query(url, query)
;@Makes database queries. <a href="db.html">More information</a>.
;@db_query("odbc:postgresql", "select * from foo")
;@
*/
static LISP db_query(LISP url, LISP query)
{
	struct db_info dbi;
	char *u = get_c_string(url);
	char *q = get_c_string(query);
 
	if (u == NULL || q == NULL) return NIL;

	get_siod_coords(&(dbi.row), &(dbi.col), &(dbi.sht), &(dbi.buf));
	dbi.res = NIL;

	if (!strncmp(u, "sdbd:", 5)) {
		proxied_query(u+5, q, db_callback, &dbi);
		return dbi.res;
#ifdef HAVE_LIBSDB
	} else {
		sdb_query(u, q, db_callback, &dbi);
#endif
	}
	return dbi.res;
}


void init_db(void)
{
#ifdef HAVE_LIBSDB
	sdb_init();
#endif
	init_subr_2("db_query", db_query);
}

