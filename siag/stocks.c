/*
   Siag, Scheme In A Grid
   Copyright (C) 2000-2003  Ulric Eriksson <ulric@siag.nu>

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

/*
stocks.c

Inspired by Eric Laeuffer's sample libstocks client.

Call through a cache. Otherwise we get the extra latency of a
long-distance http request every time the sheet is recalculated,
which is by default every time anything is changed. If we remember
when the last call was made and only ask again after a certain
time has passed, we will get better performance and generally more
civilized behaviour. A reasonable time between updates is 15 minutes,
but it should be possible to change that.
 
Make a simple SIOD interface such as:
 
(stock_price symbol)
(stock_yesterday symbol)
(stock_open symbol)
(stock_min symbol)
(stock_max symbol)
(stock_var symbol)
(stock_percent symbol)
(stock_volume symbol)
 
There are other fields in the stockstruct record that might be
interesting.
 
For example, (stock_price "TLIA.ST") returns the current price of
Telia on the Stockholm stock exchange. The function is also
available from the C interpreter as stock_price("TLIA.ST").
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <sys/wait.h>

#include "../stocks/stocks.h"

#include "../common/common.h"
#include <Mowitz/MwUtils.h>

#include "../siod/siod.h"

#include "calc.h"

/* this is simply a linked list. It will be fast enough. */
typedef struct quote_cache {
	stock quote;
	char *sym;
	time_t time;	/* when quote was fetched */
	struct quote_cache *next;
} quote_cache;

static quote_cache *cache = NULL;

static char *cachefn;
static stock *stocks_quotes = NULL;
static stock *stocks_tmp;
static stock error_quote;
static libstocks_return_code stocks_error;

static int lookup_quote(char *sym, stock *quote)
{
	quote_cache *q = cache;
	time_t now = time(NULL);

	for (q = cache; q; q = q->next) {
		if (!strcmp(q->sym, sym)) break;
	}

	if (q && (now-q->time) < 300) {		/* 5 minutes */
		*quote = q->quote;
		return 0;
	}

	if (q == NULL) {			/* new entry */
		q = MwMalloc(sizeof *q);
		q->next = cache;
		q->sym = MwStrdup(sym);
		cache = q;
	}

	q->time = now;

	stocks_error = get_stocks(sym, &stocks_quotes);
	if (stocks_error) {
		q->quote = error_quote;
		strcpy(q->quote.Name, sym);
		*quote = q->quote;
		return 1;
	}

	*quote = q->quote = *stocks_quotes;
	free_stocks(stocks_quotes);
	return 0;
}

static LISP lstock_test(LISP arg)
{
	stocks_error = get_stocks(get_c_string(arg), &stocks_quotes);
	if (stocks_error) {
		fprintf(stderr, "Error in getting stocks (%d)\n", stocks_error);
		return NIL;
	}
	stocks_tmp = stocks_quotes;

	while (stocks_tmp) {
		fprintf(stderr, "%s / %s\n", stocks_tmp->Time, stocks_tmp->Date);
		fprintf(stderr, "Symbol: %s\n", stocks_tmp->Symbol);
		fprintf(stderr, "Price: %f\n", stocks_tmp->CurrentPrice);
		fprintf(stderr, "Yesterday: %f\n", stocks_tmp->LastPrice);
		fprintf(stderr, "Open: %f\n", stocks_tmp->OpenPrice);
		fprintf(stderr, "Min: %f\n", stocks_tmp->MinPrice);
		fprintf(stderr, "Max: %f\n", stocks_tmp->MaxPrice);
		fprintf(stderr, "Variation: %f\n", stocks_tmp->Variation);
		fprintf(stderr, "Percentage: %f\n", stocks_tmp->Pourcentage);
		fprintf(stderr, "Volume: %d\n", stocks_tmp->Volume);

		stocks_tmp = next_stock(stocks_tmp);
	}

	free_stocks(stocks_quotes);
	return NIL;
}

/*
;@stock_name(symbol)
;@;@Fetches the name of a symbol.
;@stock_name("MSFT") returns "MICROSOFT CP".
;@stock_price
*/
static LISP lstock_name(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return strcons(strlen(quote.Name), quote.Name);
}

/*
;@stock_price(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_price("ABB.ST") returns the current price of ABB on the
;Stockholm stock exchange.
;@stock_yesterday, stock_open, stock_min, stock_max, stock_var, stock_percent, stock_volume
*/
static LISP lstock_price(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.CurrentPrice);
}

/*
;@stock_yesterday(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_yesterday("ABB.ST")
;@stock_price, stock_open, stock_min, stock_max, stock_var, stock_percent, stock_volume
*/
static LISP lstock_yesterday(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.LastPrice);
}

/*
;@stock_open(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_open("ABB.ST")
;@stock_yesterday, stock_price, stock_min, stock_max, stock_var, stock_percent, stock_volume
*/
static LISP lstock_open(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.OpenPrice);
}

/*
;@stock_min(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_min("ABB.ST")
;@stock_yesterday, stock_open, stock_price, stock_max, stock_var, stock_percent, stock_volume
*/
static LISP lstock_min(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.MinPrice);
}

/*
;@stock_max(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_max("ABB.ST")
;@stock_yesterday, stock_open, stock_min, stock_price, stock_var, stock_percent, stock_volume
*/
static LISP lstock_max(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.MaxPrice);
}

/*
;@stock_var(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_var("ABB.ST")
;@stock_yesterday, stock_open, stock_min, stock_max, stock_price, stock_percent, stock_volume
*/
static LISP lstock_var(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.Variation);
}

/*
;@stock_percent(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_percent("ABB.ST")
;@stock_yesterday, stock_open, stock_min, stock_max, stock_var, stock_price, stock_volume
*/
static LISP lstock_percent(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.Pourcentage);
}

/*
;@stock_volume(symbol)
;@Fetches stock information from Yahoo over the Internet.
;@stock_price("ABB.ST")
;@stock_yesterday, stock_open, stock_min, stock_max, stock_var, stock_percent, stock_price
*/
static LISP lstock_volume(LISP sym)
{
	stock quote;

	if (lookup_quote(get_c_string(sym), &quote)) return NIL;
	return flocons(quote.Volume);
}

/* ---
*/
void init_stocks(void)
{
	char *p = getenv("HOME");
	char b[1024];
	char *proxy;

	if (!p) p = "/tmp";
	sprintf(b, "%s/.siag/stocks_cache", p);
	cachefn = MwStrdup(b);

	proxy = getenv("http_proxy");
	if (proxy) {
		stocks_error = set_proxy(proxy);
		if (stocks_error) {
			fprintf(stderr, "Proxy error (%d)\n", stocks_error);
			return;
		}
	}
	error_quote.CurrentPrice = 0;
	error_quote.LastPrice = 0;
	error_quote.OpenPrice = 0;
	error_quote.MinPrice = 0;
	error_quote.MaxPrice = 0;
	error_quote.Variation = 0;
	error_quote.Pourcentage = 0;
	error_quote.Volume = 0;
	error_quote.Name = MwStrdup("ERROR");
	init_subr_1("stock_test", lstock_test);
	init_subr_1("stock_name", lstock_name);
	init_subr_1("stock_price", lstock_price);
	init_subr_1("stock_yesterday", lstock_yesterday);
	init_subr_1("stock_open", lstock_open);
	init_subr_1("stock_min", lstock_min);
	init_subr_1("stock_max", lstock_max);
	init_subr_1("stock_var", lstock_var);
	init_subr_1("stock_percent", lstock_percent);
	init_subr_1("stock_volume", lstock_volume);
}

