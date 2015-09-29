/*
   Pathetic Writer
   Copyright (C) 1997-2002  Ulric Eriksson <ulric@siag.nu>

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

/*
 * fileio_rtf.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>

#include "pw.h"
#include "../common/common.h"
#include <Mowitz/MwUtils.h>

typedef struct s_rtf_font {
	int nr;
	char type[20];
	char name[40];
} rtf_font;

typedef struct s_rtf_style {
	int nr;		/* stylesheet */
	int li;		/* left indent */
	int sa;		/* skip after */
	int keepn;	/* keep with next */
	int bold;	/* guess */
	int font;	/* font number */
	int size;	/* foo */
	int basedon;	/* based on */
	int next;	/* next style */
	int brk;	/* paragraph break after every line */
	char name[40];	/* my name for this style */
} rtf_style;

static rtf_font fnt[] = {
	{0, "modern", "Courier"},
	{1, "swiss", "Helvetica"},
	{2, "roman", "New Century Schoolbook"},
	{3, "roman", "Times"}
};

#define RTF_NROFFONTS (sizeof fnt/sizeof *fnt)

static int rtf_fontnr(char *name)
{
	int i;

	for (i = 0; i < RTF_NROFFONTS; i++) {
		if (!MwStrcasecmp(name, fnt[i].name))
			return fnt[i].nr;
	}
	return fnt[0].nr;
}

static void fontdef(FILE *fpo, rtf_font f)
{
	fprintf(fpo, "{\\f%d\\f%s %s;}\n", f.nr, f.type, f.name);
}

static void rtf_fonts(FILE *fpo)
{
	int i;

	fprintf(fpo, "{\\fonttbl\n");
	for (i = 0; i < RTF_NROFFONTS; i++) {
		fontdef(fpo, fnt[i]);
	}
	fprintf(fpo, "}\n");
}

static void styledef(FILE *fpo, rtf_style s)
{
	fprintf(fpo, "{");
	/*if (s.nr)*/ fprintf(fpo, "\\s%d", s.nr);	/* stylesheet */
	if (s.li) fprintf(fpo, "\\li%d", s.li);	/* left indent */
	if (s.sa) fprintf(fpo, "\\sa%d", s.sa);	/* skip after */
	if (s.keepn) fprintf(fpo, "\\keepn");	/* keep with next */
	if (s.nr || s.li || s.sa || s.keepn) fprintf(fpo, " ");
	if (s.bold) fprintf(fpo, "\\b");	/* bold */
	if (s.font) fprintf(fpo, "\\f%d", s.font);	/* font number */
	/*if (s.size != 12)*/ fprintf(fpo, "\\fs%d", s.size);	/* font size */
	if (s.bold || s.font || s.size) fprintf(fpo, " ");
	if (s.nr) fprintf(fpo, "\\sbasedon%d", s.basedon);	/* based on */
	fprintf(fpo, "\\snext%d %s;}\n", s.next, s.name);	/* next style */
}

static int rtf_fontno(int fmt)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	return rtf_fontnr(ft.family);
}

static int rtf_fontsize(int fmt)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	return ft.size*2/10;
}

static int rtf_isbold(int fmt)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	return ft.bold;
}

static int rtf_isitalic(int fmt)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	return ft.italic;
}

static int rtf_isulined(int fmt)
{
	MwFmt ft;
	MwDecodeFormat(fmt, ~0, &ft);
	return ft.uline;
}

static rtf_style *rtf_makestyle(rtf_style *s, int i)
{
	static rtf_style p;
	int fmt = style_table[i].format;
	if (!s) s = &p;
	s->nr = i; 
	s->li = 0;
	s->sa = 0;
	s->keepn = FALSE;
	s->bold = rtf_isbold(fmt);
	s->font = rtf_fontno(fmt);
	s->size = rtf_fontsize(fmt);
	s->basedon = MW_STY_DEFAULT;
	s->next = style_table[i].follower;
	s->brk = (i == MW_STY_PREFORMAT ? 1 : 0);
	strcpy(s->name, style_table[i].name);
	return s;
}

static void rtf_styles(FILE *fpo)
{
	int i;

	fprintf(fpo, "{\\stylesheet\n");
	for (i = MW_STY_DEFAULT; i < MW_STY_EMBED; i++) {
		rtf_style s;
		rtf_makestyle(&s, i);
		styledef(fpo, s);
	}
	fprintf(fpo, "}\n");
}

static rtf_style getstyle(int nr)
{
	int i;
	static rtf_style s;

	for (i = MW_STY_DEFAULT; i < MW_STY_EMBED; i++) {
		rtf_makestyle(&s, i);
		if (s.nr == nr) return s;
	}

	rtf_makestyle(&s, MW_STY_DEFAULT);
	return s;
}

static void preblurb(FILE *fpo)
{
	/* default font Times; default language Swedish */
	fprintf(fpo, "{\\rtf1\\ansi \\deff3\\deflang1053\n");

	rtf_fonts(fpo);
	rtf_styles(fpo);

	/* paper size */
	fprintf(fpo, "\\paperw%d\\paperh%d\n",
			20*paper_width, 20*paper_height);

	/* marginals */
	fprintf(fpo, "\\margl%d\\margr%d\\margt%d\\margb%d\n",
			20*left_margin, 20*right_margin,
			20*top_margin, 20*bottom_margin);
	fprintf(fpo, "\\gutter0 \\deftab%d\\widowctrl\\ftnbj\\hyphhotz425 \n",
			20*36);
	fprintf(fpo, "\\sectd \n");
	fprintf(fpo, "\\binfsxn1\\binsxn1\\linex0\\endnhere \n");
}

static void postblurb(FILE *fpo)
{
	fprintf(fpo, "}\n");
}

static int last_sty;
static /*siag_format*/int last_fmt;

static void save_style(FILE *fp, int sty)
{
	rtf_style s = getstyle(sty);

	if (last_sty != -1) {
		fprintf(fp, "\\par\n");
	}

	/* paragraph definition */
	fprintf(fp, "\\pard\\plain \n");

	fprintf(fp, "\\s%d", s.nr);
	if (s.li) fprintf(fp, "\\li%d", s.li);
	if (s.sa) fprintf(fp, "\\sa%d", s.sa);
	if (s.keepn) fprintf(fp, "\\keepn");
	fprintf(fp, " ");

	/* save_fmt takes care of character formats */

	fprintf(fp, "\n");

	last_sty = sty;
	last_fmt = 0;
}

static void save_fmt(FILE *fp, int fmt)
{
	fprintf(fp, "\\f%d\\fs%d", rtf_fontno(fmt), rtf_fontsize(fmt));
	if (rtf_isbold(fmt)) fprintf(fp, "\\b");
	else fprintf(fp, "\\b0");
	if (rtf_isitalic(fmt)) fprintf(fp, "\\i");
	else fprintf(fp, "\\i0");
	if (rtf_isulined(fmt)) fprintf(fp, "\\ul");
	else fprintf(fp, "\\ul0");
	fprintf(fp, " ");
	last_fmt = fmt;
}

static int save_char(FILE *fp, int fmt, int c)
{
	if (fmt != last_fmt)
		save_fmt(fp, fmt);

	switch (c) {
	case '\\':
		fprintf(fp, "\\\\");
		break;
	case '{':
		fprintf(fp, "\\{");
		break;
	case '}':
		fprintf(fp, "\\}");
		break;
	default:
		if (c > 127) fprintf(fp, "\\\'%02x", c);
		else putc(c, fp);
	}
	return 0;
}

/* ---
start new paragraph for each of these cases (heuristic alert):
    - new style
    - sty is preformat
    - empty line
    - line starts with white space (suggests indentation)

Changed by Ulric 990522: New paragraph if bop.
*/

static int save_line(FILE *fp, int sty, int bop, MwRichchar *p)
{
	int i;
	if (bop) {
		save_style(fp, sty);
	} else {
		save_char(fp, last_fmt, ' ');
	}
	/* This isn't really enough. We also need to handle adjustment,
	   line height and such. Get back to this... */
	for (i = 0; i < MwRcStrlen(p); i++) {
		if (save_char(fp, /*format_table[*/p[i].fmt/*]*/, p[i].c))
			return 1;
	}
	save_char(fp, last_fmt, '\n');	/* separate lines */
	return 0;
}

/* ---
Returns: 0 if successful, otherwise 1
*/

static int save(char *fn, buffer *buf)
{
	FILE *fp;
	int i;
	int s = 0;

	if ((fp = fopen(fn, "w")) == NULL) return 1;

	/* do the header */
	preblurb(fp);

	last_sty = -1;	/* force new definitions */

	for (i = 1; i <= line_last_used(buf, s); i++) {
		if (save_line(fp, ret_style(buf, s, i),
				ret_bop(buf, s, i),
				buf->sht[s].text[i].p)) {
			fclose(fp);
			return 1;
		}
	}
	fprintf(fp, "\\par\n");		/* just in case */

	postblurb(fp);

	fclose(fp);
	return 0;
}


/* loader code */

static int row;
static buffer *buf;

/* ---
This code was kindly donated by Microsoft as part of the RTF 1.5
   specification. Rewritten in C and adapted for Pathetic Writer.
--- */

typedef char bool;
#define fTrue 1
#define fFalse 0

typedef struct char_prop {
    char fBold;
    char fUnderline;
    char fItalic;
    int fSize;
    int fNo;
} CHP;                  /* CHaracter Properties */

typedef enum {justL, justR, justC, justF } JUST;
typedef struct para_prop {
    int xaLeft;                 /* left indent in twips */
    int xaRight;                /* right indent in twips */
    int xaFirst;                /* first line indent in twips */
    JUST just;                  /* justification */
} PAP;                  /* PAragraph Properties */

typedef enum {sbkNon, sbkCol, sbkEvn, sbkOdd, sbkPg} SBK;
typedef enum {pgDec, pgURom, pgLRom, pgULtr, pgLLtr} PGN;
typedef struct sect_prop {
    int cCols;                  /* number of columns */
    SBK sbk;                    /* section break type */
    int xaPgn;                  /* x position of page number in twips */
    int yaPgn;                  /* y position of page number in twips */
    PGN pgnFormat;              /* how the page number is formatted */
} SEP;                  /* SEction Properties */

typedef struct doc_prop {
    int xaPage;                 /* page width in twips */
    int yaPage;                 /* page height in twips */
    int xaLeft;                 /* left margin in twips */
    int yaTop;                  /* top margin in twips */
    int xaRight;                /* right margin in twips */
    int yaBottom;               /* bottom margin in twips */
    int pgnStart;               /* starting page number in twips */
    char fFacingp;              /* facing pages enabled? */
    char fLandscape;            /* landscape or portrait?? */
    int fDefFont;		/* default font number */
    char fMac;			/* hack to deal with mac charset */
} DOP;                  /* DOcument Properties */

typedef enum { rdsNorm, rdsSkip, rdsFonttbl } RDS;  /* Rtf Destination State */
typedef enum { risNorm, risBin, risHex } RIS;       /* Rtf Internal State */

typedef struct save {           /* property save structure */
    struct save *pNext;         /* next save */
    CHP chp;
    PAP pap;
    SEP sep;
    DOP dop;
    RDS rds;
    RIS ris;
} SAVE;

/* What types of properties are there? */
typedef enum {ipropBold, ipropItalic, ipropUnderline,
	      ipropFontSize, ipropFontNo,
	      ipropDefFont,
	      ipropLeftInd,
              ipropRightInd, ipropFirstInd, ipropCols, ipropPgnX,
              ipropPgnY, ipropXaPage, ipropYaPage, ipropXaLeft,
              ipropXaRight, ipropYaTop, ipropYaBottom, ipropPgnStart,
              ipropSbk, ipropPgnFormat, ipropFacingp, ipropLandscape,
	      ipropMac,
              ipropJust, ipropPard, ipropPlain, ipropSectd,
              ipropMax } IPROP;

typedef enum {actnSpec, actnByte, actnWord} ACTN;
typedef enum {propChp, propPap, propSep, propDop} PROPTYPE;

typedef struct propmod {
    ACTN actn;              /* size of value */
    PROPTYPE prop;          /* structure containing value */
    int  offset;            /* offset of value from base of structure */
} PROP;

typedef enum {ipfnBin, ipfnHex, ipfnSkipDest } IPFN;
typedef enum {idestPict, idestSkip, idestFonttbl } IDEST;
typedef enum {kwdChar, kwdDest, kwdProp, kwdSpec} KWD;

typedef struct symbol {
    char *szKeyword;        /* RTF keyword */
    int  dflt;              /* default value to use */
    bool fPassDflt;         /* true to use default value from this table */
    KWD  kwd;               /* base action to take */
    int  idx;               /* index into property table if kwd == kwdProp */
                            /* index into destination table if kwd == kwdDest */
                            /* character to print if kwd == kwdChar */
} SYM;

/* RTF parser declarations */

static int ecRtfParse(FILE *fp);
static int ecPushRtfState(void);
static int ecPopRtfState(void);
static int ecParseRtfKeyword(FILE *fp);
static int ecParseChar(int c);
static int ecTranslateKeyword(char *szKeyword, int param, bool fParam);
static int ecPrintChar(int ch);
static int ecEndGroupAction(RDS rds);
static int ecApplyPropChange(IPROP iprop, int val);
static int ecChangeDest(IDEST idest);
static int ecParseSpecialKeyword(IPFN ipfn);
static int ecParseSpecialProperty(IPROP iprop, int val);
/*static int ecParseHexByte(void);
*/

/* RTF variable declarations */

static int cGroup;
static RDS rds;
static RIS ris;

static CHP chp;
static PAP pap;
static SEP sep;
static DOP dop;

static SAVE *psave;
static long cbBin;
static long lParam;
static bool fSkipDestIfUnk;
static FILE *fpIn;

/* RTF parser error codes */

#define ecOK 0                      /* Everything's fine! */
#define ecStackUnderflow    1       /* Unmatched '}' */
#define ecStackOverflow     2       /* Too many '{' -- memory exhausted */
#define ecUnmatchedBrace    3       /* RTF ended during an open group. */
#define ecInvalidHex        4       /* invalid hex character found in data */
#define ecBadTable          5       /* RTF table (sym or prop) invalid */
#define ecAssertion         6       /* Assertion failure */
#define ecEndOfFile         7       /* End of file reached while reading RTF */

/* RTF parser tables */

/* Property descriptions */
PROP rgprop [ipropMax] = {
    {actnByte,   propChp,    offsetof(CHP, fBold)},       /* ipropBold */
    {actnByte,   propChp,    offsetof(CHP, fItalic)},     /* ipropItalic */
    {actnByte,   propChp,    offsetof(CHP, fUnderline)},  /* ipropUnderline */
    {actnWord,	 propChp,    offsetof(CHP, fSize)},	  /* ipropFontSize */
    {actnWord,   propChp,    offsetof(CHP, fNo)},	  /* ipropFontNo */
    {actnWord,	 propDop,    offsetof(DOP, fDefFont)},	  /* ipropDefFont */
    {actnWord,   propPap,    offsetof(PAP, xaLeft)},      /* ipropLeftInd */
    {actnWord,   propPap,    offsetof(PAP, xaRight)},     /* ipropRightInd */
    {actnWord,   propPap,    offsetof(PAP, xaFirst)},     /* ipropFirstInd */
    {actnWord,   propSep,    offsetof(SEP, cCols)},       /* ipropCols */
    {actnWord,   propSep,    offsetof(SEP, xaPgn)},       /* ipropPgnX */
    {actnWord,   propSep,    offsetof(SEP, yaPgn)},       /* ipropPgnY */
    {actnWord,   propDop,    offsetof(DOP, xaPage)},      /* ipropXaPage */
    {actnWord,   propDop,    offsetof(DOP, yaPage)},      /* ipropYaPage */
    {actnWord,   propDop,    offsetof(DOP, xaLeft)},      /* ipropXaLeft */
    {actnWord,   propDop,    offsetof(DOP, xaRight)},     /* ipropXaRight */
    {actnWord,   propDop,    offsetof(DOP, yaTop)},       /* ipropYaTop */
    {actnWord,   propDop,    offsetof(DOP, yaBottom)},    /* ipropYaBottom */
    {actnWord,   propDop,    offsetof(DOP, pgnStart)},    /* ipropPgnStart */
    {actnByte,   propSep,    offsetof(SEP, sbk)},         /* ipropSbk */
    {actnByte,   propSep,    offsetof(SEP, pgnFormat)},   /* ipropPgnFormat */
    {actnByte,   propDop,    offsetof(DOP, fFacingp)},    /* ipropFacingp */
    {actnByte,   propDop,    offsetof(DOP, fLandscape)},  /* ipropLandscape */
    {actnByte,	 propDop,    offsetof(DOP, fMac)},	  /* ipropMac */
    {actnByte,   propPap,    offsetof(PAP, just)},        /* ipropJust */
    {actnSpec,   propPap,    0},                          /* ipropPard */
    {actnSpec,   propChp,    0},                          /* ipropPlain */
    {actnSpec,   propSep,    0},                          /* ipropSectd */
};

/* Keyword descriptions */
SYM rgsymRtf[] = {
/*  keyword     dflt    fPassDflt   kwd         idx */
    {"b",        1,      fFalse,     kwdProp,    ipropBold},
    {"ul",       1,      fFalse,     kwdProp,    ipropUnderline},
    {"i",        1,      fFalse,     kwdProp,    ipropItalic},
    {"fs",	 24,	 fFalse,     kwdProp,	 ipropFontSize},
    {"f",	 0,	 fFalse,     kwdProp,	 ipropFontNo},
    {"deff",	 0,	 fFalse,     kwdProp,	 ipropDefFont},
    {"li",       0,      fFalse,     kwdProp,    ipropLeftInd},
    {"ri",       0,      fFalse,     kwdProp,    ipropRightInd},
    {"fi",       0,      fFalse,     kwdProp,    ipropFirstInd},
    {"cols",     1,      fFalse,     kwdProp,    ipropCols},
    {"sbknone",  sbkNon, fTrue,      kwdProp,    ipropSbk},
    {"sbkcol",   sbkCol, fTrue,      kwdProp,    ipropSbk},
    {"sbkeven",  sbkEvn, fTrue,      kwdProp,    ipropSbk},
    {"sbkodd",   sbkOdd, fTrue,      kwdProp,    ipropSbk},
    {"sbkpage",  sbkPg,  fTrue,      kwdProp,    ipropSbk},
    {"pgnx",     0,      fFalse,     kwdProp,    ipropPgnX},
    {"pgny",     0,      fFalse,     kwdProp,    ipropPgnY},
    {"pgndec",   pgDec,  fTrue,      kwdProp,    ipropPgnFormat},
    {"pgnucrm",  pgURom, fTrue,      kwdProp,    ipropPgnFormat},
    {"pgnlcrm",  pgLRom, fTrue,      kwdProp,    ipropPgnFormat},
    {"pgnucltr", pgULtr, fTrue,      kwdProp,    ipropPgnFormat},
    {"pgnlcltr", pgLLtr, fTrue,      kwdProp,    ipropPgnFormat},
    {"qc",       justC,  fTrue,      kwdProp,    ipropJust},
    {"ql",       justL,  fTrue,      kwdProp,    ipropJust},
    {"qr",       justR,  fTrue,      kwdProp,    ipropJust},
    {"qj",       justF,  fTrue,      kwdProp,    ipropJust},
    {"paperw",   12240,  fFalse,     kwdProp,    ipropXaPage},
    {"paperh",   15480,  fFalse,     kwdProp,    ipropYaPage},
    {"margl",    1800,   fFalse,     kwdProp,    ipropXaLeft},
    {"margr",    1800,   fFalse,     kwdProp,    ipropXaRight},
    {"margt",    1440,   fFalse,     kwdProp,    ipropYaTop},
    {"margb",    1440,   fFalse,     kwdProp,    ipropYaBottom},
    {"pgnstart", 1,      fTrue,      kwdProp,    ipropPgnStart},
    {"facingp",  1,      fTrue,      kwdProp,    ipropFacingp},
    {"landscape",1,      fTrue,      kwdProp,    ipropLandscape},
    {"mac",	 1,	 fFalse,     kwdProp,	 ipropMac},
    {"par",      0,      fFalse,     kwdChar,    '\n'},
    {"\n",       0,      fFalse,     kwdChar,    '\n'},
    {"\r",       0,      fFalse,     kwdChar,    '\n'},
    {"tab",      0,      fFalse,     kwdChar,    '\t'},
    {"ldblquote",0,      fFalse,     kwdChar,    '"'},
    {"rdblquote",0,      fFalse,     kwdChar,    '"'},
    {"lquote",	 0,	 fFalse,     kwdChar,	 '`'},
    {"rquote",	 0,	 fFalse,     kwdChar,	 '´'},
    {"bin",      0,      fFalse,     kwdSpec,    ipfnBin},
    {"*",        0,      fFalse,     kwdSpec,    ipfnSkipDest},
    {"'",        0,      fFalse,     kwdSpec,    ipfnHex},
    {"author",   0,      fFalse,     kwdDest,    idestSkip},
    {"buptim",   0,      fFalse,     kwdDest,    idestSkip},
    {"colortbl", 0,      fFalse,     kwdDest,    idestSkip},
    {"comment",  0,      fFalse,     kwdDest,    idestSkip},
    {"creatim",  0,      fFalse,     kwdDest,    idestSkip},
    {"doccomm",  0,      fFalse,     kwdDest,    idestSkip},
    {"fonttbl",  0,      fFalse,     kwdDest,    idestFonttbl},
    {"footer",   0,      fFalse,     kwdDest,    idestSkip},
    {"footerf",  0,      fFalse,     kwdDest,    idestSkip},
    {"footerl",  0,      fFalse,     kwdDest,    idestSkip},
    {"footerr",  0,      fFalse,     kwdDest,    idestSkip},
    {"footnote", 0,      fFalse,     kwdDest,    idestSkip},
    {"ftncn",    0,      fFalse,     kwdDest,    idestSkip},
    {"ftnsep",   0,      fFalse,     kwdDest,    idestSkip},
    {"ftnsepc",  0,      fFalse,     kwdDest,    idestSkip},
    {"header",   0,      fFalse,     kwdDest,    idestSkip},
    {"headerf",  0,      fFalse,     kwdDest,    idestSkip},
    {"headerl",  0,      fFalse,     kwdDest,    idestSkip},
    {"headerr",  0,      fFalse,     kwdDest,    idestSkip},
    {"info",     0,      fFalse,     kwdDest,    idestSkip},
    {"keywords", 0,      fFalse,     kwdDest,    idestSkip},
    {"operator", 0,      fFalse,     kwdDest,    idestSkip},
    {"pict",     0,      fFalse,     kwdDest,    idestSkip},
    {"printim",  0,      fFalse,     kwdDest,    idestSkip},
    {"private1", 0,      fFalse,     kwdDest,    idestSkip},
    {"revtim",   0,      fFalse,     kwdDest,    idestSkip},
    {"rxe",      0,      fFalse,     kwdDest,    idestSkip},
    {"stylesheet",  0,   fFalse,     kwdDest,    idestSkip},
    {"subject",  0,      fFalse,     kwdDest,    idestSkip},
    {"tc",       0,      fFalse,     kwdDest,    idestSkip},
    {"title",    0,      fFalse,     kwdDest,    idestSkip},
    {"txe",      0,      fFalse,     kwdDest,    idestSkip},
    {"xe",       0,      fFalse,     kwdDest,    idestSkip},
    {"{",        0,      fFalse,     kwdChar,    '{'},
    {"}",        0,      fFalse,     kwdChar,    '}'},
    {"\\",       0,      fFalse,     kwdChar,    '\\'},
    {"~",	 0,	 fFalse,     kwdChar,	 ' '},
    {"_",	 0,	 fFalse,     kwdChar,	 ' '},
    {"emspace",	 0,	 fFalse,     kwdChar,	 ' '},
    {"enspace",	 0,	 fFalse,     kwdChar,	 ' '},
    {"pard",	 0,	 fFalse,     kwdProp,	 ipropPard},
    {"plain",	 0,	 fFalse,     kwdProp,	 ipropPlain},
    {"sectd",	 0,	 fFalse,     kwdProp,	 ipropSectd},
    {"row",	 0,	 fFalse,     kwdChar,	 '\n'},
    {"cell",	 0,	 fFalse,     kwdChar,	 '\t'},
    };
static int isymMax = sizeof(rgsymRtf) / sizeof(SYM);


/* %%Function: ecApplyPropChange */

/* ---
Set the property identified by _iprop_ to the value _val_.
*/

static int ecApplyPropChange(IPROP iprop, int val)
{
    char *pb;

    if (rds == rdsSkip) {               /* If we're skipping text, */
        return ecOK;                    /* don't do anything. */
    }

    switch (rgprop[iprop].prop) {
    case propDop:
        pb = (char *)&dop;
        break;
    case propSep:
        pb = (char *)&sep;
        break;
    case propPap:
        pb = (char *)&pap;
        break;
    case propChp:
        pb = (char *)&chp;
        break;
    default:
	pb = '\0';	/* make gcc shut up */
        if (rgprop[iprop].actn != actnSpec)
            return ecBadTable;
        break;
    }
    switch (rgprop[iprop].actn) {
    case actnByte:
        pb[rgprop[iprop].offset] = (unsigned char) val;
        break;
    case actnWord:
        (*(int *) (pb+rgprop[iprop].offset)) = val;
        break;
    case actnSpec:
        return ecParseSpecialProperty(iprop, val);
        break;
    default:
        return ecBadTable;
    }
    return ecOK;
}

/* %%Function: ecParseSpecialProperty */

/* ---
Set a property that requires code to evaluate.
*/

static int ecParseSpecialProperty(IPROP iprop, int val)
{
    switch (iprop) {
    case ipropPard:
        memset(&pap, 0, sizeof(pap));
        return ecOK;
    case ipropPlain:
        memset(&chp, 0, sizeof(chp));
	chp.fSize = 24;		/* 0 is too small to see */
	chp.fNo = dop.fDefFont;	/* hope this works */
        return ecOK;
    case ipropSectd:
        memset(&sep, 0, sizeof(sep));
        return ecOK;
    default:
        return ecBadTable;
    }
    /*NOTREACHED*/
    return ecBadTable;
}

/* %%Function: ecTranslateKeyword. */

/* ---
Step 3.
 Search rgsymRtf for szKeyword and evaluate it appropriately.

 Inputs:
 szKeyword:   The RTF control to evaluate.
 param:       The parameter of the RTF control.
 fParam:      fTrue if the control had a parameter; (that is, if param is valid)
              fFalse if it did not.
*/

static int ecTranslateKeyword(char *szKeyword, int param, bool fParam)
{
    int isym;

    /* search for szKeyword in rgsymRtf */

    for (isym = 0; isym < isymMax; isym++)
        if (strcmp(szKeyword, rgsymRtf[isym].szKeyword) == 0)
            break;
    if (isym == isymMax) {           /* control word not found */
        if (fSkipDestIfUnk)         /* if this is a new destination */
            rds = rdsSkip;          /* skip the destination */
                                    /* else just discard it */
        fSkipDestIfUnk = fFalse;
        return ecOK;
    }

    /* found it!  use kwd and idx to determine what to do with it. */

    fSkipDestIfUnk = fFalse;
    switch (rgsymRtf[isym].kwd) {
    case kwdProp:
        if (rgsymRtf[isym].fPassDflt || !fParam)
            param = rgsymRtf[isym].dflt;
        return ecApplyPropChange(rgsymRtf[isym].idx, param);
    case kwdChar:
        return ecParseChar(rgsymRtf[isym].idx);
    case kwdDest:
        return ecChangeDest(rgsymRtf[isym].idx);
    case kwdSpec:
        return ecParseSpecialKeyword(rgsymRtf[isym].idx);
    default:
        return ecBadTable;
    }
    /*NOTREACHED*/
    return ecBadTable;
}

/* %%Function: ecChangeDest */

/* ---
Change to the destination specified by idest.
There's usually more to do here than this...
*/

static int ecChangeDest(IDEST idest)
{
    if (rds == rdsSkip)             /* if we're skipping text, */
        return ecOK;                /* don't do anything */

    switch (idest) {
    case idestFonttbl:
	rds = rdsFonttbl;
	break;
    default:
        rds = rdsSkip;              /* when in doubt, skip it... */
        break;
    }
    return ecOK;
}

/* %%Function: ecEndGroupAction */

/* ---
The destination specified by rds is coming to a close.
If there's any cleanup that needs to be done, do it now.
*/

static int ecEndGroupAction(RDS rds)
{
#ifdef DEBUG
    if (rds == rdsFonttbl) {
	fprintf(stderr, "End of font table\n");
    }
#endif
    return ecOK;
}

/* %%Function: ecParseSpecialKeyword */

/* ---
Evaluate an RTF control that needs special processing.
*/

static int ecParseSpecialKeyword(IPFN ipfn)
{
    if (rds == rdsSkip && ipfn != ipfnBin)  /* if we're skipping, and it's not */
        return ecOK;                        /* the \bin keyword, ignore it. */
    switch (ipfn) {
    case ipfnBin:
        ris = risBin;
        cbBin = lParam;
        break;
    case ipfnSkipDest:
        fSkipDestIfUnk = fTrue;
        break;
    case ipfnHex:
	ris = risHex;
	break;
    default:
        return ecBadTable;
    }
    return ecOK;
}

/* %%Function: main */

/* ---
Main loop. Initialize and parse RTF.
*/

static int load(char *fn, buffer *b)
{
    FILE *fp;
    int ec;

    fp = fpIn = fopen(fn, "r");
    if (!fp) {
        printf ("Can't open file %s!\n", fn);
        return 1;
    }
    row = 1;
    b->height_interest = 0;	/* fix heights later */
    set_bop(b, 0, row, 1);
    buf = b;
    if ((ec = ecRtfParse(fp)) != ecOK)
        printf("error %d parsing rtf\n", ec);
#ifdef DEBUG
    else
        printf("Parsed RTF file OK\n");
#endif
    fclose(fp);
    b->height_interest = 1;
    for (row = 1; row <= line_last_used(b, 0); row++)
		rebreak_line(b, 0, row);
    b->change = 0;
    return 0;
}

static int hexdigit(int d)
{
	switch (d) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		return d-'0';
	case 'a':
	case 'A':
		return 10;
	case 'b':
	case 'B':
		return 11;
	case 'c':
	case 'C':
		return 12;
	case 'd':
	case 'D':
		return 13;
	case 'e':
	case 'E':
		return 14;
	case 'f':
	case 'F':
		return 15;
	default:
		;
	}
	return -1;
}

/* %%Function: ecRtfParse */

/* ---
Step 1:
Isolate RTF keywords and send them to ecParseRtfKeyword;
Push and pop state at the start and end of RTF groups;
Send text to ecParseChar for further processing.
*/

static int ecRtfParse(FILE *fp)
{
    int ch;
    int ec;
    int cNibble = 2;
    int b = 0;
    while ((ch = getc(fp)) != EOF) {
        if (cGroup < 0)
            return ecStackUnderflow;
        if (ris == risBin) { /* if we're parsing binary data, handle it directly */
            if ((ec = ecParseChar(ch)) != ecOK)
                return ec;
        } else {
            switch (ch) {
            case '{':
                if ((ec = ecPushRtfState()) != ecOK)
                    return ec;
                break;
            case '}':
                if ((ec = ecPopRtfState()) != ecOK)
                    return ec;
                break;
            case '\\':
                if ((ec = ecParseRtfKeyword(fp)) != ecOK)
                    return ec;
                break;
            case '\r':
            case '\n':          /* cr and lf are noise characters... */
                break;
            default:
                if (ris == risNorm) {
                    if ((ec = ecParseChar(ch)) != ecOK)
                        return ec;
                } else {               /* parsing hex data */
		    int d;
                    if (ris != risHex)
                        return ecAssertion;
		    d = hexdigit(ch);
		    if (d == -1) return ecInvalidHex;

                    b = 16*b+d;
                    cNibble--;
                    if (!cNibble) {
                        if ((ec = ecParseChar(b)) != ecOK)
                            return ec;
                        cNibble = 2;
                        b = 0;
			ris = risNorm;
                    }
                }                   /* end else (ris != risNorm) */
                break;
            }       /* switch */
        }           /* else (ris != risBin) */
    }               /* while */
    if (cGroup < 0)
        return ecStackUnderflow;
    if (cGroup > 0)
        return ecUnmatchedBrace;
    return ecOK;
}

/* %%Function: ecPushRtfState */

/* ---
Save relevant info on a linked list of SAVE structures.
*/

static int ecPushRtfState(void)
{
    SAVE *psaveNew = malloc(sizeof(SAVE));
    if (!psaveNew)
        return ecStackOverflow;

    psaveNew -> pNext = psave;
    psaveNew -> chp = chp;
    psaveNew -> pap = pap;
    psaveNew -> sep = sep;
    psaveNew -> dop = dop;
    psaveNew -> rds = rds;
    psaveNew -> ris = ris;
    ris = risNorm;
    psave = psaveNew;
    cGroup++;
    return ecOK;
}

/* %%Function: ecPopRtfState */

/* ---
If we're ending a destination (that is, the destination is changing),
call ecEndGroupAction.
Always restore relevant info from the top of the SAVE list.
*/

static int ecPopRtfState(void)
{
    SAVE *psaveOld;
    int ec;

    if (!psave)
        return ecStackUnderflow;

    if (rds != psave->rds) {
        if ((ec = ecEndGroupAction(rds)) != ecOK)
            return ec;
    }
    chp = psave->chp;
    pap = psave->pap;
    sep = psave->sep;
    dop = psave->dop;
    rds = psave->rds;
    ris = psave->ris;

    psaveOld = psave;
    psave = psave->pNext;
    cGroup--;
    free(psaveOld);
    return ecOK;
}

/* %%Function: ecParseRtfKeyword */

/* ---
Step 2:
get a control word (and its associated value) and
call ecTranslateKeyword to dispatch the control.
*/

static int ecParseRtfKeyword(FILE *fp)
{
    int ch;
    char fParam = fFalse;
    char fNeg = fFalse;
    int param = 0;
    char *pch;
    char szKeyword[30];
    char szParameter[20];

    szKeyword[0] = '\0';
    szParameter[0] = '\0';
    if ((ch = getc(fp)) == EOF)
        return ecEndOfFile;
    if (!isalpha(ch)) {          /* a control symbol; no delimiter. */
        szKeyword[0] = (char) ch;
        szKeyword[1] = '\0';
        return ecTranslateKeyword(szKeyword, 0, fParam);
    }
    for (pch = szKeyword; isalpha(ch); ch = getc(fp))
        *pch++ = (char) ch;
    *pch = '\0';
    if (ch == '-') {
        fNeg  = fTrue;
        if ((ch = getc(fp)) == EOF)
            return ecEndOfFile;
    }
    if (isdigit(ch)) {
        fParam = fTrue;         /* a digit after the control means we have a parameter */
        for (pch = szParameter; isdigit(ch); ch = getc(fp))
            *pch++ = (char) ch;
        *pch = '\0';
        param = atoi(szParameter);
        if (fNeg)
            param = -param;
        lParam = atol(szParameter);
        if (fNeg)
            param = -param;
    }
    if (ch != ' ')
        ungetc(ch, fp);
    return ecTranslateKeyword(szKeyword, param, fParam);
}

static char fontname[256];

static struct {
	char *name;
} fonttbl[256];

static int ecFontName(int ch)
{
	char b[2];

	if (ch == ';') {	/* no more to come */
		fonttbl[chp.fNo].name = MwStrdup(fontname);
		fontname[0] = '\0';
	} else {		/* add to the font name */
		b[0] = ch;
		b[1] = '\0';
		strcat(fontname, b);
	}

	return ecOK;
}

/* %%Function: ecParseChar */

/* ---
Route the character to the appropriate destination stream.
*/

static int ecParseChar(int ch)
{
    if (ris == risBin && --cbBin <= 0)
        ris = risNorm;
    switch (rds) {
    case rdsSkip:
        /* Toss this character. */
        return ecOK;
    case rdsNorm:
        /* Output a character. Properties are valid at this point. */
        return ecPrintChar(ch);
    case rdsFonttbl:
	/* This is probably the font name */
	return ecFontName(ch);
    default:
    /* handle other destinations.... */
        return ecOK;
    }
}

/* %%Function: ecPrintChar */

/* ---
Send a character to the output file.

001225: hack to deal with mac charset
*/

static int ecPrintChar(int ch)
{
	MwFmt fmt;
	int s = 0;

	if (dop.fMac) {
		switch (ch) {
		case 0x88: ch = 'à'; break;
		case 0x89: ch = 'â'; break;
		case 0x8d: ch = 'ç'; break;
		case 0x8e: ch = 'é'; break;
		case 0x8f: ch = 'è'; break;
		case 0x90: ch = 'ê'; break;
		case 0x94: ch = 'î'; break;
		case 0x95: ch = 'ï'; break;
		case 0x99: ch = 'ô'; break;
		case 0x9d: ch = 'ù'; break;
		case 0x9e: ch = 'û'; break;
		case 0xcf: ecPrintChar('o'); ch = 'e'; break;	/* FIXME */
		default: break;
		}
	}

	if (ch == '\n') {
		row++;
		set_bop(buf, s, row, 1);
	} else {
		MwDecodeFormat(0, ~0, &fmt);
		fmt.family = MwLookupFontAlias(fonttbl[chp.fNo].name);
		if (fmt.family == NULL) fmt.family = "Helvetica";
		fmt.bold = chp.fBold;
		fmt.italic = chp.fItalic;
		fmt.uline = chp.fUnderline;
		fmt.size = 5*chp.fSize;	/* twips => decipoints */
		ins_char(buf, s, row, line_length(buf, s, row), ch,
			MwEncodeFormat(~0, &fmt));
	}

    return ecOK;
}


/* ---
Format guessing:
   1. extension .rtf or .RTF
   2. contains the string "\rtf"
*/

static int myformat(char *fn)
{
        char *ext;
        FILE *fp;
        char b[256];

        ext = strrchr(fn, '.');
        if (!ext) return 0;     /* no extension */
        if (MwStrcasecmp(ext, ".rtf"))
                return 0;       /* wrong extension */
        if ((fp = fopen(fn, "r")) == NULL) return 0;    /* can't open */
        while (fgets(b, sizeof b, fp)) {
                if (strstr(b, "\\rtf")) {
                        fclose(fp);
                        return 1;
                }
        }
        fclose(fp);
	return 0;
}

/* ---
*/
void fileio_rtf_init(void)
{
	register_format(load, save, myformat, "Rich Text Format (*.rtf)");
}

