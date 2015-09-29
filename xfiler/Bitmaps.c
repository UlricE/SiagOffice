/*---------------------------------------------------------------------------
  Module FmBitmaps

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994
  
  Functions & data for handling the bitmaps and cursors.
---------------------------------------------------------------------------*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Drawing.h>

#ifdef XPM
#include <X11/xpm.h>
#endif

#include "bitmaps/xfm_file.xbm"
#include "bitmaps/xfm_filemsk.xbm"
#include "bitmaps/xfm_files.xbm"
#include "bitmaps/xfm_filesmsk.xbm"
#include "bitmaps/xfm_noentry.xbm"
#include "bitmaps/xfm_noentrymsk.xbm"
#include "bitmaps/xfm_dir.xbm"
#include "bitmaps/xfm_dirmsk.xbm"
#include "bitmaps/xfm_exec.xbm"
#include "bitmaps/xfm_execmsk.xbm"
#include "bitmaps/xfm_watch.xbm"
#include "bitmaps/xfm_watchmsk.xbm"
#include "bitmaps/xfm_lline.xbm"
#include "bitmaps/xfm_tline.xbm"
#include "bitmaps/xfm_fline.xbm"
#include "bitmaps/xfm_cline.xbm"
#include "bitmaps/xfm_larrow.xbm"
#include "bitmaps/xfm_rarrow.xbm"
#include "bitmaps/xfm_wavy_arrow.xbm"
#include "bitmaps/xfm_tick.xbm"
#include "bitmaps/xfm_notick.xbm"
#include "bitmaps/xfm_excl.xbm"
#ifndef XPM
#include "bitmaps/xfm_symlnk.xbm"
#include "bitmaps/xfm_dirlnk.xbm"
#include "bitmaps/xfm_execlnk.xbm"
#include "bitmaps/xfm_blackhole.xbm"
#include "bitmaps/xfm_icon.xbm"
#else
#include "pixmaps/xfm_file.xpm"
#include "pixmaps/xfm_dir.xpm"
#include "pixmaps/xfm_updir.xpm"
#include "pixmaps/xfm_exec.xpm"
#include "pixmaps/xfm_files.xpm"
#include "pixmaps/xfm_symlnk.xpm"
#include "pixmaps/xfm_dirlnk.xpm"
#include "pixmaps/xfm_execlnk.xpm"
#include "pixmaps/xfm_blackhole.xpm"
#include "pixmaps/xfm_icon.xpm"
#endif

#include "Files.h"

/*-----------------------------------------------------------------------------
  STATIC DATA
-----------------------------------------------------------------------------*/

typedef struct {
  char *bits;
  int width, height;
} BitmapRec;

#ifdef __STDC__
#define ICON(x) { x##_bits, x##_width, x##_height }
#else
#define ICON(x) { x/**/_bits, x/**/_width, x/**/_height }
#endif

static BitmapRec bitmaps[] = {
  ICON(xfm_file), ICON(xfm_filemsk), ICON(xfm_files), ICON(xfm_filesmsk),
  ICON(xfm_noentry), ICON(xfm_noentrymsk), ICON(xfm_dir), ICON(xfm_dirmsk),
  ICON(xfm_exec), ICON(xfm_execmsk), ICON(xfm_watch), ICON(xfm_watchmsk),
  ICON(xfm_lline), ICON(xfm_tline), ICON(xfm_fline), ICON(xfm_cline),
  ICON(xfm_larrow), ICON(xfm_rarrow), ICON(xfm_wavy_arrow), ICON(xfm_tick),
  ICON(xfm_notick), ICON(xfm_excl),
#ifndef XPM
  ICON(xfm_files), ICON(xfm_dir), ICON(xfm_dir), ICON(xfm_file),
  ICON(xfm_exec), ICON(xfm_symlnk), ICON(xfm_dirlnk), ICON(xfm_execlnk),
  ICON(xfm_blackhole),
#endif
};

#ifdef XPM
static char **pixmaps[] = {
  xfm_files_xpm, xfm_dir_xpm, xfm_updir_xpm, xfm_file_xpm,
  xfm_exec_xpm, xfm_symlnk_xpm, xfm_dirlnk_xpm, xfm_execlnk_xpm,
  xfm_blackhole_xpm,
};
#endif

typedef struct {
  int source, mask;
} CursorRec;

static CursorRec cursors[] = {
  { FILE_CBM, FILEMSK_CBM },
  { FILES_CBM, FILESMSK_CBM },
  { NOENTRY_CBM, NOENTRYMSK_CBM },
  { DIR_CBM, DIRMSK_CBM },
  { EXEC_CBM, EXECMSK_CBM },
  { WATCH_CBM, WATCHMSK_CBM }
};

/*-----------------------------------------------------------------------------
  PUBLIC DATA
-----------------------------------------------------------------------------*/

Pixmap *bm;
Cursor *curs;

/*-----------------------------------------------------------------------------
  PUBLIC FUNCTIONS
-----------------------------------------------------------------------------*/

void readBitmaps()
{
  int i;
  Display *dpy;
  int scrn;
  Colormap cmp;
  Window win;
  XColor black, white;
#ifdef XPM
  XpmAttributes xpm_attr;
  static XpmColorSymbol none_color = { NULL, "None", (Pixel)0 };
#endif

  dpy = XtDisplay(toplevel);
  win = DefaultRootWindow(dpy);
  scrn = DefaultScreen(dpy);
  cmp = DefaultColormap(dpy, scrn);

  black.pixel = BlackPixel(dpy, scrn);
  XQueryColor(dpy, cmp, &black);
  white.pixel = WhitePixel(dpy, scrn);
  XQueryColor(dpy, cmp, &white);

  bm = (Pixmap *) XtMalloc(END_BM * sizeof(Pixmap *));
  curs = (Cursor *) XtMalloc(XtNumber(cursors) * sizeof(Cursor *));

  /* create the hardcoded bitmaps */

  for (i=0; i<XtNumber(bitmaps); i++)
    bm[i] = XCreateBitmapFromData(dpy, win, bitmaps[i].bits,
				  bitmaps[i].width, bitmaps[i].height);
#ifdef XPM
  xpm_attr.valuemask = XpmReturnPixels|XpmColorSymbols;
  xpm_attr.colorsymbols = &none_color;
  xpm_attr.numsymbols = 1;
  XtVaGetValues(toplevel, XtNbackground, &none_color.pixel, NULL);
  for (i=XtNumber(bitmaps); i<ICON_BM; i++)
    XpmCreatePixmapFromData(dpy, win, pixmaps[i-XtNumber(bitmaps)], &bm[i],
			    NULL, &xpm_attr);
#endif

  /* create the cursors */

  for (i=0; i<XtNumber(cursors); i++)
    curs[i] = XCreatePixmapCursor(dpy, bm[cursors[i].source], 
				  bm[cursors[i].mask], &black, &white, 16, 16);

  /* create the application icons */

#ifdef XPM
  XpmCreatePixmapFromData(dpy, win, xfm_icon_xpm, &bm[ICON_BM],
			  &bm[ICONMSK_BM], NULL);
#else
  bm[ICON_BM] = XCreateBitmapFromData(dpy, win, xfm_icon_bits,
				      xfm_icon_width, xfm_icon_height);
  bm[ICONMSK_BM] = None;
#endif
}

Pixmap readIcon(char *name)
{
  Display *dpy = XtDisplay(toplevel);
  Window win = DefaultRootWindow(dpy);
  Screen *scrn = XtScreen(toplevel);
  Pixmap icon_bm;
  char fullname[MAXPATHLEN];
  unsigned int w, h;
  int x, y;

#ifdef XPM

  XpmAttributes xpm_attr;
  static XpmColorSymbol none_color = { NULL, "None", (Pixel)0 };

  /* first search for xpm icons: */

  xpm_attr.valuemask = XpmReturnPixels|XpmColorSymbols;
  xpm_attr.colorsymbols = &none_color;
  xpm_attr.numsymbols = 1;
  XtVaGetValues(toplevel, XtNbackground, &none_color.pixel, NULL);

  if (XpmReadFileToPixmap(dpy, win,
			  searchPath(fullname, resources.pixmap_path, name),
			  &icon_bm, NULL, &xpm_attr) == XpmSuccess)
    return icon_bm;
#endif

  /* now search bitmap in standard locations (*bitmapFilePath): */

  icon_bm = XmuLocateBitmapFile(scrn, name, NULL, 0, (int *)&w, (int *)&h,
				 &x, &y);
  if (icon_bm != None)
    return icon_bm;

  /* finally search along *bitmapPath: */

  if (XReadBitmapFile(dpy, win,
		      searchPath(fullname, resources.bitmap_path, name),
		      &w, &h, &icon_bm, &x, &y) == BitmapSuccess)
    return icon_bm;
  return None;
}

