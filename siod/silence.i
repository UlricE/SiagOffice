
/* This stuff added to keep Siod from printing on stdout. */
/* 960125 Hacked fput_st to use this instead of stdout */
/* 980304 Added TRACEME */

#include <stdarg.h>
#include <Mowitz/MwUtils.h>
#define printf statusprint	/* gets printed on llpr */
extern int ok2print;
extern void llpr(char *);
static void statusprint(char *fmt, ...)
{
  va_list ap;
  char buf[256];

  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
MW_TRACE((stderr, "statusprint(%s)", buf));
  if (ok2print) llpr(buf);
  va_end(ap);
}
