/*---------------------------------------------------------------------------
  Module FmExec

  (c) Simon Marlow 1990-92
  (c) Albert Graef 1994

  Procedures for executing files
---------------------------------------------------------------------------*/
#include <stdio.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include <X11/Intrinsic.h>

#include "Files.h"

ExecMapRec *exec_map = NULL;
int n_exec_maps = 0;

/*---------------------------------------------------------------------------
  PUBLIC FUNCTIONS
---------------------------------------------------------------------------*/

char **makeArgv(char *action)
{
  char **argv;
  int i = 0;

  argv = (char **) XtMalloc( (user.arg0flag ? 5 : 4) * sizeof(char *));

  argv[i++] = XtNewString(user.shell);
  argv[i++] = XtNewString("-c");
  argv[i++] = XtNewString(action);
  if (user.arg0flag)
    argv[i++] = XtNewString(user.shell);
  argv[i] = NULL;

  return argv;
}

/*---------------------------------------------------------------------------*/

void freeArgv(char **argv)
{
  int j;

  for (j=0; argv[j]; j++)
    XTFREE(argv[j]);
  XTFREE(argv);
}

/*---------------------------------------------------------------------------*/

static void echoarg(char *arg)
{
  char *s;
  for (s = arg; *s; s++)
    if (isspace(*s)) {
      fprintf(stderr, " '%s'", arg);
      return;
    }
  fprintf(stderr, " %s", arg);
}

/*---------------------------------------------------------------------------*/

void executeApplication(char *path, char *directory, char **argv)
{
  int pid;

  zzz();
  XFlush(XtDisplay(toplevel));
  if (chdir(directory)) {
    wakeUp();
    sysError("Can't chdir:");
  } else if ((pid = fork()) == -1) {
    wakeUp();
    sysError("Can't fork:");
  } else {
    if (!pid) {
      if (resources.echo_actions) {
	char **arg;
	fprintf(stderr, "[%s] %s", directory, path);
	for (arg = argv+1; *arg; arg++)
	  echoarg(*arg);
	fprintf(stderr, "\n");
      }
      /* Make sure that child processes don't lock up xfm with keyboard
	 input. This is certainly a kludge and if you know of any better
	 way to do this, please let me know. -ag */
      freopen("/dev/null", "r", stdin);
      execvp(path, argv);
      perror("Exec failed");
      exit(1);
    } else {
      sleep(1);
      wakeUp();
    }
  }    
}
