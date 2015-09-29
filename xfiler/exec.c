#include <X11/Intrinsic.h>
#include <string.h>
#include "Files.h"

void ExecExternalApplication(Display *dpy, int x, int y, char *label, 
			     char *icon, char *cmd, char *drop, char *dir)
{	
	char path[1024];
	char *p;
	char **argv;

	if (dir)
	{
	    newFileWindow(dir,resources.initial_display_type,True,False);
	    return;
	}

	strcpy(path, cmd);
	p= path+strlen(path);
	while(*p--!='/');
	p++;
	p[0] = '\0';
	
	argv = (char **) XtMalloc(2 * sizeof(char *));
	argv[0] = XtNewString(cmd);
	argv[1] = NULL;
	
	executeApplication(cmd, path, argv);

	freeArgv(argv);

}
