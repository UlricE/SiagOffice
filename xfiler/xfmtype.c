#include <stdio.h>

extern void magic_parse_file();
extern int magic_get_type();

char buf[1024];

void usage(prog)
char *prog;
{
    fprintf(stderr, "Usage: %s [-m magic_file ] [-f] file ...\n", prog);
}

void main(argc, argv)
int argc;
char **argv;
{
    int i;
    for(i = 1; i < argc; i++)
    {
	if(argv[i][0] == '-')
	{
	    switch (argv[i][1])
	    {
	    case 'm':
		i++;
		if(i < argc)
		    magic_parse_file(argv[i]);
		else
		{
		    fprintf(stderr, "Missing argument: magic file\n");
		    usage(argv[0]);
		    exit(1);
		}
		break;
	    case 'f':
		i++;
		if(i < argc)
		{
		    magic_get_type(argv[i], buf);
		    printf("%s: %s\n", argv[i], buf);
		}
		else
		{
		    fprintf(stderr, "Missing argument: file\n");
		    usage(argv[0]);
		    exit(1);
		}
		break;
	    default:
		fprintf(stderr, "Bad option %s\n", argv[i]);
		usage(argv[0]);
		exit(1);
	    }
	    continue;
	}
	magic_get_type(argv[i], buf);
	printf("%s: %s\n", argv[i], buf);
    }
}
