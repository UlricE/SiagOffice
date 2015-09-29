/*-*-mode:c-*-*/

/* DEC-94 George Carrette. Additional lisp util subrs,
   many of them depending on operating system calls.
   Note that I have avoided more than one nesting of conditional compilation,
   and the use of the else clause, in hopes of preserving some readability.
   For better or worse I avoided gnu autoconfigure because it was complex required
   scripts nearly as big as this source file. Meanwhile there is some ANSI POSIX
   convergence going on.
 */

/* ---
981205 Autoconf support by Ulric

Removed all "#ifdef unix" et al, because we only use this for Unix
Removed all VMS specific code.
Removed all WIN32 specific code.
Most "#if defined(thissystem) || defined(thatsystem)" replaced with
autoconf tests.
Removed all dynamic library code.
--- */

/* start of Siag-specific attempts to make this module more portable */

#include "../config.h"

#ifndef unix
#define unix
#endif
#ifndef __USE_XOPEN
#define __USE_XOPEN
#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <stdarg.h>
#include <limits.h>

#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <pwd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <grp.h>
#include <utime.h>
#include <sys/fcntl.h>


#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

#ifdef HAVE_SYS_MODE_H
#include <sys/mode.h>
#endif
#ifdef HAVE_FNMATCH_H
#include <fnmatch.h>
#endif
#ifdef HAVE_CRYPT_H
#include <crypt.h>
#endif
#ifdef HAVE_SYS_MKDEV_H
#include <sys/mkdev.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif


#include "siod.h"
#include "siodp.h"
#include "md5.h"

static void init_slibu_version(void)
{setvar(cintern("*slibu-version*"),
	cintern("$Id: slibu.c,v 1.1.1.1 2003/01/15 23:55:13 ulric Exp $"),
	NIL);}


LISP sym_channels = NIL;
long tc_opendir = 0;

char *ld_library_path_env = "LD_LIBRARY_PATH";


LISP lsystem(LISP args)
{int retval;
 long iflag;
 iflag = no_interrupt(1);
 retval = system(get_c_string(string_append(args)));
 no_interrupt(iflag);
 if (retval < 0)
   return(cons(flocons(retval),llast_c_errmsg(-1)));
 else
   return(flocons(retval));}

LISP lgetuid(void)
{return(flocons(getuid()));}

LISP lgetgid(void)
{return(flocons(getgid()));}


LISP lcrypt(LISP key,LISP salt)
{char *result;
 if ((result = crypt(get_c_string(key),get_c_string(salt))))
   return(strcons(strlen(result),result));
 else
   return(NIL);}


LISP lgetcwd(void)
{char path[PATH_MAX+1];
 if (getcwd(path,sizeof(path)))
   return(strcons(strlen(path),path));
 else
   return(err("getcwd",llast_c_errmsg(-1)));}


LISP ldecode_pwent(struct passwd *p)
{return(symalist(
		 "name",strcons(strlen(p->pw_name),p->pw_name),
		 "passwd",strcons(strlen(p->pw_passwd),p->pw_passwd),
		 "uid",flocons(p->pw_uid),
		 "gid",flocons(p->pw_gid),
		 "dir",strcons(strlen(p->pw_dir),p->pw_dir),
		 "gecos",strcons(strlen(p->pw_gecos),p->pw_gecos),
/* FIXME: this is horrible */
#if defined(__osf__) || defined(hpux) || defined(sun)
		 "comment",strcons(strlen(p->pw_comment),p->pw_comment),
#endif
#if defined(hpux) || defined(sun)
		 "age",strcons(strlen(p->pw_age),p->pw_age),
#endif
#if defined(__osf__)
		 "quota",flocons(p->pw_quota),
#endif
		 "shell",strcons(strlen(p->pw_shell),p->pw_shell),
		 NULL));}

static char *strfield(char *name,LISP alist)
{LISP value,key = rintern(name);
 if NULLP(value = assq(key,alist))
   return("");
 return(get_c_string(cdr(value)));}

static long longfield(char *name,LISP alist)
{LISP value,key = rintern(name);
 if NULLP(value = assq(key,alist))
   return(0);
 return(get_c_long(cdr(value)));}
 
void lencode_pwent(LISP alist,struct passwd *p)
{p->pw_name = strfield("name",alist);
 p->pw_passwd = strfield("passwd",alist);
 p->pw_uid = longfield("uid",alist);
 p->pw_gid = longfield("gid",alist);
 p->pw_dir = strfield("dir",alist);
 p->pw_gecos = strfield("gecos",alist);
/* FIXME: */
#if defined(__osf__) || defined(hpux) || defined(sun)
 p->pw_comment = strfield("comment",alist);
#endif
#if defined(hpux) || defined(sun)
 p->pw_age = strfield("age",alist);
#endif
#if defined(__osf__)
 p->pw_quota = longfield("quota",alist);
#endif
 p->pw_shell = strfield("shell",alist);}

LISP lgetpwuid(LISP luid)
{int iflag;
 uid_t uid;
 struct passwd *p;
 LISP result = NIL;
 uid = get_c_long(luid);
 iflag = no_interrupt(1);
 if ((p = getpwuid(uid)))
   result = ldecode_pwent(p);
 no_interrupt(iflag);
 return(result);}

LISP lgetpwnam(LISP nam)
{int iflag;
 struct passwd *p;
 LISP result = NIL;
 iflag = no_interrupt(1);
 if ((p = getpwnam(get_c_string(nam))))
   result = ldecode_pwent(p);
 no_interrupt(iflag);
 return(result);}

LISP lgetpwent(void)
{int iflag;
 LISP result = NIL;
 struct passwd *p;
 iflag = no_interrupt(1);
 if ((p = getpwent()))
   result = ldecode_pwent(p);
 no_interrupt(iflag);
 return(result);}

LISP lsetpwent(void)
{int iflag = no_interrupt(1);
 setpwent();
 no_interrupt(iflag);
 return(NIL);}

LISP lendpwent(void)
{int iflag = no_interrupt(1);
 endpwent();
 no_interrupt(iflag);
 return(NIL);}

#if defined(HAVE_SETPWFILE)
LISP lsetpwfile(LISP fname)
{int iflag = no_interrupt(1);
 setpwfile(get_c_string(fname));
 no_interrupt(iflag);
 return(NIL);}
#endif

#ifdef HAVE_PUTPWENT
LISP lputpwent(LISP alist,LISP file)
{int iflag = no_interrupt(1);
 int status;
 struct passwd p;
 lencode_pwent(alist,&p);
 status = putpwent(&p,get_c_file(file,NULL));
 no_interrupt(iflag);
 return(NIL);}
#endif

LISP laccess_problem(LISP lfname,LISP lacc)
{char *fname = get_c_string(lfname);
 char *acc = get_c_string(lacc),*p;
 int amode = 0,iflag = no_interrupt(1),retval;
 for(p=acc;*p;++p)
   switch(*p)
     {case 'r':
	amode |= R_OK;
	break;
      case 'w':
	amode |= W_OK;
	break;
      case 'x':
	amode |= X_OK;
	break;
      case 'f':
	amode |= F_OK;
	break;
      default:
	err("bad access mode",lacc);}
 retval = access(fname,amode);
 no_interrupt(iflag);
 if (retval < 0)
   return(llast_c_errmsg(-1));
 else
   return(NIL);}

LISP lsymlink(LISP p1,LISP p2)
{long iflag;
 iflag = no_interrupt(1);
 if (symlink(get_c_string(p1),get_c_string(p2)))
   return(err("symlink",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}

LISP llink(LISP p1,LISP p2)
{long iflag;
 iflag = no_interrupt(1);
 if (link(get_c_string(p1),get_c_string(p2)))
   return(err("link",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}

LISP lunlink(LISP p)
{long iflag;
 iflag = no_interrupt(1);
 if (unlink(get_c_string(p)))
   return(err("unlink",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}

LISP lrmdir(LISP p)
{long iflag;
 iflag = no_interrupt(1);
 if (rmdir(get_c_string(p)))
   return(err("rmdir",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}

LISP lmkdir(LISP p,LISP m)
{long iflag;
 iflag = no_interrupt(1);
 if (mkdir(get_c_string(p),get_c_long(m)))
   return(err("mkdir",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}

LISP lreadlink(LISP p)
{long iflag;
 char buff[PATH_MAX+1];
 int size;
 iflag = no_interrupt(1);
 if ((size = readlink(get_c_string(p),buff,sizeof(buff))) < 0)
   return(err("readlink",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(strcons(size,buff));}

LISP lrename(LISP p1,LISP p2)
{long iflag;
 iflag = no_interrupt(1);
 if (rename(get_c_string(p1),get_c_string(p2)))
   return(err("rename",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}


/* Changing all of this to rand so it conforms to C /Ulric */

LISP lrandom(LISP n)
{int res;
 res = rand();
 return(flocons(NNULLP(n) ? res % get_c_long(n) : res));}

LISP lsrandom(LISP n)
{long seed;
 seed = get_c_long(n);
 srand(seed);
 return(NIL);}


LISP lfork(void)
{int iflag;
 pid_t pid;
 iflag = no_interrupt(1);
 pid = fork();
 if (pid == 0)
   {no_interrupt(iflag);
    return(NIL);}
 if (pid == -1)
   return(err("fork",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(flocons(pid));}


char **list2char(LISP *safe,LISP v)
{char **x,*tmp;
 long j,n;
 LISP l;
 n = get_c_long(llength(v));
 *safe = cons(mallocl(&x,sizeof(char *) * (n + 1)),*safe);
 for(l=v,j=0;j<n;l=cdr(l),++j)
   {tmp = get_c_string(car(l));
    *safe = cons(mallocl(&x[j],strlen(tmp)+1),*safe);
    strcpy(x[j],tmp);}
 return(x);}


LISP lexec(LISP path,LISP args,LISP env)
{int iflag;
 char **argv = NULL, **envp = NULL;
 LISP gcsafe=NIL;
 iflag = no_interrupt(1);
 argv = list2char(&gcsafe,args);
 if NNULLP(env)
   envp = list2char(&gcsafe,env);
 if (envp)
   execve(get_c_string(path),argv,envp);
 else
   execv(get_c_string(path),argv);
 no_interrupt(iflag);
 return(err("exec",llast_c_errmsg(-1)));}

LISP lnice(LISP val)
{int iflag,n;
 n = get_c_long(val);
 iflag = no_interrupt(1);
 n = nice(n);
 if (n == -1)
   err("nice",llast_c_errmsg(-1));
 no_interrupt(iflag);
 return(flocons(n));}


int assemble_options(LISP l, ...)
{int result = 0,val,noptions,nmask = 0;
 LISP lsym,lp = NIL;
 char *sym;
 va_list syms;
 if NULLP(l) return(0);
 noptions = CONSP(l) ? get_c_long(llength(l)) : -1;
 va_start(syms,l);
 while((sym = va_arg(syms,char *)))
   {val = va_arg(syms,int);
    lsym = cintern(sym);
    if (EQ(l,lsym) || (CONSP(l) && NNULLP(lp = memq(lsym,l))))
      {result |= val;
       if (noptions > 0)
	 nmask = nmask | (1 << (noptions - get_c_long(llength(lp))));
       else
	 noptions = -2;}}
 va_end(syms);
 if ((noptions == -1) ||
     ((noptions > 0) && (nmask != ((1 << noptions) - 1))))
   err("contains undefined options",l);
 return(result);}


LISP lwait(LISP lpid,LISP loptions)
{pid_t pid,ret;
 int iflag,status = 0,options;
 pid = NULLP(lpid) ? -1 : get_c_long(lpid);
 options = assemble_options(loptions,
#ifdef WCONTINUED
			    "WCONTINUED",WCONTINUED,
#endif
#ifdef WNOWAIT
			    "WNOWAIT",WNOWAIT,
#endif
			    "WNOHANG",WNOHANG,
			    "WUNTRACED",WUNTRACED,
			    NULL);
 iflag = no_interrupt(1); 
 ret = waitpid(pid,&status,options);
 no_interrupt(iflag);
 if (ret == 0)
   return(NIL);
 else if (ret == -1)
   return(err("wait",llast_c_errmsg(-1)));
 else
   /* should do more decoding on the status */
   return(cons(flocons(ret),cons(flocons(status),NIL)));}

LISP lkill(LISP pid,LISP sig)
{long iflag;
 iflag = no_interrupt(1);
 if (kill(get_c_long(pid),
	  NULLP(sig) ? SIGKILL : get_c_long(sig)))
   err("kill",llast_c_errmsg(-1));
 else
   no_interrupt(iflag);
 return(NIL);}


LISP lgetpid(void)
{return(flocons(getpid()));}

LISP lgetpgrp(void)
{return(flocons(getpgrp()));}

LISP lgetgrgid(LISP n)
{gid_t gid;
 struct group *gr;
 long iflag,j;
 LISP result = NIL;
 gid = get_c_long(n);
 iflag = no_interrupt(1);
 if ((gr = getgrgid(gid)))
   {result = cons(strcons(strlen(gr->gr_name),gr->gr_name),result);
    for(j=0;gr->gr_mem[j];++j)
      result = cons(strcons(strlen(gr->gr_mem[j]),gr->gr_mem[j]),result);
    result = nreverse(result);}
 no_interrupt(iflag);
 return(result);}


LISP lgetppid(void)
{return(flocons(getppid()));}

LISP lmemref_byte(LISP addr)
{unsigned char *ptr = (unsigned char *) get_c_long(addr);
 return(flocons(*ptr));}

LISP lexit(LISP val)
{int iflag = no_interrupt(1);
 exit(get_c_long(val));
 no_interrupt(iflag);
 return(NIL);}

LISP ltrunc(LISP x)
{long i;
 if NFLONUMP(x) err("wta to trunc",x);
 i = (long) FLONM(x);
 return(flocons((double) i));}

LISP lputenv(LISP lstr)
{char *orig,*cpy;
 orig = get_c_string(lstr);
 /* unix putenv keeps a pointer to the string we pass,
    therefore we must make a fresh copy, which is memory leaky. */
 cpy = (char *) must_malloc(strlen(orig)+1);
 strcpy(cpy,orig);
  if (putenv(cpy))
   return(err("putenv",llast_c_errmsg(-1)));
 else
   return(NIL);}


/* I hate doing this, but it is apparently necessary /Ulric */
/*#ifdef MD5*/

MD5_CTX * get_md5_ctx(LISP a)
{if (TYPEP(a,tc_byte_array) &&
     (a->storage_as.string.dim == sizeof(MD5_CTX)))
   return((MD5_CTX *)a->storage_as.string.data);
 else
   {err("not an MD5_CTX array",a);
    return(NULL);}}

LISP md5_init(void)
{LISP a = arcons(tc_byte_array,sizeof(MD5_CTX),1);
 MD5Init(get_md5_ctx(a));
 return(a);}

void md5_update_from_file(MD5_CTX *ctx,FILE *f,unsigned char *buff,long dim)
{size_t len;
 while((len = fread(buff,sizeof(buff[0]),dim,f)))
   MD5Update(ctx,buff,len);}

LISP md5_update(LISP ctx,LISP str,LISP len)
{char *buffer; long dim,n;
 buffer = get_c_string_dim(str,&dim);
 if TYPEP(len,tc_c_file)
   {md5_update_from_file(get_md5_ctx(ctx), get_c_file(len,NULL),
			 (unsigned char *)buffer,dim);
    return(NIL);}
 else if NULLP(len)
   n = dim;
 else
   {n = get_c_long(len);
    if ((n < 0) || (n > dim)) err("invalid length for string",len);}
 MD5Update(get_md5_ctx(ctx),(unsigned char *)buffer,n);
 return(NIL);}

LISP md5_final(LISP ctx)
{LISP result = arcons(tc_byte_array,16,0);
 MD5Final((unsigned char *) result->storage_as.string.data,
	  get_md5_ctx(ctx));
 return(result);}

/*#endif*/	/* MD5 */
/* End of Ulric's butchery */

#if defined(HAVE_GETRLIMIT) && defined(RLIMIT_CPU)

void handle_sigxcpu(int sig)
{struct rlimit x;
 if (getrlimit(RLIMIT_CPU,&x))
   {errjmp_ok = 0;
    err("getrlimit",llast_c_errmsg(-1));}
 if (x.rlim_cur >= x.rlim_max)
   {errjmp_ok = 0;
    err("hard cpu limit exceded",NIL);}
 if (nointerrupt == 1)
   interrupt_differed = 1;
 else
   err("cpu limit exceded",NIL);}

LISP cpu_usage_limits(LISP soft,LISP hard)
{struct rlimit x;
 if (NULLP(soft) && NULLP(hard))
   {if (getrlimit(RLIMIT_CPU,&x))
      return(err("getrlimit",llast_c_errmsg(-1)));
    else
      return(listn(2,flocons(x.rlim_cur),flocons(x.rlim_max)));}
 else
   {x.rlim_cur = get_c_long(soft);
    x.rlim_max = get_c_long(hard);
    signal(SIGXCPU,handle_sigxcpu);
    if (setrlimit(RLIMIT_CPU,&x))
      return(err("setrlimit",llast_c_errmsg(-1)));
    else
      return(NIL);}}

#endif


static int handle_sigalrm_flag = 0;

void handle_sigalrm(int sig)
{if (nointerrupt == 1)
  {if (handle_sigalrm_flag)
    /* If we were inside a system call then it would be
       interrupted even if we take no action here.
       But sometimes we want to be really sure of signalling
       an error, hence the flag. */
    interrupt_differed = 1;}
 else
   err("alarm signal",NIL);}

LISP lalarm(LISP seconds,LISP flag)
{long iflag;
 int retval;
 iflag = no_interrupt(1);
 signal(SIGALRM,handle_sigalrm);
 handle_sigalrm_flag = NULLP(flag) ? 0 : 1;
 retval = alarm(get_c_long(seconds));
 no_interrupt(iflag);
 return(flocons(retval));}


#if defined(HAVE_GETRUSAGE) && defined(RUSAGE_SELF) && defined(RUSAGE_CHILDREN)

#define TV_FRAC(x) (((double)x.tv_usec) * 1.0e-6)

#ifdef SUN5
int getrusage(int,struct rusage *);
#endif

LISP current_resource_usage(LISP kind)
{struct rusage u;
 int code;
 if (NULLP(kind) || EQ(cintern("SELF"),kind))
   code = RUSAGE_SELF;
 else if EQ(cintern("CHILDREN"),kind)
   code = RUSAGE_CHILDREN;
 else
   return(err("unknown rusage",kind));
 if (getrusage(code,&u))
   return(err("getrusage",llast_c_errmsg(-1)));
 return(symalist("utime",flocons(((double)u.ru_utime.tv_sec) +
				 TV_FRAC(u.ru_utime)),
		 "stime",flocons(((double)u.ru_stime.tv_sec) +
				 TV_FRAC(u.ru_stime)),
		 "maxrss",flocons(u.ru_maxrss),
		 "ixrss",flocons(u.ru_ixrss),
		 "idrss",flocons(u.ru_idrss),
		 "isrss",flocons(u.ru_isrss),
		 "minflt",flocons(u.ru_minflt),
		 "majflt",flocons(u.ru_majflt),
		 "nswap",flocons(u.ru_nswap),
		 "inblock",flocons(u.ru_inblock),
		 "oublock",flocons(u.ru_oublock),
		 "msgsnd",flocons(u.ru_msgsnd),
		 "msgrcv",flocons(u.ru_msgrcv),
		 "nsignals",flocons(u.ru_nsignals),
		 "nvcsw",flocons(u.ru_nvcsw),
		 "nivcsw",flocons(u.ru_nivcsw),
		 NULL));}

#endif


/* FIXME: there are standard autoconf macros to make this more portable */

LISP l_opendir(LISP name)
{long iflag;
 LISP value;
 DIR *d;
 iflag = no_interrupt(1);
 value = cons(NIL,NIL);
 if (!(d = opendir(get_c_string(name))))
   return(err("opendir",llast_c_errmsg(-1)));
 value->type = tc_opendir;
 CAR(value) = (LISP) d;
 no_interrupt(iflag);
 return(value);}

DIR *get_opendir(LISP v,long oflag)
{if NTYPEP(v,tc_opendir) err("not an opendir",v);
 if NULLP(CAR(v))
   {if (oflag) err("opendir not open",v);
    return(NULL);}
 return((DIR *)CAR(v));}

LISP l_closedir(LISP v)
{long iflag,old_errno;
 DIR *d;
 iflag = no_interrupt(1);
 d = get_opendir(v,1);
 old_errno = errno;
 CAR(v) = NIL;
 if (closedir(d))
   return(err("closedir",llast_c_errmsg(old_errno)));
 no_interrupt(iflag);
 return(NIL);}

void  opendir_gc_free(LISP v)
{DIR *d;
 if ((d = get_opendir(v,0)))
   closedir(d);}

LISP l_readdir(LISP v)
{long iflag,namlen;
 DIR *d;
 struct dirent *r;
 d = get_opendir(v,1);
 iflag = no_interrupt(1);
 r = readdir(d);
 no_interrupt(iflag);
 if (!r) return(NIL);
/* FIXME: the following is unsatisfactory */
#if defined(sun) || defined(sgi) || defined(linux)
 namlen = safe_strlen(r->d_name,r->d_reclen);
#else
 namlen = r->d_namlen;
#endif
 return(strcons(namlen,r->d_name));}

void opendir_prin1(LISP ptr,struct gen_printio *f)
{char buffer[256];
 sprintf(buffer,"#<OPENDIR %p>",get_opendir(ptr,0));
 gput_st(f,buffer);}


LISP file_times(LISP fname)
{struct stat st;
 int iflag,ret;
 iflag = no_interrupt(1);
 ret = stat(get_c_string(fname),&st);
 no_interrupt(iflag);
 if (ret)
   return(NIL);
 else
   return(cons(flocons(st.st_ctime),
	       cons(flocons(st.st_mtime),NIL)));}


LISP decode_st_moden(mode_t mode)
{LISP ret = NIL;
 if (mode & S_ISUID) ret = cons(cintern("SUID"),ret);
 if (mode & S_ISGID) ret = cons(cintern("SGID"),ret);
 if (mode & S_IRUSR) ret = cons(cintern("RUSR"),ret);
 if (mode & S_IWUSR) ret = cons(cintern("WUSR"),ret);
 if (mode & S_IXUSR) ret = cons(cintern("XUSR"),ret);
 if (mode & S_IRGRP) ret = cons(cintern("RGRP"),ret);
 if (mode & S_IWGRP) ret = cons(cintern("WGRP"),ret);
 if (mode & S_IXGRP) ret = cons(cintern("XGRP"),ret);
 if (mode & S_IROTH) ret = cons(cintern("ROTH"),ret);
 if (mode & S_IWOTH) ret = cons(cintern("WOTH"),ret);
 if (mode & S_IXOTH) ret = cons(cintern("XOTH"),ret);
 if (S_ISFIFO(mode)) ret = cons(cintern("FIFO"),ret);
 if (S_ISDIR(mode)) ret = cons(cintern("DIR"),ret);
 if (S_ISCHR(mode)) ret = cons(cintern("CHR"),ret);
 if (S_ISBLK(mode)) ret = cons(cintern("BLK"),ret);
 if (S_ISREG(mode)) ret = cons(cintern("REG"),ret);
 if (S_ISLNK(mode)) ret = cons(cintern("LNK"),ret);
 if (S_ISSOCK(mode)) ret = cons(cintern("SOCK"),ret);
 return(ret);}

LISP encode_st_mode(LISP l)
{return(flocons(assemble_options(l,
				 "SUID",S_ISUID,
				 "SGID",S_ISGID,
				 "RUSR",S_IRUSR,
				 "WUSR",S_IWUSR,
				 "XUSR",S_IXUSR,
				 "RGRP",S_IRGRP,
				 "WGRP",S_IWGRP,
				 "XGRP",S_IXGRP,
				 "ROTH",S_IROTH,
				 "WOTH",S_IWOTH,
				 "XOTH",S_IXOTH,
				 NULL)));}

LISP decode_st_mode(LISP value)
{return(decode_st_moden(get_c_long(value)));}

LISP decode_stat(struct stat *s)
{return(symalist("dev",flocons(s->st_dev),
		 "ino",flocons(s->st_ino),
		 "mode",decode_st_moden(s->st_mode),
		 "nlink",flocons(s->st_nlink),
		 "uid",flocons(s->st_uid),
		 "gid",flocons(s->st_gid),
		 "rdev",flocons(s->st_rdev),
		 "size",flocons(s->st_size),
		 "atime",flocons(s->st_atime),
		 "mtime",flocons(s->st_mtime),
		 "ctime",flocons(s->st_ctime),
		 "blksize",flocons(s->st_blksize),
		 "blocks",flocons(s->st_blocks),
/* FIXME: should check for these fields, not for the system */
#if defined(__osf__)
		 "flags",flocons(s->st_flags),
		 "gen",flocons(s->st_gen),
#endif
		 NULL));}
	      

LISP g_stat(LISP fname,int (*fcn)(const char *,struct stat *))
{struct stat st;
 int iflag,ret;
 iflag = no_interrupt(1);
 ret = (*fcn)(get_c_string(fname),&st);
 no_interrupt(iflag);
 if (ret)
   return(NIL);
 else
   return(decode_stat(&st));}

LISP l_stat(LISP fname)
{return(g_stat(fname,stat));}

LISP l_lstat(LISP fname)
{return(g_stat(fname,lstat));}

LISP l_fstat(LISP f)
{struct stat st;
 int iflag,ret;
 iflag = no_interrupt(1);
 ret = fstat(fileno(get_c_file(f,NULL)),&st);
 no_interrupt(iflag);
 if (ret)
   return(NIL);
 else
   return(decode_stat(&st));}

#ifdef HAVE_FNMATCH

LISP l_fnmatch(LISP pat,LISP str,LISP flgs)
{if (fnmatch(get_c_string(pat),
	     get_c_string(str),
	     0))
   return(NIL);
 else
   return(a_true_value());}

#endif

LISP lutime(LISP fname,LISP mod,LISP ac)
{struct utimbuf x;
 x.modtime = get_c_long(mod);
 x.actime = NNULLP(ac) ? get_c_long(ac) : time(NULL);
 if (utime(get_c_string(fname), &x))
   return(err("utime",llast_c_errmsg(-1)));
 else
   return(NIL);}

LISP l_chmod(LISP path,LISP mode)
{if (chmod(get_c_string(path),get_c_long(mode)))
   return(err("chmod",llast_c_errmsg(-1)));
 else
   return(NIL);}

LISP lfchmod(LISP file,LISP mode)
{if (fchmod(fileno(get_c_file(file,NULL)),get_c_long(mode)))
   return(err("fchmod",llast_c_errmsg(-1)));
 else
   return(NIL);}

LISP encode_open_flags(LISP l)
{return(flocons(assemble_options(l,
				 "NONBLOCK",O_NONBLOCK,
				 "APPEND",O_APPEND,
				 "RDONLY",O_RDONLY,
				 "WRONLY",O_WRONLY,
				 "RDWR",O_RDWR,
				 "CREAT",O_CREAT,
				 "TRUNC",O_TRUNC,
				 "EXCL",O_EXCL,
				 NULL)));}

int get_fd(LISP ptr)
{if TYPEP(ptr,tc_c_file)
   return(fileno(get_c_file(ptr,NULL)));
 else
   return(get_c_long(ptr));}

LISP gsetlk(int op,LISP lfd,LISP ltype,LISP whence,LISP start,LISP len)
{struct flock f;
 int fd = get_fd(lfd);
 f.l_type = get_c_long(ltype);
 f.l_whence = NNULLP(whence) ? get_c_long(whence) : SEEK_SET;
 f.l_start = NNULLP(start) ? get_c_long(start) : 0;
 f.l_len = NNULLP(len) ? get_c_long(len) : 0;
 f.l_pid = 0;
 if (fcntl(fd,op,&f) == -1)
   return(llast_c_errmsg(-1));
 else if (op != F_GETLK)
   return(NIL);
 else if (f.l_type == F_UNLCK)
   return(NIL);
 else
   return(listn(2,flocons(f.l_type),flocons(f.l_pid)));}

LISP lF_SETLK(LISP fd,LISP ltype,LISP whence,LISP start,LISP len)
{return(gsetlk(F_SETLK,fd,ltype,whence,start,len));}

LISP lF_SETLKW(LISP fd,LISP ltype,LISP whence,LISP start,LISP len)
{return(gsetlk(F_SETLKW,fd,ltype,whence,start,len));}

LISP lF_GETLK(LISP fd,LISP ltype,LISP whence,LISP start,LISP len)
{return(gsetlk(F_GETLK,fd,ltype,whence,start,len));}


LISP delete_file(LISP fname)
{int iflag,ret;
 iflag = no_interrupt(1);
 ret = remove(get_c_string(fname));
 no_interrupt(iflag);
 if (ret)
   return(strcons(-1,last_c_errmsg(-1)));
 else
   return(NIL);}

LISP utime2str(LISP u)
{time_t bt;
 struct tm *btm;
 char sbuff[100];
 bt = get_c_long(u);
 if ((btm = localtime(&bt)))
   {sprintf(sbuff,"%04d%02d%02d%02d%02d%02d%02d",
	    btm->tm_year+1900,btm->tm_mon + 1,btm->tm_mday,
	    btm->tm_hour,btm->tm_min,btm->tm_sec,0);
    return(strcons(strlen(sbuff),sbuff));}
 else
   return(NIL);}


LISP lgetenv(LISP var)
{char *str;
 if ((str = getenv(get_c_string(var))))
   return(strcons(strlen(str),str));
 else
   return(NIL);}

LISP unix_time(void)
{return(flocons(time(NULL)));}

LISP unix_ctime(LISP value)
{time_t b;
 char *buff,*p;
 if NNULLP(value)
   b = get_c_long(value);
 else
   time(&b);
 if ((buff = ctime(&b)))
   {if ((p = strchr(buff,'\n'))) *p = 0;
    return(strcons(strlen(buff),buff));}
 else
   return(NIL);}

LISP http_date(LISP value)
     /* returns the internet standard RFC 1123 format */
{time_t b;
 char buff[256];
 struct tm *t;
 if NNULLP(value)
   b = get_c_long(value);
 else
   time(&b);
 if (!(t = gmtime(&b))) return(NIL);
 (sprintf
  (buff,"%s, %02d %s %04d %02d:%02d:%02d GMT",
   &"Sun\0Mon\0Tue\0Wed\0Thu\0Fri\0Sat"[t->tm_wday*4],
   t->tm_mday,
   &"Jan\0Feb\0Mar\0Apr\0May\0Jun\0Jul\0Aug\0Sep\0Oct\0Nov\0Dec"[t->tm_mon*4],
   t->tm_year+1900,
   t->tm_hour,
   t->tm_min,
   t->tm_sec));
 return(strcons(strlen(buff),buff));}

/* FIXME: what makes this osf-only? */
#if defined(__osf__)

LISP http_date_parse(LISP input)
     /* handle RFC 822, RFC 850, RFC 1123 and the ANSI C ascitime() format */
{struct tm tm,*lc;
 time_t t;
 int gmtoff;
 char *str = get_c_string(input),*format;
 t = time(NULL);
 if (lc = localtime(&t))
   gmtoff = lc->tm_gmtoff;
 if (strchr(str,',') && strchr(str,'-')) 
   /* rfc-850: Sunday, 06-Nov-94 08:49:37 GMT */
   format = "%a, %d-%b-%y %H:%M:%S GMT";
 else if (strchr(str,','))
   /* rfc-1123: Sun, 06 Nov 1994 08:49:37 GMT */
   format = "%a, %d %b %Y %H:%M:%S GMT";
 else
   /* ascitime: Sun Nov  6 08:49:37 1994 */
   {format = "%c";
    gmtoff = 0;}
 if (strptime(str,format,&tm))
   {t = mktime(&tm);
    /* unfortunately there is no documented way to tell mktime
       to assume GMT. Except for saving the value of the current
       timezone, setting TZ to GMT, doing a tzset() then doing
       our mktime() followed by setting the time zone back to the way
       it was before. That is fairly horrible, so instead we work around
       this by adding the gmtoff we computed above, which of course may
       have changed since we computed it (if the system mananager switched
       daylight savings time modes, for example).
       There is an executable /usr/lib/mh/dp which is presumably
       doing the same sort of thing, although perhaps it uses tzset */
    return(flocons(t + gmtoff));}
 else
   return(NIL);}

#endif


LISP lsleep(LISP ns)
{double val = get_c_double(ns);
#ifdef HAVE_USLEEP
 usleep((unsigned int)(val * 1.0e6));
#else
 sleep((unsigned int)val);
#endif
 return(NIL);}

LISP url_encode(LISP in)
{int spaces=0,specials=0,regulars=0,c;
 char *str = get_c_string(in),*p,*r;
 LISP out;
 for(p=str,spaces=0,specials=0,regulars=0;(c = *p);++p)
   if (c == ' ') ++spaces;
   else if (!(isalnum(c) || strchr("*-._@",c))) ++specials;
   else ++regulars;
 if ((spaces == 0) && (specials == 0))
   return(in);
 out = strcons(spaces + regulars + specials * 3,NULL);
 for(p=str,r=get_c_string(out);(c = *p);++p)
   if (c == ' ')
     *r++ = '+';
   else if (!(isalnum(c) || strchr("*-._@",c)))
     {sprintf(r,"%%%02X",c & 0xFF);
      r += 3;}
   else
     *r++ = c;
 *r = 0;
 return(out);}

LISP url_decode(LISP in)
{int pluses=0,specials=0,regulars=0,c,j;
 char *str = get_c_string(in),*p,*r;
 LISP out;
 for(p=str,pluses=0,specials=0,regulars=0;(c = *p);++p)
   if (c == '+') ++pluses;
   else if (c == '%')
     {if (isxdigit(p[1]) && isxdigit(p[2]))
	++specials;
      else
	return(NIL);}
   else
     ++regulars;
 if ((pluses == 0) && (specials == 0))
   return(in);
 out = strcons(regulars + pluses + specials,NULL);
 for(p=str,r=get_c_string(out);(c = *p);++p)
   if (c == '+')
     *r++ = ' ';
   else if (c == '%')
     {for(*r = 0,j=1;j<3;++j)
	*r = *r * 16 + ((isdigit(p[j]))
			? (p[j] - '0')
			: (toupper(p[j]) - 'A' + 10));
      p += 2;
      ++r;}
   else
     *r++ = c;
 *r = 0;
 return(out);}

LISP html_encode(LISP in)
{long j,n,m;
 char *str,*ptr;
 LISP out;
 switch(TYPE(in))
   {case tc_string:
    case tc_symbol:
      break;
    default:
      return(in);}
 str = get_c_string(in);
 n = strlen(str);
 for(j=0,m=0;j < n; ++j)
   switch(str[j])
     {case '>':
      case '<':
	m += 4;
	break;
      case '&':
	m += 5;
	break;
      default:
	++m;}
 if (n == m) return(in);
 out = strcons(m,NULL);
 for(j=0,ptr=get_c_string(out);j < n; ++j)
   switch(str[j])
     {case '>':
	strcpy(ptr,"&gt;");
	ptr += strlen(ptr);
	break;
      case '<':
	strcpy(ptr,"&lt;");
	ptr += strlen(ptr);
	break;
      case '&':
	strcpy(ptr,"&amp;");
	ptr += strlen(ptr);
	break;
      default:
	*ptr++ = str[j];}
 return(out);}

LISP html_decode(LISP in)
{return(in);}

LISP lgets(LISP file,LISP buffn)
{FILE *f;
 int iflag;
 long n;
 char buffer[2048],*ptr;
 f = get_c_file(file,stdin);
 if NULLP(buffn)
   n = sizeof(buffer);
 else if ((n = get_c_long(buffn)) < 0)
   err("size must be >= 0",buffn);
 else if (n > sizeof(buffer))
   err("not handling buffer of size",listn(2,buffn,flocons(sizeof(buffer))));
 iflag = no_interrupt(1);
 if ((ptr = fgets(buffer,n,f)))
   {no_interrupt(iflag);
    return(strcons(strlen(buffer),buffer));}
 no_interrupt(iflag);
 return(NIL);}

LISP lreadline(LISP file)
{LISP result;
 char *start,*ptr;
 result = lgets(file,NIL);
 if NULLP(result) return(NIL);
 start = get_c_string(result);
 if ((ptr = strchr(start,'\n')))
   {*ptr = 0;
    /* we also change the dim, because otherwise our equal? function
       is confused. What we really need are arrays with fill pointers. */
    result->storage_as.string.dim = ptr - start;
    return(result);}
 else
   /* we should be doing lgets until we  get a string with a newline or NIL,
      and then append the results */
   return(result);}


LISP l_chown(LISP path,LISP uid,LISP gid)
{long iflag;
 iflag = no_interrupt(1);
 if (chown(get_c_string(path),get_c_long(uid),get_c_long(gid)))
   err("chown",cons(path,llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}


#ifdef HAVE_LCHOWN
LISP l_lchown(LISP path,LISP uid,LISP gid)
{long iflag;
 iflag = no_interrupt(1);
 if (lchown(get_c_string(path),get_c_long(uid),get_c_long(gid)))
   err("lchown",cons(path,llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(NIL);}
#endif


LISP popen_l(LISP name,LISP how)
{return(fopen_cg(popen,
		 get_c_string(name),
		 NULLP(how) ? "r" : get_c_string(how)));}

/* note: if the user fails to call pclose then the gc is going
         to utilize fclose, which can result in a <defunct>
	 process laying around. However, we don't want to
	 modify file_gc_free nor add a new datatype.
	 So beware.
	 */
LISP pclose_l(LISP ptr)
{FILE *f = get_c_file(ptr,NULL);
 long iflag = no_interrupt(1);
 int retval,xerrno;
 retval = pclose(f);
 xerrno = errno;
 ptr->storage_as.c_file.f = (FILE *) NULL;
 free(ptr->storage_as.c_file.name);
 ptr->storage_as.c_file.name = NULL;
 no_interrupt(iflag);
 if (retval < 0)
   err("pclose",llast_c_errmsg(xerrno));
 return(flocons(retval));}



LISP siod_lib_l(void)
{return(rintern(siod_lib));}


LISP ccall_catch_1(LISP (*fcn)(void *),void *arg)
{LISP val;
 val = (*fcn)(arg);
 catch_framep = catch_framep->next;
 return(val);}

LISP ccall_catch(LISP tag,LISP (*fcn)(void *),void *arg)
{struct catch_frame frame;
 int k;
 frame.tag = tag;
 frame.next = catch_framep;
 k = setjmp(frame.cframe);
 catch_framep = &frame;
 if (k == 2)
   {catch_framep = frame.next;
    return(frame.retval);}
 return(ccall_catch_1(fcn,arg));}

LISP decode_tm(struct tm *t)
{return(symalist("sec",flocons(t->tm_sec),
		 "min",flocons(t->tm_min),
		 "hour",flocons(t->tm_hour),
		 "mday",flocons(t->tm_mday),
		 "mon",flocons(t->tm_mon),
		 "year",flocons(t->tm_year),
		 "wday",flocons(t->tm_wday),
		 "yday",flocons(t->tm_yday),
		 "isdst",flocons(t->tm_isdst),
/* FIXME: check for record fields, not system */
#if defined(__osf__)
		 "gmtoff",flocons(t->__tm_gmtoff),
		 "tm_zone",(t->__tm_zone) ? rintern(t->__tm_zone) : NIL,
#endif
		 NULL));}

LISP symalist(char *arg,...)
{va_list args;
 LISP result,l,val;
 char *key;
 if (!arg) return(NIL);
 va_start(args,arg);
 val = va_arg(args,LISP);
 result = cons(cons(cintern(arg),val),NIL);
 l = result;
 while((key = va_arg(args,char *)))
   {val = va_arg(args,LISP);
    CDR(l) = cons(cons(cintern(key),val),NIL);
    l = CDR(l);}
 va_end(args);
 return(result);}

void encode_tm(LISP alist,struct tm *t)
{LISP val;
 val = cdr(assq(cintern("sec"),alist));
 t->tm_sec = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("min"),alist));
 t->tm_min = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("hour"),alist));
 t->tm_hour = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("mday"),alist));
 t->tm_mday = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("mon"),alist));
 t->tm_mon = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("year"),alist));
 t->tm_year = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("wday"),alist));
 t->tm_wday = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("yday"),alist));
 t->tm_yday = NULLP(val) ? 0 : get_c_long(val);
 val = cdr(assq(cintern("isdst"),alist));
 t->tm_isdst = NULLP(val) ? -1 : get_c_long(val);
#if defined(__osf__)
 val = cdr(assq(cintern("gmtoff"),alist));
 t->__tm_gmtoff = NULLP(val) ? 0 : get_c_long(val);
#endif
}

LISP llocaltime(LISP value)
{time_t b;
 struct tm *t;
 if NNULLP(value)
   b = get_c_long(value);
 else
   time(&b);
 if ((t = localtime(&b)))
   return(decode_tm(t));
 else
   return(err("localtime",llast_c_errmsg(-1)));}

LISP lgmtime(LISP value)
{time_t b;
 struct tm *t;
 if NNULLP(value)
   b = get_c_long(value);
 else
   time(&b);
 if ((t = gmtime(&b)))
   return(decode_tm(t));
 else
   return(err("gmtime",llast_c_errmsg(-1)));}

LISP ltzset(void)
{tzset();
 return(NIL);}

LISP lmktime(LISP alist)
{struct tm tm;
 time_t t;
 encode_tm(alist,&tm);
 t = mktime(&tm);
 return(flocons(t));}

#if defined(HAVE_STRPTIME)

LISP lstrptime(LISP str,LISP fmt,LISP in)
{struct tm tm;
 encode_tm(in,&tm);
 if (strptime(get_c_string(str),get_c_string(fmt),&tm))
   return(decode_tm(&tm));
 else
   return(NIL);}

#endif


LISP lstrftime(LISP fmt,LISP in)
{struct tm tm;
 time_t b;
 struct tm *t;
 size_t ret;
 char buff[1024];
 if NNULLP(in)
   {encode_tm(in,&tm);
    t = &tm;}
 else
   {time(&b);
    if (!(t = gmtime(&b)))
      return(NIL);}
 if ((ret = strftime(buff,sizeof(buff),get_c_string(fmt),t)))
   return(strcons(ret,buff));
 else
   return(NIL);}


LISP lchdir(LISP dir)
{long iflag;
 FILE *f;
 int fd;
 char *path;
 switch(TYPE(dir))
   {case tc_c_file:
      f = get_c_file(dir,NULL);
      fd = fileno(f);
      iflag = no_interrupt(1);
      if (fchdir(fd))
	return(err("fchdir",llast_c_errmsg(-1)));
      no_interrupt(iflag);
      return(NIL);
    default:
      path = get_c_string(dir);
      iflag = no_interrupt(1);
      if (chdir(path))
	return(err("chdir",llast_c_errmsg(-1)));
      no_interrupt(iflag);
      return(NIL);}}


LISP lgetpass(LISP lprompt)
{long iflag;
 char *result;
 iflag = no_interrupt(1);
 result = getpass(NULLP(lprompt) ? "" : get_c_string(lprompt));
 no_interrupt(iflag);
 if (result)
   return(strcons(strlen(result),result));
 else
   return(NIL);}

LISP lpipe(void)
{int filedes[2];
 long iflag;
 LISP f1,f2;
 f1 = cons(NIL,NIL);
 f2 = cons(NIL,NIL);
 iflag = no_interrupt(1);
 if (pipe(filedes) == 0)
   {f1->type = tc_c_file;
    f1->storage_as.c_file.f = fdopen(filedes[0],"r");
    f2->type = tc_c_file;
    f2->storage_as.c_file.f = fdopen(filedes[1],"w");
    no_interrupt(iflag);
    return(listn(2,f1,f2));}
 else
   return(err("pipe",llast_c_errmsg(-1)));}

#define CTYPE_FLOAT   1
#define CTYPE_DOUBLE  2
#define CTYPE_CHAR    3
#define CTYPE_UCHAR   4
#define CTYPE_SHORT   5
#define CTYPE_USHORT  6
#define CTYPE_INT     7
#define CTYPE_UINT    8
#define CTYPE_LONG    9
#define CTYPE_ULONG  10

LISP err_large_index(LISP ind)
{return(err("index too large",ind));}

LISP datref(LISP dat,LISP ctype,LISP ind)
{char *data;
 long size,i;
 data = get_c_string_dim(dat,&size);
 i = get_c_long(ind);
 if (i < 0) err("negative index",ind);
 switch(get_c_long(ctype))
   {case CTYPE_FLOAT:
      if (((i+1) * (int) sizeof(float)) > size) err_large_index(ind);
      return(flocons(((float *)data)[i]));
    case CTYPE_DOUBLE:
      if (((i+1) * (int) sizeof(double)) > size) err_large_index(ind);
      return(flocons(((double *)data)[i]));
    case CTYPE_LONG:
      if (((i+1) * (int) sizeof(long)) > size) err_large_index(ind);
      return(flocons(((long *)data)[i]));
    case CTYPE_SHORT:
      if (((i+1) * (int) sizeof(short)) > size) err_large_index(ind);
      return(flocons(((short *)data)[i]));
    case CTYPE_CHAR:
      if (((i+1) * (int) sizeof(char)) > size) err_large_index(ind);
      return(flocons(((char *)data)[i]));
    case CTYPE_INT:
      if (((i+1) * (int) sizeof(int)) > size) err_large_index(ind);
      return(flocons(((int *)data)[i]));
    case CTYPE_ULONG:
      if (((i+1) * (int) sizeof(unsigned long)) > size) err_large_index(ind);
      return(flocons(((unsigned long *)data)[i]));
    case CTYPE_USHORT:
      if (((i+1) * (int) sizeof(unsigned short)) > size) err_large_index(ind);
      return(flocons(((unsigned short *)data)[i]));
    case CTYPE_UCHAR:
      if (((i+1) * (int) sizeof(unsigned char)) > size) err_large_index(ind);
      return(flocons(((unsigned char *)data)[i]));
    case CTYPE_UINT:
      if (((i+1) * (int) sizeof(unsigned int)) > size) err_large_index(ind);
      return(flocons(((unsigned int *)data)[i]));
    default:
      return(err("unknown CTYPE",ctype));}}

LISP sdatref(LISP spec,LISP dat)
{return(datref(dat,car(spec),cdr(spec)));}

LISP mkdatref(LISP ctype,LISP ind)
{return(closure(cons(ctype,ind),
		leval(cintern("sdatref"),NIL)));}

LISP datlength(LISP dat,LISP ctype)
{char *data;
 long size;
 data = get_c_string_dim(dat,&size);
 switch(get_c_long(ctype))
   {case CTYPE_FLOAT:
      return(flocons(size / sizeof(float)));
    case CTYPE_DOUBLE:
      return(flocons(size / sizeof(double)));
    case CTYPE_LONG:
      return(flocons(size / sizeof(long)));
    case CTYPE_SHORT:
      return(flocons(size / sizeof(short)));
    case CTYPE_CHAR:
      return(flocons(size / sizeof(char)));
    case CTYPE_INT:
      return(flocons(size / sizeof(int)));
    case CTYPE_ULONG:
      return(flocons(size / sizeof(unsigned long)));
    case CTYPE_USHORT:
      return(flocons(size / sizeof(unsigned short)));
    case CTYPE_UCHAR:
      return(flocons(size / sizeof(unsigned char)));
    case CTYPE_UINT:
      return(flocons(size / sizeof(unsigned int)));
    default:
      return(err("unknown CTYPE",ctype));}}

void init_slibu(void)
{long j;
 char *tmp1,*tmp2;
 tc_opendir = allocate_user_tc();
 set_gc_hooks(tc_opendir,
	      NULL,
	      NULL,
	      NULL,
	      opendir_gc_free,
	      &j);
 set_print_hooks(tc_opendir,opendir_prin1);

 gc_protect_sym(&sym_channels,"*channels*");
 setvar(sym_channels,NIL,NIL);
 init_lsubr("system",lsystem);
 init_subr_0("getgid",lgetgid);
 init_subr_0("getuid",lgetuid);
 init_subr_0("getcwd",lgetcwd);
 init_subr_2("crypt",lcrypt);
 init_subr_1("getpwuid",lgetpwuid);
 init_subr_1("getpwnam",lgetpwnam);
 init_subr_0("getpwent",lgetpwent);
 init_subr_0("setpwent",lsetpwent);
 init_subr_0("endpwent",lendpwent);
#if defined(HAVE_SETPWFILE)
 init_subr_1("setpwfile",lsetpwfile);
#endif
#if defined(HAVE_PUTPWENT)
 init_subr_2("putpwent",lputpwent);
#endif
 init_subr_2("access-problem?",laccess_problem);
 init_subr_3("utime",lutime);
 init_subr_2("chmod",l_chmod);
 init_subr_2("fchmod",lfchmod);
 init_subr_1("random",lrandom);
 init_subr_1("srandom",lsrandom);
 init_subr_1("first",car);
 init_subr_1("rest",cdr);
 init_subr_0("fork",lfork);
 init_subr_3("exec",lexec);
 init_subr_1("nice",lnice);
 init_subr_2("wait",lwait);
 init_subr_0("getpgrp",lgetpgrp);
 init_subr_1("getgrgid",lgetgrgid);
 init_subr_2("kill",lkill);
 init_subr_1("%%%memref",lmemref_byte);
 init_subr_0("getpid",lgetpid);
 init_subr_0("getppid",lgetppid);
 init_subr_1("exit",lexit);
 init_subr_1("trunc",ltrunc);
 init_subr_1("putenv",lputenv);
/*#ifdef MD5*/ /* butchery by Ulric */
 init_subr_0("md5-init",md5_init);
 init_subr_3("md5-update",md5_update);
 init_subr_1("md5-final",md5_final);
/*#endif*/ /* MD5 */
#if defined(HAVE_GETRLIMIT) && defined(RLIMIT_CPU)
 init_subr_2("cpu-usage-limits",cpu_usage_limits);
#endif
#if defined(HAVE_GETRUSAGE) && defined(RUSAGE_SELF) && defined(RUSAGE_CHILDREN)
 init_subr_1("current-resource-usage",current_resource_usage);
#endif
 init_subr_1("opendir",l_opendir);
 init_subr_1("closedir",l_closedir);
 init_subr_1("readdir",l_readdir);
 init_subr_1("delete-file",delete_file);
 init_subr_1("file-times",file_times);
 init_subr_1("unix-time->strtime",utime2str);
 init_subr_0("unix-time",unix_time);
 init_subr_1("unix-ctime",unix_ctime);
 init_subr_1("getenv",lgetenv);
 init_subr_1("sleep",lsleep);
 init_subr_1("url-encode",url_encode);
 init_subr_1("url-decode",url_decode);
 init_subr_2("gets",lgets);
 init_subr_1("readline",lreadline);
 init_subr_1("html-encode",html_encode);
 init_subr_1("html-decode",html_decode);
 init_subr_1("decode-file-mode",decode_st_mode);
 init_subr_1("encode-file-mode",encode_st_mode);
 init_subr_1("encode-open-flags",encode_open_flags);
 init_subr_1("stat",l_stat);
 init_subr_1("lstat",l_lstat);
 init_subr_1("fstat",l_fstat);
#if defined(HAVE_FNMATCH)
 init_subr_3("fnmatch",l_fnmatch);
#endif
 init_subr_2("symlink",lsymlink);
 init_subr_2("link",llink);
 init_subr_1("unlink",lunlink);
 init_subr_1("rmdir",lrmdir);
 init_subr_2("mkdir",lmkdir);
 init_subr_2("rename",lrename);
 init_subr_1("readlink",lreadlink);
 init_subr_3("chown",l_chown);
#if defined(HAVE_LCHOWN)
 init_subr_3("lchown",l_lchown);
#endif
 init_subr_1("http-date",http_date);
#if defined(__osf__)
 init_subr_1("http-date-parse",http_date_parse);
#endif
 init_subr_2("popen",popen_l);
 init_subr_1("pclose",pclose_l);


 setvar(cintern("SEEK_SET"),flocons(SEEK_SET),NIL);
 setvar(cintern("SEEK_CUR"),flocons(SEEK_CUR),NIL);
 setvar(cintern("SEEK_END"),flocons(SEEK_END),NIL);
 setvar(cintern("F_RDLCK"),flocons(F_RDLCK),NIL);
 setvar(cintern("F_WRLCK"),flocons(F_WRLCK),NIL);
 setvar(cintern("F_UNLCK"),flocons(F_UNLCK),NIL);
 init_subr_5("F_SETLK",lF_SETLK);
 init_subr_5("F_SETLKW",lF_SETLKW);
 init_subr_5("F_GETLK",lF_GETLK);

 init_subr_0("siod-lib",siod_lib_l);

 if ((!(tmp1 = getenv(ld_library_path_env))) ||
     (!strstr(tmp1,siod_lib)))
   {tmp2 = (char *) must_malloc(strlen(ld_library_path_env) + 1 +
				((tmp1) ? strlen(tmp1) + 1 : 0) +
				strlen(siod_lib) + 1);
    sprintf(tmp2,"%s=%s%s%s",
	    ld_library_path_env,
	    (tmp1) ? tmp1 : "",
	    (tmp1) ? ":" : "",
	    siod_lib);
    /* note that we cannot free the string afterwards. */
    putenv(tmp2);}
 init_subr_1("localtime",llocaltime);
 init_subr_1("gmtime",lgmtime);
 init_subr_0("tzset",ltzset);
 init_subr_1("mktime",lmktime);
 init_subr_1("chdir",lchdir);
#if defined(HAVE_STRPTIME)
 init_subr_3("strptime",lstrptime);
#endif
 init_subr_2("strftime",lstrftime);
 init_subr_1("getpass",lgetpass);
 init_subr_0("pipe",lpipe);
 init_subr_2("alarm",lalarm);

 setvar(cintern("CTYPE_FLOAT"),flocons(CTYPE_FLOAT),NIL);
 setvar(cintern("CTYPE_DOUBLE"),flocons(CTYPE_DOUBLE),NIL);
 setvar(cintern("CTYPE_LONG"),flocons(CTYPE_LONG),NIL);
 setvar(cintern("CTYPE_SHORT"),flocons(CTYPE_SHORT),NIL);
 setvar(cintern("CTYPE_CHAR"),flocons(CTYPE_CHAR),NIL);
 setvar(cintern("CTYPE_INT"),flocons(CTYPE_INT),NIL);
 setvar(cintern("CTYPE_ULONG"),flocons(CTYPE_ULONG),NIL);
 setvar(cintern("CTYPE_USHORT"),flocons(CTYPE_USHORT),NIL);
 setvar(cintern("CTYPE_UCHAR"),flocons(CTYPE_UCHAR),NIL);
 setvar(cintern("CTYPE_UINT"),flocons(CTYPE_UINT),NIL);
 init_subr_3("datref",datref);
 init_subr_2("sdatref",sdatref);
 init_subr_2("mkdatref",mkdatref);
 init_subr_2("datlength",datlength);

 init_slibu_version();}
