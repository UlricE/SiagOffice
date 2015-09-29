
AC_DEFUN(AC_CHECK_SIAG,
[
dnl We want to use gmatch where fnmatch is not available
AC_CHECK_FUNC(fnmatch, AC_DEFINE(HAVE_FNMATCH, 1, [#undef HAVE_FNMATCH]),
	[AC_CHECK_LIB(gen, gmatch)
	AC_CHECK_FUNC(gmatch, AC_DEFINE(HAVE_GMATCH, 1, [#undef HAVE_GMATCH]))])

dnl Somehow we must be able to set NDBM even if no libdb exists (Solaris)
dnl Try named lib first, then libdb, then libgdbm, then libndbm
define(siag_CHECK_NDBM,
[AC_CHECK_LIB($MAYBE_LIBNDBM, dbm_open, LIBDB=-l$MAYBE_LIBNDBM,
	AC_CHECK_LIB(db, dbm_open, LIBDB="-ldb",
		AC_CHECK_LIB(gdbm, dbm_open, LIBDB="-lgdbm",
			AC_CHECK_LIB(ndbm, dbm_open, LIBDB="-lndbm"))))
if test "$LIBDB" != ""; then
	echo "Found ndbm in $LIBDB"
	LIBS="$LIBDB $LIBS"
fi
AC_CHECK_FUNC(dbm_dirfno, AC_DEFINE(HAVE_DBM_DIRFNO, 1, [#undef HAVE_DBM_DIRFNO]))
AC_CHECK_FUNC(dbm_pagfno, AC_DEFINE(HAVE_DBM_PAGFNO, 1, [#undef HAVE_DBM_PAGFNO]))
AC_CHECK_FUNC(dbm_rdonly, AC_DEFINE(HAVE_DBM_RDONLY, 1, [#undef HAVE_DBM_RDONLY]))
AC_CHECK_FUNC(dbm_open, NDBM="-DNDBM")
])

AC_ARG_WITH(ndbm,
	[  --with-ndbm[=libname]     enable (named) ndbm library [default: gdbm]],
	[ case $withval in
	yes)
		MAYBE_LIBNDBM="gdbm"
		;;
	no)
		;;
	*)
		MAYBE_LIBNDBM=$withval
		;;
	esac
	
	if test "$withval" != "no"; then
		siag_CHECK_NDBM
	fi ],
dnl	[ MAYBE_LIBNDBM="gdbm"
dnl	siag_CHECK_NDBM ])
)

AC_SUBST(NDBM)

AC_CHECK_LIB(m, main)

dnl Check what math functions we have
AC_CHECK_FUNC(acosh, AC_DEFINE(HAVE_ACOSH, 1, [#undef HAVE_ACOSH]))
AC_CHECK_FUNC(asinh, AC_DEFINE(HAVE_ASINH, 1, [#undef HAVE_ASINH]))
AC_CHECK_FUNC(log1p, AC_DEFINE(HAVE_LOG1P, 1, [#undef HAVE_LOG1P]))
AC_CHECK_FUNC(expm1, AC_DEFINE(HAVE_EXPM1, 1, [#undef HAVE_EXPM1]))
AC_CHECK_FUNC(cbrt, AC_DEFINE(HAVE_CBRT, 1, [#undef HAVE_CBRT]))
AC_CHECK_FUNC(drem, AC_DEFINE(HAVE_DREM, 1, [#undef HAVE_DREM]))
AC_CHECK_FUNC(atanh, AC_DEFINE(HAVE_ATANH, 1, [#undef HAVE_ATANH]))

AC_CHECK_LIB(crypt, main, [LIBSYSDEP="-lcrypt"])
if test "$LIBSYSDEP" = ""; then
	AC_CHECK_LIB(posix4, main, [LIBSYSDEP="-lposix4"])
	if test "$LIBSYSDEP" = ""; then
		LIBSYSDEP=""
	fi
fi
AC_SUBST(LIBSYSDEP)

dnl These are necessary for SIOD on Solaris and probably Irix.
AC_CHECK_LIB(nsl, main)
AC_CHECK_LIB(socket, connect)

dnl SIOD
AC_CHECK_HEADERS(sys/mode.h fnmatch.h crypt.h sys/mkdev.h fcntl.h)

AC_CHECK_FUNCS(setpwfile getrlimit getrusage fnmatch \
		usleep lchown strptime putpwent getwd)

dnl Guile, Python, Ruby and Tcl

dnl Empty libdl in case we are making static binaries
dnl LIBDL=""
INTERPRETERS=""

AC_ARG_WITH(guile,
	[  --with-guile            enable guile commands and expressions],
	[ case $withval in
	yes)
		MAYBE_LIBGUILE="guile"
		;;
	no)
		;;
	*)
		MAYBE_LIBGUILE=$withval
		;;
	esac
	if test "$withval" != "no"; then
		AC_CHECK_LIB($MAYBE_LIBGUILE, gh_enter,
			[INTERPRETERS="-l$MAYBE_LIBGUILE `guile-config link` $INTERPRETERS"
			CFLAGS="`guile-config compile` $CFLAGS"
			AC_DEFINE(HAVE_LIBGUILE, 1, [#undef HAVE_LIBGUILE])],
			[echo "$MAYBE_LIBGUILE not found"
			echo "You need to visit http://www.guile.org/"
			exit 1],
			`guile-config link`)
	fi ],
)

AC_ARG_WITH(python,
	[  --with-python           enable python commands and expressions],
	[ case $withval in
	yes)
		MAYBE_LIBPYTHON="python2.2"
		;;
	no)
		;;
	*)
		MAYBE_LIBPYTHON=$withval
		;;
	esac
	if test "$withval" != "no"; then
		AC_CHECK_LIB(dl, main, INTERPRETERS="-ldl $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB(pthread, main, INTERPRETERS="-lpthread $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB(util, main, INTERPRETERS="-lutil $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB($MAYBE_LIBPYTHON, PyFloat_AsDouble,
			[INTERPRETERS="-l$MAYBE_LIBPYTHON $INTERPRETERS"
			AC_DEFINE(HAVE_LIBPYTHON, 1, [#undef HAVE_LIBPYTHON])],
			[echo "$MAYBE_LIBPYTHON not found"
			exit 1],
			$INTERPRETERS)
	fi ])

AC_ARG_WITH(ruby,
	[  --with-ruby             enable ruby commands and expressions],
	[ case $withval in
	yes)
		MAYBE_LIBRUBY="ruby"
		;;
	no)
		;;
	*)
		MAYBE_LIBRUBY=$withval
		;;
	esac
	if test "$withval" != "no"; then
		AC_CHECK_LIB(m, main, INTERPRETERS="-lm $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB(crypt, main, INTERPRETERS="-lcrypt $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB(dl, main, INTERPRETERS="-ldl $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB($MAYBE_LIBRUBY, ruby_init,
			[INTERPRETERS="-l$MAYBE_LIBRUBY $INTERPRETERS"
			AC_DEFINE(HAVE_LIBRUBY, 1, [#undef HAVE_LIBRUBY])],
			[echo Ruby not found
			exit 1],
			$INTERPRETERS)
	fi ])

AC_ARG_WITH(tcl,
	[  --with-tcl[=libname]      enable (named) tcl library [default: tcl]],
	[ case $withval in 
	yes)
		MAYBE_LIBTCL="tcl"
		;;
	no)
		;;
	*)
		MAYBE_LIBTCL=$withval
		;;
	esac

	if test "$withval" != "no"; then
		AC_CHECK_LIB(dl, main, INTERPRETERS="-ldl $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB(m, main, INTERPRETERS="-lm $INTERPRETERS", $INTERPRETERS)
		AC_CHECK_LIB($MAYBE_LIBTCL, Tcl_Main,
			[INTERPRETERS="-l$MAYBE_LIBTCL $INTERPRETERS"
			AC_DEFINE(HAVE_LIBTCL, 1, [#undef HAVE_LIBTCL])],
			[echo "$MAYBE_LIBTCL not found"
			exit 1],
			$INTERPRETERS)
	fi ],
)

AC_SUBST(INTERPRETERS)

AC_SUBST(MY_X_LIBS)
LDFLAGS=$ac_save_LDFLAGS
dnl AC_ARG_WITH(stocks,
dnl 	[  --with-stocks           enable libstocks (stock quotes over the Internet)],
dnl 	[ if test "$withval" != "no"; then
dnl 		AC_CHECK_LIB(stocks, main)
dnl 	fi ])

AC_ARG_WITH(gmp,
	[  --with-gmp              enable Gnu arbitrary precision arithmetic],
	[ if test "$withval" != "no"; then
		AC_CHECK_LIB(gmp, main)
	fi ])

AC_ARG_WITH(sdb,
	[  --with-sdb              enable database queries through LibSDB],
	[ if test "$withval" != "no"; then
		CFLAGS="$CFLAGS `sdb-config --cflags`"
		SDBLIBS="`sdb-config --libs`"
		AC_CHECK_LIB(sdb, sdb_init, LDFLAGS="$LDFLAGS $SDBLIBS", exit, $SDBLIBS)
		AC_DEFINE(HAVE_LIBSDB, 1, [#undef HAVE_LIBSDB])
	fi ])

dnl ccmath defines variables in ccmath.h, which causes AC_CHECK_LIB to fail
dnl so we roll our own
define(siag_CHECK_CCMATH,
[ac_save_LIBS=$LIBS
LIBS="-lccm $LIBS"
AC_TRY_LINK([#include <ccmath.h>], [main();], LIBCCMATH=-lccm, )
LIBS=$ac_save_LIBS
if test "$LIBCCMATH" != ""; then
	echo "Found ccmath in $LIBCCMATH"
	CCMATH=-DCCMATH
fi ])

AC_ARG_WITH(ccmath,
	[  --with-ccmath           enable the CCMATH mathematics library],
	[ if test "$withval" != "no"; then
		siag_CHECK_CCMATH
	fi ])
AC_SUBST(CCMATH)
AC_SUBST(LIBCCMATH)

dnl No Perl
LIBPERL=""
AC_SUBST(LIBPERL)
AC_SUBST(PERL)
AC_SUBST(PERLINC)

dnl X-specific checks
ac_save_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS $X_LIBS"
AC_CHECK_LIB(Xext, main,
[MY_X_LIBS="-lXext"
AC_DEFINE(HAVE_LIBXEXT, 1, [#undef HAVE_LIBXEXT])], ,
$X_PRE_LIBS $X_EXTRA_LIBS $X_EXTRA_LIBS -lXt -lX11)

AC_CHECK_LIB(Xmu, main,
[MY_X_LIBS="-lXmu $MY_X_LIBS"], ,
$MY_X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS $X_EXTRA_LIBS -lXt -lX11)
AC_CHECK_LIB(X11, XCreateIC, AC_DEFINE(HAVE_XCREATEIC, 1, [#undef HAVE_XCREATEIC]), , $x_libs)

AC_CHECK_LIB(Xpm, main,
[MY_X_LIBS="-lXpm $MY_X_LIBS"
AC_DEFINE(HAVE_LIBXPM, 1, [#undef HAVE_LIBXPM])],
[echo "Xpm library not found"
exit 1],
$MY_X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS $X_EXTRA_LIBS -lXt -lX11)

AC_ARG_WITH(xawm,
	[  --with-xawm             choose Xaw3d compatible library (default neXtaw)],
	[ case $withval in
	yes | no )
		XAWMLIB=XawM
		;;
	* )
		XAWMLIB=$withval
		;;
	esac ]
)

AC_ARG_WITH(xaw3d,
	[  --with-xaw3d            choose Xaw3d compatible library (default neXtaw)],
	[ case $withval in
	yes | no )
		XAW3DLIB=neXtaw
		;;
	* )
		XAW3DLIB=$withval
		;;
	esac ]
)

if test ! -z "$XAW3DLIB"; then
	XAWLIB="$XAW3DLIB"
elif test ! -z "$XAWMLIB"; then
	XAWLIB="$XAWMLIB"
else
	XAWLIB="neXtaw"
fi

AC_CHECK_LIB($XAWLIB, main,
[MY_X_LIBS="-l$XAWLIB $MY_X_LIBS"],
[echo "$XAWLIB library not found"
echo "Read INSTALL for guidance"
exit 1],
$MY_X_LIBS $X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS $X_EXTRA_LIBS -lXt -lX11)

AC_CHECK_LIB(Mowitz, main,
[MY_X_LIBS="-lMowitz $MY_X_LIBS"
AC_DEFINE(HAVE_LIBMOWITZ, 1, [#undef HAVE_LIBMOWITZ])],
[echo "Mowitz library not found"
echo "You need to visit http://siag.nu/mowitz/"
exit 1],
$MY_X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS $X_EXTRA_LIBS -lXt -lX11)

AC_SUBST(MY_X_LIBS)
LDFLAGS=$ac_save_LDFLAGS

if test "$GCC" = "yes"; then
	CFLAGS="-Wall $CFLAGS"
fi

AC_ARG_ENABLE(debugging,
	[  --enable-debugging      enable debugging],
	[ if test "$withval" != "no"; then
		CFLAGS="$CFLAGS -g"
	fi ])

AC_ARG_ENABLE(profiling,
	[  --enable-profiling      enable profiling],
	[ if test "$withval" != "no"; then
		CFLAGS="$CFLAGS -pg"
	fi ])

KDEINST='${top_srcdir}/common/kdeinst'
AC_ARG_ENABLE(kdeinst,
	[  --disable-kdeinst       disable installation of KDE entries],
	[ case $enableval in
	yes)
		;;
	no)
		KDEINST=": $KDEINST"
		;;
	*)
		KDEINST=$enableval
		;;
	esac ])
AC_SUBST(KDEINST)

AC_CHECK_FUNC(vsnprintf, AC_DEFINE(HAVE_VSNPRINTF, 1, [#undef HAVE_VSNPRINTF]))

dnl What curses lib is available? Try in this order:
dnl 0. use termcap if available
dnl 1. ncurses
dnl 2. curses
ac_save_LDFLAGS="$LDFLAGS"
AC_CHECK_LIB(termcap, main, LIBTERMCAP="-ltermcap")
AC_CHECK_LIB(ncurses, wgetch, LIBCURSES="-lncurses $LIBTERMCAP",
	AC_CHECK_LIB(curses, wgetch, LIBCURSES="-lcurses $LIBTERMCAP", , $LIBTERMCAP),
	$LIBTERMCAP)
LDFLAGS="$LDFLAGS $LIBCURSES"
AC_CHECK_FUNCS(resizeterm beep immedok keypad)
AC_SUBST(LIBCURSES)
LDFLAGS=$ac_save_LDFLAGS

dnl Look for T1lib including its X part
AC_ARG_WITH(t1lib,
	[  --with-t1lib            enable t1lib],
	[ if test "$withval" != "no"; then
		AC_CHECK_LIB(t1, main, LIBT1="-lt1")
		if test "$LIBT1" != ""; then
			AC_DEFINE(HAVE_LIB_T1, 1, [#undef HAVE_LIB_T1])
		fi
		AC_CHECK_LIB(t1x, main, LIBT1X="-lt1x", , $LIBT1 $X_LIBS -lX11)
		if test "$LIBT1X" != ""; then
			AC_DEFINE(HAVE_LIB_T1X, 1, [#undef HAVE_LIB_T1X])
		fi
	fi ])
AC_SUBST(LIBT1)
AC_SUBST(LIBT1X)

dnl Set NARROWPROTO
case `$ac_config_guess` in
*freebsd*|*gnu*|*irix5*|*irix6*|*linux-gnu*|*netbsd*|*openbsd*)
	CFLAGS="-DNARROWPROTO=1 $CFLAGS"
esac

docdir='${prefix}/doc/siag'
AC_ARG_WITH(docdir,
	[  --with-docdir=DIR       install docs in DIR [[PREFIX/doc/siag]]],
	[ if test "$withval" != "yes" && test "$withval" != "no"; then
		docdir=$withval
	fi ])
AC_SUBST(docdir)
])

