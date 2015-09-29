dnl Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([README])
AM_INIT_AUTOMAKE(siag, 3.6.1)
AM_CONFIG_HEADER(config.h)

dnl Set of available languages
dnl ALL_LINGUAS=""

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_RANLIB
AC_ISC_POSIX

dnl Checks for libraries.

dnl Checks for header files.
AC_PATH_XTRA
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_MAJOR


dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

AC_STRUCT_ST_BLOCKS

AC_HEADER_TIME
AC_STRUCT_TM
AC_CHECK_SIZEOF(unsigned char)
AC_CHECK_SIZEOF(unsigned short)
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(unsigned long long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
AC_C_BIGENDIAN

dnl Checks for library functions.
dnl Done elsewhere AC_FUNC_FNMATCH
AC_FUNC_GETPGRP
AC_FUNC_MEMCMP
AC_TYPE_SIGNAL
AC_FUNC_STRFTIME
AC_FUNC_UTIME_NULL
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(getcwd \
	gethostname \
	gettimeofday \
	mkdir \
	mktime \
	putenv \
	regcomp \
	rmdir \
	select \
	socket \
	strcspn \
	strdup \
	strerror \
	strspn \
	strstr \
	strtod \
	strtol)

dnl Check things for Siag
AC_CHECK_SIAG
dnl AC_CHECK_XMAGICK
dnl AC_CHECK_STOCKS

dnl Internationalization macros
dnl AM_GNU_GETTEXT

dnl And finally generate an incredible number of Makefiles...

AC_CONFIG_FILES([Makefile \
	siod/Makefile siod/docs/Makefile \
	cole/Makefile stocks/Makefile \
	common/Makefile common/docs/Makefile common/bitmaps/Makefile \
	common/bitmaps/kde/Makefile common/bitmaps/kde2/Makefile \
	common/bitmaps/gnome/Makefile \
	common/bitmaps/ms/Makefile \
	common/themes/Makefile \
	xcommon/Makefile \
	siag/Makefile siag/docs/Makefile siag/examples/Makefile \
	xsiag/Makefile \
	tsiag/Makefile \
	pw/Makefile pw/docs/Makefile pw/examples/Makefile \
	xpw/Makefile \
	egon/Makefile egon/docs/Makefile egon/examples/Makefile \
	xegon/Makefile \
	plugins/Makefile \
	xed/Makefile \
	xfiler/Makefile xfiler/pixmaps/Makefile xfiler/bitmaps/Makefile \
	o3read/Makefile gvu/Makefile])
AC_OUTPUT

