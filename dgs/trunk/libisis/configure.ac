dnl====================================================================
dnl  
dnl  Autoconf (configure script) generation file
dnl  
dnl  $Id: configure.ac 1444 2008-12-22 10:51:11Z Freddie Akeroyd $
dnl
dnl  Copyright (C) 2004 Freddie Akeroyd
dnl
dnl                       -*- Autoconf -*-
dnl Process this file with autoconf to produce a configure script.
dnl


AC_PREREQ(2.52)
AC_REVISION($Revision: 1444 $)
AC_INIT([LIBISIS Library], [1.0.0], [libisis-developers@libisis.org], [libisis])
AC_COPYRIGHT([Copyright (C) 2004 Freddie Akeroyd 
This software is covered by the GNU LESSER GENERAL PUBLIC LICENSE
see file COPYING for further information])

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_SRCDIR([libcore/IXMlibcore.f90])
AC_CANONICAL_TARGET

dnl $EGREP is used in macros included from acinclude.m4
AC_PROG_EGREP

AM_INIT_AUTOMAKE([1.6])

AM_CONFIG_HEADER([libisisconfig.h])

dnl libisis static libraries do not work on mac
case $host in
    *darwin*)
	enable_static=no
        ;;
esac

dnl Checks for programs.
AC_ARG_ENABLE([optimise], AC_HELP_STRING([--enable-optimise], [Turn on optimisation]), [enable_optimise=$enableval],[enable_optimise=no])
AC_ARG_ENABLE([profiling], AC_HELP_STRING([--enable-profiling], [Turn on execution profiling]), [enable_profiling=$enableval],[enable_profiling=no])

AC_ARG_WITH([cc], AC_HELP_STRING([--with-cc=c compiler], [Specify name of C compiler]),
	[], [with_cc=yes])
if test x"$with_cc" != x"no" ; then
	if test x"$with_cc" = x"yes" ; then
		AC_PROG_CC
	else
		AC_CHECK_PROGS(CC, ["$with_cc"])
	fi
	dnl Check C compiler options
	dnl -no-cpp-precomp needed on MacOS-X (and others?)
	AC_CHECK_C_OPTION([-no-cpp-precomp])
	AC_CHECK_C_OPTION([-fno-common])
else
	AC_MSG_ERROR([You need a C compiler to compile this package])
fi

AC_ARG_WITH([f77], AC_HELP_STRING([--with-f77=f77 compiler], [Specify name of FORTRAN 77 compiler]),
	[], [with_f77=yes])
if test x"$with_f77" != x"no" ; then
	if test x"$with_f77" = x"yes" ; then
		AC_PROG_F77
	else
		AC_CHECK_PROGS(F77, ["$with_f77"])
	fi
	dnl Check FORTRAN compiler options
	dnl 	AC_CHECK_F77_OPTION([-Wno-globals])
	dnl 	AC_CHECK_F77_OPTION([-fno-common])  # use with caution
	mytop=`pwd`
	dnl get right F77 include option
	AC_CHECK_F77_OPTION([-I$mytop/bindings/f77],[      INCLUDE 'napif.inc'])
	AC_F77_LIBRARY_LDFLAGS
else
	F77=
fi

AC_ARG_WITH([f90], AC_HELP_STRING([--with-f90=f90 compiler], [Specify name of FORTRAN 90 compiler]),
	[], [with_f90=yes])
if test x"$with_f90" != x"no" ; then
	if test x"$with_f90" = x"yes" ; then
#		AC_PROG_FC
		AC_CHECK_PROGS(FC, [ifort g95])
	else
		AC_CHECK_PROGS(FC, ["$with_f90"])
	fi
	dnl f90 has a "module path" option - hard to get as you really need a
	dnl ready compiled module to point the option at so for now we are just
	dnl check that the option takes a directory and the compiler works
	mytop=`pwd`
#	FCFLAGS="$FCFLAGS -g -O0 -fimplicit-none -fsloppy-char -fbounds-check -fmodule-private -I$mytop/libcore -I$mytop/libclasses"
#	FCFLAGS="$FCFLAGS -g -O0 -fsloppy-char -fbounds-check -I$mytop/libcore -I$mytop/libclasses"
#	FCFLAGS="$FCFLAGS -fpic -implicitnone -check all -automatic -traceback -warn all -warn nounused"
	AC_FC_LIBRARY_LDFLAGS
else
	FC=
fi

AC_ARG_WITH([matlabroot], AC_HELP_STRING([--with-matlabroot=path to matlab install dir], [Specify location of matlab installation directory]),
	[], [with_matlabroot=no])
if test x"$with_matlabroot" != x"no" ; then
    MATLAB_ROOT="$with_matlabroot"
fi
AC_PATH_PROG(MATLAB,matlab,[],[$MATLAB_ROOT/bin:/usr/local/matlab/bin:/opt/matlab/bin:$PATH])
if test x"$MATLAB_ROOT" = x""; then
    MATLAB_ROOT=`dirname $MATLAB`/..
fi

libisisdir='${prefix}/libisis'
libisislibdir='$(libisisdir)/lib'
libisisbindir='$(libisisdir)/bin'
libisismatlabdir='$(libisisdir)/matlab'
libisisdocdir='$(libisisdir)/doc'
AC_SUBST(libisisdir)
AC_SUBST(libisisbindir)
AC_SUBST(libisislibdir)
AC_SUBST(libisismatlabdir)
AC_SUBST(libisisdocdir)

CFLAGS="$CFLAGS -Wall -pthread -fPIC -fexceptions -ansi"
if test "$enable_optimise" = "yes"; then
    CFLAGS="$CFLAGS -g -O2"
else
    CFLAGS="$CFLAGS -g -O0 -DLIBISIS_DEBUG=1"
    FCFLAGS="$FCFLAGS -DLIBISIS_DEBUG=1"
fi
if test "$enable_profiling" = "yes"; then
    CFLAGS="$CFLAGS -pg"
fi

AC_SUBST(FC_LOAD_PATH)
if test "$FC" = "g95"; then
	FCFLAGS="$FCFLAGS -fimplicit-none -fexceptions -fPIC -Wall -cpp -pthread -DBYTERECL=1 -ffree-line-length-huge" # -fmodule-private
	if test "$enable_optimise" = "yes"; then
	    FCFLAGS="$FCFLAGS -g -O2"
	else
	    FCFLAGS="$FCFLAGS -g -O0 -fbounds-check -ftrace=full -freal=nan -finteger=-99999 -fpointer=invalid" # -Wextra
	fi
	if test "$enable_profiling" = "yes"; then
	    FCFLAGS="$FCFLAGS -pg"
	fi
	FC_LOAD_PATH=/usr/lib
fi
if test "$FC" = "gfortran"; then
	FCFLAGS="$FCFLAGS -DBYTERECL=1 -x f95-cpp-input -ffree-line-length-none -fimplicit-none -fexceptions -fPIC -Wall -g -O0 -fbounds-check -pthread"
	FC_LOAD_PATH=/usr/lib
fi
if test "$FC" = "ifort"; then
# Took off -no-cpprt as seems to cause problems with libunwind
	FCFLAGS="$FCFLAGS -assume 2underscores -implicitnone -auto-scalar -fPIC -cpp -threads -nofor_main"
	if test "$enable_optimise" = "yes"; then
	    FCFLAGS="$FCFLAGS -O2"
	else
	    FCFLAGS="$FCFLAGS -traceback -check bounds -check format -ftrapuv -g -O0 -debug extended"
	fi
	if test "$enable_profiling" = "yes"; then
	    FCFLAGS="$FCFLAGS -pg"
	fi
        i=`which ifort`
	FC_LOAD_PATH=`dirname $i`/../lib
fi
if test ! -z "$FC"; then
    fcpathlc=`which $FC 2>/dev/null | tr 'A-Z' 'a-z'`
    fcname=`basename $FC`
    case "$fcpathlc" in
        *sun*)
            echo "SUN f95 compiler detected - adjusting FCLAGS"
            FCFLAGS="$FCFLAGS -g -fpp  -M$mytop/libcore -M$mytop/libclasses"
            ;;
    esac
fi

java_host="linux"
javaroot=""
AC_ARG_WITH([java-home], AC_HELP_STRING([--with-java-home=JAVA SDK Home], [Specify location of top directory of JAVA SDK]),
	[], [with_java_home=$withval])
if test -d "$with_java_home"; then
	javaroot="$with_java_home"
fi


AC_ARG_WITH([java], AC_HELP_STRING([--with-java=java compiler], [Specify name of Java compiler]),
	[], [with_java=yes])
if test x$with_java != xno ; then
	if test -z "$javaroot"; then
	    javapath=`which java`
	    if test ! -z "$javapath"; then javaroot=`dirname $javapath`/..; fi
	fi

AC_ARG_WITH([doxygen], AC_HELP_STRING([--with-doxygen=path to doxygen executable], [Specify name of fortran aware doxygen executable]),
	[if test "$withval" != no; then DOXYGEN="$withval"; fi])

dnl	if test -z "$javaroot"; then 
dnl	    AC_MSG_ERROR([Unable to determine JAVA SDK directory - please specify this using  --with-java-home=/sdk/directory  or instead use  --without-java])
dnl	fi
	AC_MSG_NOTICE([Java SDK home is $javaroot])
	if test x"$with_java" = x"yes" ; then
		AC_PATH_PROGS(JAVAC,javac,,[$PATH:$javaroot/bin])
	else
		AC_PATH_PROGS(JAVAC,["$with_java"],,[$PATH:$javaroot/bin])
	fi
	AC_PATH_PROGS(JAVADOC,javadoc,,[$PATH:$javaroot/bin])
	AC_PATH_PROGS(JAR,jar,,[$PATH:$javaroot/bin])
	AC_PATH_PROGS(JAVA,java,,[$PATH:$javaroot/bin])
	AC_PATH_PROGS(JAVAH,javah,,[$PATH:$javaroot/bin])
	JAVAINCLUDE="-I$javaroot/include"
else
	JAVAC=
	JAVAINCLUDE=
fi

AC_SUBST(SHARED_LDFLAGS)
AC_SUBST(JAVAINCLUDE)

TOPSOURCE=`pwd`
AC_SUBST(TOPSOURCE)

SHARED_SUFFIX=so
SYMBOL_PREFIX=
AC_SUBST(SHARED_SUFFIX)
AC_SUBST(SYMBOL_PREFIX)

AC_PROG_LIBTOOL

AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CHECK_PROGS(DOXYGEN,doxygen)
AC_CHECK_PROGS(PDFLATEX,pdflatex)
AC_CHECK_PROGS(DOT,dot)

dnl These are used in build_rules.am to pass any approptiate 
dnl tag to libtool for f90 building
#if test `$EGREP "available_tags=.*F77" libtool | wc -l` -gt 0; then
#    LTF90COMPILETAG="--tag=F77"
#    LTF90LINKTAG="--tag=CC"
#else
#    LTF90COMPILETAG=""
#    LTF90LINKTAG=""
#fi
#AC_SUBST(LTF90COMPILETAG)
#AC_SUBST(LTF90LINKTAG)

dnl
dnl First any host specific bits
dnl
case $host in
    alpha*-dec-osf*)
	java_host="alpha"
	;;
    *darwin*)
	CFLAGS="$CFLAGS -g -D__unix"
	FFLAGS="$FFLAGS -g -Wno-globals"
	SHARED_SUFFIX=dylib
	SYMBOL_PREFIX=_
dnl This is a hack for libtool - for some reason the F77 tag
dnl does not get set with the commands to create a shared library
	AC_MSG_NOTICE([Patching libtool for F77 shared linking])
	sed -e "s/^archive_cmds=\"\"//" < libtool > libtool.$$
	mv -f libtool.$$ libtool
	chmod +x libtool
	JAVAINCLUDE="-I/System/Library/Frameworks/JavaVM.framework/Headers"
dnl	F90FLAGS="$F90FLAGS -g -cons -YEXT_NAMES=LCS -YEXT_SFX=_"
	FFLAGS="$FFLAGS -qextname"
	SHARED_LDFLAGS="-Wl,-single_module"
#	LTF90COMPILETAG="--tag=F77"
#	LTF90LINKTAG="--tag=CC"
dnl	LDFLAGS="$LDFLAGS -flat_namespace"
dnl     if $CC -dumpspecs 2>&1 | grep 'single_module' >/dev/null ; then
dnl           lt_int_apple_cc_single_mod=yes
dnl     fi
dnl     if test "X$lt_int_apple_cc_single_mod" = Xyes ; then
dnl           archive_cmds='$CC -dynamiclib -single_module'
dnl	fi
	;;
esac

if test ! -z "$JAVAINCLUDE" -a ! -z "$java_host"; then
    JAVAINCLUDE="$JAVAINCLUDE -I$javaroot/include/$java_host"
fi
dnl
dnl * locate path to HDF libraries *
dnl
dnl first see if one has been given
dnl
H4ROOT=""
H5ROOT=""
AC_SUBST(H4ROOT)
AC_SUBST(H5ROOT)
AC_ARG_WITH([hdf4],
	AC_HELP_STRING([--with-hdf4=/path/to/hdf4],
                       [Specify location of HDF4 files]),
	[if test $withval != no; then H4ROOT=$withval; fi])
AC_ARG_WITH([hdf5],
	AC_HELP_STRING([--with-hdf5=/path/to/hdf5],
                       [Specify location of HDF5 files]),
	[if test $withval != no; then H5ROOT=$withval; fi])
dnl otherwise try and find HDF path, but not if
dnl e.g. --without-hdf4 has been specified (hence check on $with_hdf4 != no )
dnl first HDF4
HDF4SEARCH="/usr/local/hdf4 /usr/local/hdf /usr/local /sw /usr"
if test "$with_hdf4" != "no" -a -z "$H4ROOT"; then
    AC_MSG_CHECKING(for location of HDF4 libraries)
    for i in $HDF4SEARCH; do
	if test -z "$H4ROOT" -a -r ${i}/lib/libdf.a; then H4ROOT=$i; fi
    done
    if test -z "$H4ROOT"; then 
	AC_MSG_RESULT(unknown)
    else 
	AC_MSG_RESULT($H4ROOT)
    fi
fi
if test "$H4ROOT"; then
    H4VERSION=`grep LIBVER_STRING ${H4ROOT}/include/hfile.h | cut -d '"' -f 2 | cut -d ' ' -f4,6 | tr ' ,' '. '`
    case $H4VERSION in
	4.[[12]]*) 
    		HDF4_LDFLAGS="-L$H4ROOT/lib -lmfhdf -ldf -ljpeg";
    		HDF4_CPPFLAGS="-I$H4ROOT/include -DHDF4" 
		;;
	*) 	AC_MSG_WARN([The HDF 4 installation has not the right version ($H4VERSION). You need at least 4.1])
#		H4ROOT=""
		;;
    esac		
fi
AC_SUBST(HDF4_LDFLAGS)
AC_SUBST(HDF4_CPPFLAGS)

dnl HDF5
HDF5SEARCH="/usr/local/hdf5 /usr/local/hdf /usr/local /sw /usr"
if test "$with_hdf5" != "no" -a -z "$H5ROOT"; then
    AC_MSG_CHECKING(for location of HDF5 libraries)
    for i in $HDF5SEARCH; do
	if test -z "$H5ROOT" -a -r ${i}/lib/libhdf5.a; then H5ROOT=$i; fi
    done
    if test -z "$H5ROOT"; then 
	AC_MSG_RESULT(unknown)
    else 
	AC_MSG_RESULT($H5ROOT)
    fi
fi
if test "$H5ROOT"; then
    H5VERSION=`grep H5_VERS_INFO ${H5ROOT}/include/H5public.h | cut -d '"' -f 2 | cut -d ' ' -f 4`
    case $H5VERSION in
	1.[[6]]*)
    		HDF5_LDFLAGS="-L$H5ROOT/lib -lhdf5 -lz -lm"
    		HDF5_CPPFLAGS="-I$H5ROOT/include -DHDF5";;
	*)	AC_MSG_WARN([The HDF 5 installation has not the right version ($H5VERSION). You need at least 1.6]) 
#		H5ROOT=""
		;;
    esac
fi
AC_SUBST(HDF5_LDFLAGS)
AC_SUBST(HDF5_CPPFLAGS)

AC_CHECK_SIZEOF(void*)

if test $ac_cv_sizeof_voidp -eq 4; then
    F90_POINTER_KIND=9
fi
if test $ac_cv_sizeof_voidp -eq 8; then
    F90_POINTER_KIND=17
fi
AC_SUBST(F90_POINTER_KIND)

for i in /usr/local/hdf/lib /usr/local/hdf4/lib \
         /usr/local/hdf5/lib /usr/local/lib /sw/lib /usr/lib/hdf; do
    if test -d $i; then 
	LDFLAGS="$LDFLAGS -L$i"
    fi
done

dnl finally set LIBS variable if we 
AC_CHECK_LIB(m, cos)
AC_CHECK_LIB(z, gzopen)
AC_CHECK_LIB(jpeg, jpeg_CreateCompress)
AC_CHECK_LIB(df, Hopen)
AC_CHECK_LIB(mfhdf, ncopen)
AC_CHECK_LIB(sz, SZ_Compress)
AC_CHECK_LIB(hdf5, H5open)
AC_CHECK_LIB(mxml, mxmlLoadFile)
AC_CHECK_LIB(NeXus, nxiopen_)

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_C_BIGENDIAN


dnl Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MKTIME
AC_FUNC_STRFTIME
AC_CHECK_FUNCS([ftime memset strchr strdup strrchr strstr tzset])

AM_CONDITIONAL([HAVE_F77], [test ! -z "$F77"])
AM_CONDITIONAL([HAVE_FC], [test ! -z "$FC"])
AM_CONDITIONAL([HAVE_JAVA], [test ! -z "$JAVA"])
AM_CONDITIONAL([HAVE_JAVAC], [test ! -z "$JAVAC"])
AM_CONDITIONAL([HAVE_JAVADOC], [test ! -z "$JAVADOC"])
AM_CONDITIONAL([HAVE_HDF4], [test ! -z "$H4ROOT"])
AM_CONDITIONAL([HAVE_HDF5], [test ! -z "$H5ROOT"])
AM_CONDITIONAL([HAVE_MATLAB], [test ! -z "$MATLAB"])
AM_CONDITIONAL([HAVE_DOXYGEN], [test ! -z "$DOXYGEN"])
AM_CONDITIONAL([HAVE_PDFLATEX], [test ! -z "$PDFLATEX"])
AM_CONDITIONAL([HAVE_DOT], [test ! -z "$DOT"])

dnl AC_CONFIG_TESTDIR(tests)
dnl AC_CONFIG_FILES(tests/Makefile tests/atlocal)
AM_MISSING_PROG([AUTOM4TE], [autom4te])

if test X$MATLAB != X; then
    for i in glnxa64 glnxi64 glnx86 maci maci64; do
	if test -r "$MATLAB_ROOT/bin/$i"; then
    		MATLAB_ARCH="$i"
	fi
    done
    case $MATLAB_ARCH in
        glnx86)
	    MATLAB_MEXSUFFIX=mexglx
	    ;;
        glnxa64)
	    MATLAB_MEXSUFFIX=mexa64
	    ;;
        glnxi64)
	    MATLAB_MEXSUFFIX=mexi64
	    ;;
	maci)
	    MATLAB_MEXSUFFIX=mexmaci
	    ;;
    esac
    AC_SUBST(MATLAB_MEXSUFFIX)
    AC_SUBST(MATLAB_ARCH)
    AC_SUBST(MATLAB_ROOT)
fi

AC_CONFIG_FILES([Makefile
		libclasses/Makefile
		libcore/Makefile 
		libcore/ixctypes.f90
		bindings/Makefile 
		bindings/f90/Makefile 
		bindings/matlab/Makefile 
		applications/Makefile
		applications/matlab/Makefile
		applications/matlab/homer/Makefile
		applications/matlab/mgeniefuncs/Makefile
		applications/matlab/homer_gui/Makefile
		applications/matlab/MAPS/Makefile
		applications/matlab/MARI/Makefile
		applications/matlab/HET/Makefile
		applications/matlab/MERLIN/Makefile
		doc/Makefile
		doc/doxygen/Makefile
		doc/doxygen/Doxyfile
		libisisexc.spec
		build_rpm
		make_doc
	  	paths.gdb
		xml/Makefile
		tests/Makefile])

AC_OUTPUT

HDF4SUPPORT=`if test -n "$H4ROOT" ; then echo yes ; else echo no ; echo "                found version $H4VERSION"; fi`
HDF5SUPPORT=`if test -n "$H5ROOT" ; then echo yes ; else echo no ; echo "                found version $H5VERSION"; fi`
F77BINDING=`if test -n "$F77" ; then echo yes ; else echo no ; fi`
FCBINDING=`if test -n "$FC" ; then echo yes ; else echo no ; fi`
JAVABINDING=`if test -n "$JAVAC" ; then echo yes ; else echo no ; fi` 
MATLABBINDING=`if test -n "$MATLAB" ; then echo "yes (matlabroot=$MATLAB_ROOT)"; else echo no ; fi` 
AC_MSG_RESULT([

Configuration (libisis):

        Source code location:  ${srcdir}
        Version:               ${VERSION}
        Compiler:              ${CC},${CXX},${F77},${FC}
	Optimise:	       ${enable_optimise}

build:
        libisis with:
                HDF4 support:  ${HDF4SUPPORT} 
                HDF5 support:  ${HDF5SUPPORT}

        bindings:
                F90 :          ${FCBINDING} 
	      MATLAB:	       ${MATLABBINDING}	

Please check whether the configuration I detected matches what you
would like to have.
])
