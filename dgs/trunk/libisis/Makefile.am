#====================================================================
#  $Id: Makefile.am 1351 2008-04-29 17:37:23Z Freddie Akeroyd $
#
#  Top level Makefile for coordinating NeXus build
#  
#  Copyright (C) 2004 Freddie Akeroyd
#  
# $Id: Makefile.am 1351 2008-04-29 17:37:23Z Freddie Akeroyd $
#
# @configure_input@
#
#====================================================================

# so   "make test"   is the same as   "make check"
test: check

SUBDIRS=libcore libclasses bindings applications xml tests doc

doc : sources.doxy
	( cd doc; $(MAKE) $(AM_MAKEFLAGS) )

sources.doxy :
	rm -f sources.doxy
	for subdir in $(SUBDIRS); do \
	    (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) doc ); \
	done

EXTRA_DIST=build_rules.am autoversion.sh bindings/matlab/Makefile.in

# Add any flags here that you want passed to configure when you do 
# a "make distcheck"
# DISTCHECK_CONFIGURE_FLAGS=--with-f90=g95

#MATLAB_DIR=/usr/local/matlab
#MATLAB_LIB=$(MATLAB_DIR)/extern/lib/glnx86
#CFLAGS=-fPIC -I$(MATLAB_DIR)/extern/include
#F90=ifort
#F90FLAGS=-fpic -implicitnone -check all -automatic -traceback -warn all -warn nounused

#libisisexc.mexglx : $(OBJ)
#	$(F90) -nofor_main -Wl,-rpath,/opt/intel/lib -Wl,-u,mexFunction -Bdynamic -shared -o $@ -L$(MATLAB_LIB) $(MATLAB_LIB)/version4.o $(OBJ) -lmx
#	( cd matlab; ln -fs ../$@ . )
#
#clean :
#	rm -f *.o *.mod *_cpp.f90 libisisexc.mexglx

#libisisexc.o : libisisexc.h
