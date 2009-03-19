## Process this file with automake to produce Makefile.in
#  
#  $Id: Makefile.am 454 2005-09-29 14:01:40Z faa59 $
#
#  Makefile for coordinating the generation of language bindings
#  
#  Copyright (C) 2004 Freddie Akeroyd
#  
#====================================================================
if HAVE_MATLAB
MATLABSUB = matlab
endif
SUBDIRS = f90 $(MATLABSUB)
doc :
	for subdir in $(SUBDIRS); do \
	    (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) doc ); \
	done
