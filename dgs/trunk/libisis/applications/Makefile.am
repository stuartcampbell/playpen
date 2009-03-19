## Process this file with automake to produce Makefile.in
#  
#  $Id: Makefile.am 842 2006-09-27 15:06:57Z faa59 $
#
#  Makefile for coordinating the generation of language bindings
#  
#  Copyright (C) 2004 Freddie Akeroyd
#  
#====================================================================
if HAVE_MATLAB
MATLABSUB = matlab
endif
SUBDIRS = $(MATLABSUB)
doc :
	for subdir in $(SUBDIRS); do \
	    (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) doc ); \
	done
