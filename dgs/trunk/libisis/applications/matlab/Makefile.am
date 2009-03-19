## Process this file with automake to produce Makefile.in
#  
#  $Id: Makefile.am 1397 2008-06-09 08:43:15Z Dickon Champion $
#
#  Makefile for coordinating the generation of language bindings
#  
#  Copyright (C) 2004 Freddie Akeroyd
#  
#====================================================================
if HAVE_MATLAB
MATLABSUB = homer mgeniefuncs homer_gui MAPS HET MARI MERLIN
endif
SUBDIRS = $(MATLABSUB)
doc :
	@echo done
#doc :
#	for subdir in $(SUBDIRS); do \
#	    (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) doc ); \
#	done
