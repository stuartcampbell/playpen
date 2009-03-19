## Process this file with automake to produce Makefile.in
#====================================================================
#
# $Id: Makefile.am 854 2007-02-12 18:30:49Z Freddie Akeroyd $
#

# documenttion subdirectories
if HAVE_DOXYGEN
if HAVE_PDFLATEX
if HAVE_DOT
DOXYSUB=doxygen
endif
endif
endif
SUBDIRS=$(DOXYSUB)

doc :

# add files and example code here from this directory
libisisdoc_DATA		= README.doc

EXTRA_DIST		= README.doc
