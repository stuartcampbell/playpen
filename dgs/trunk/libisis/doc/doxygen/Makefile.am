## Process this file with automake to produce Makefile.in
#====================================================================
#
# $Id: Makefile.am 515 2005-06-22 08:33:09Z faa59 $
#

# documenttion subdirectories

if HAVE_DOXYGEN
if HAVE_PDFLATEX
if HAVE_DOT

bin_PROGRAMS=join_f90_continuations
join_f90_continuations_SOURCES=join_f90_continuations.c

DOXY_STUFF = refman.pdf # html
CLEANFILES = refman.pdf doxy.err

refman.pdf :
	if test -e $(top_builddir)/sources.doxy; then \
	    rm -fr html; \
	    $(DOXYGEN) Doxyfile > doxy.err 2>&1; \
	else \
	    echo $(top_builddir)/sources.doxy does not exist; \
	    touch refman.pdf; \
	fi
endif
endif
endif

libisisdoc_DATA 	= $(DOXY_STUFF)

EXTRA_DIST = $(DOXY_STUFF)
