## Process this file with automake to produce Makefile.in
FC_CPPFLAGS=-I$(top_srcdir)/libclasses -I$(top_builddir)/libcore
AM_CPPFLAGS=-I$(top_srcdir)/libclasses
AM_FCFLAGS=$(FC_CPPFLAGS)
libisislib_LTLIBRARIES = libisisxbindings.la

libisisxbindings_la_SOURCES = \
bindings.f90 \
bindings_utils.c

#libisisxclasses = $(top_builddir)/bindings/f77/libNeXus77.la \
#			$(top_builddir)/src/libNeXus.la
#libisisxbindings_la_LDFLAGS = @SHARED_LDFLAGS@
libisisxbindings_la_LINK = $(FCLINK)

# This is needed as f90 is not supported as a valid tag in libtool
# and the relink command (used during an install) which is written to
# libisisxclasses.la will not work. 
#all: all-am libisisxbindings.la_orig

#libisisxbindings.la_orig: libisisxbindings.la
#	mv libisisxbindings.la $@
#	sed -e 's/--mode=relink/--mode=relink @LTF90LINKTAG@/' < $@ > libisisxbindings.la
#	touch $@

doc : $(SOURCES)
	for i in $(SOURCES); do echo "INPUT += \"`pwd`/$$i\"" >> $(top_builddir)/sources.doxy; done

include $(top_srcdir)/build_rules.am

CLEANFILES = libisisxbindings.la_orig *.mod *_cpp.f90

