## Process this file with automake to produce Makefile.in
libisislib_LTLIBRARIES = libisisxcore.la
FC_CPPFLAGS=-I.
AM_FCFLAGS=$(FC_CPPFLAGS)
AM_LDFLAGS=-static

libisisxcore_la_SOURCES = \
IXMtype_definitions.f90 \
IXMphysical_constants.f90 \
IXMneutron_constants.f90 \
IXMerrorcodes.f90 \
IXMstatus.f90 \
IXMmemory.f90 \
memory_utils.f90 \
IXMio.f90 \
IXMlibcore.f90 \
IXMsort.f90 \
IXMmask_array.f90 \
IXMpointer_to_array.f90 \
IXMarraymanips.f90 \
IXMderivative.f90 \
IXMindex.f90 \
IXMshift.f90 \
IXMintegrate.f90 \
IXMrebin.f90 \
IXMrebunch.f90 \
IXMregroup.f90 \
IXMmaths_utils.f90 \
IXMmaths_basis.f90 \
IXMmaths_geometry.f90 \
IXMmaths_projection.f90 \
IXMtools.f90 \
IXMefficiency.f90 \
IXMneutron_units.f90 \
IXMunits_utils.f90 \
IXMunspike.f90 \
IXMmoments_utils.f90 \
IXMregrid.f90 \
libcore.c

EXTRA_DIST = \
IXMmemory_interface.f90 \
IXMmemory_routines.f90 \
IXMarraymanips_routines.f90

#libisisxcore_la_LIBADD = $(top_builddir)/bindings/f77/libNeXus77.la \
#			$(top_builddir)/src/libNeXus.la
libisisxcore_la_LINK = $(FCLINK)
libisisxcore_la_LDFLAGS = -no-undefined @SHARED_LDFLAGS@ $(LDFLAGS)

# This is needed as f90 is not supported as a valid tag in libtool
# and the relink command (used during an install) which is written to
# libisisxcore.la will not work. 
#all: all-am libisisxcore.la_orig

#libisisxcore.la_orig: libisisxcore.la
#	mv libisisxcore.la $@
#	sed -e 's/--mode=relink/--mode=relink @LTF90LINKTAG@/' < $@ > libisisxcore.la
#	touch $@

doc : $(SOURCES)
	for i in $(SOURCES); do echo "INPUT += \"`pwd`/$$i\"" >> $(top_builddir)/sources.doxy; done
#	sh $(top_srcdir)/make_doc "libcore" "$(FC_CPPFLAGS)" $+

include $(top_srcdir)/build_rules.am

CLEANFILES = libisisxcore.la_orig *.mod *_cpp.f90

