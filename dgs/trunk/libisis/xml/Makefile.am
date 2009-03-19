## Process this file with automake to produce Makefile.in
#====================================================================
#  $Id: Makefile.am 721 2006-05-19 10:55:07Z faa59 $
#  
# @configure_input@
#====================================================================
LIBCORE=$(top_builddir)/libcore/libisisxcore.la
LIBCLASSES=$(top_builddir)/libclasses/libisisxclasses.la
LIBBINDINGS=$(top_builddir)/bindings/f90/libisisxbindings.la
#AM_CPPFLAGS=-I. -I$(top_srcdir)/include
AM_FCFLAGS=-I$(top_builddir)/libcore -I$(top_builddir)/libclasses

libisisbin_PROGRAMS = make_xml

make_xml_SOURCES = make_xml.f90
make_xml_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
make_xml_LINK = $(FCLINK)
make_xml_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

doc:
	@echo "No doc"

CLEANFILES = *.mod *_cpp.f90 *.xml *.nxs
EXTRA_DIST=xml_routines.f90

include $(top_srcdir)/build_rules.am
