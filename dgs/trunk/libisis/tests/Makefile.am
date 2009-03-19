## Process this file with automake to produce Makefile.in
#====================================================================
#  $Id: Makefile.am 1323 2008-03-17 17:44:20Z Dickon Champion $
#  
# @configure_input@
#====================================================================
LIBCORE=$(top_builddir)/libcore/libisisxcore.la
LIBCLASSES=$(top_builddir)/libclasses/libisisxclasses.la
LIBBINDINGS=$(top_builddir)/bindings/f90/libisisxbindings.la
#AM_CPPFLAGS=-I. -I$(top_srcdir)/include
AM_FCFLAGS=-I$(top_builddir)/libcore -I$(top_builddir)/libclasses

libisisbin_PROGRAMS = core_test classes_test file_test mari_homer maps_homer het_homer merlin_homer

maps_homer_SOURCES = maps_homer.f90 # diag.f90
maps_homer_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
maps_homer_LINK = $(FCLINK)
maps_homer_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

merlin_homer_SOURCES = merlin_homer.f90 # diag.f90
merlin_homer_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
merlin_homer_LINK = $(FCLINK)
merlin_homer_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

mari_homer_SOURCES = mari_homer.f90 # diag.f90
mari_homer_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
mari_homer_LINK = $(FCLINK)
mari_homer_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

het_homer_SOURCES = het_homer.f90 # diag.f90
het_homer_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
het_homer_LINK = $(FCLINK)
het_homer_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

core_test_SOURCES = core_test.f90
core_test_LDADD = $(LIBCORE) $(LIBBINDINGS) -lNeXus
core_test_LINK = $(FCLINK)
core_test_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

classes_test_SOURCES = classes_test.f90
classes_test_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
classes_test_LINK = $(FCLINK)
classes_test_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

file_test_SOURCES = file_test.f90
file_test_LDADD = $(LIBCLASSES) $(LIBCORE) $(LIBBINDINGS) -lNeXus
file_test_LINK = $(FCLINK)
file_test_LDFLAGS = -static -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(LDFLAGS)

doc:
	@echo "No doc"

CLEANFILES = *.mod *_cpp.f90

include $(top_srcdir)/build_rules.am
