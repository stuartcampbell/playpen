## Process this file with automake to produce Makefile.in

# subdirectories that contain .m files and whihc we want to 
# put into the kit
MATLAB_MDIST=classes graphics utilities
FORCE_SYMBOLS=-Wl,-u,$(SYMBOL_PREFIX)mexFunction,-u,$(SYMBOL_PREFIX)mexWarnMsgTxt,-u,$(SYMBOL_PREFIX)mexVersion

LIBCORE=$(top_builddir)/libcore/libisisxcore.la
LIBCLASSES=$(top_builddir)/libclasses/libisisxclasses.la
MATLABLIBDIR=$(MATLAB_ROOT)/bin/$(MATLAB_ARCH)
MATLAB_LIBS=-lmx -lmex
# need both $(top_srcdir) and $(top_builddir)
# as modules get built in "builddir" whereas file includes are in "srcdir"
# in the "make dist" kit, srcdir and builddir are not the same
FC_CPPFLAGS=-I$(top_srcdir)/libclasses -I$(top_builddir)/libclasses -I$(top_builddir)/libcore -I$(MATLAB_ROOT)/extern/include
AM_FCFLAGS=$(FC_CPPFLAGS)

# for argcheck add -DARGCHECK and mwdebug.c
AM_CPPFLAGS=-I$(MATLAB_ROOT)/extern/include -DARGCHECK -DMATLAB_MEX_FILE # -DPRINT_POINTERS

libisislib_LTLIBRARIES = libisisxmatlabdb.la libisisxmatlab.la

if HAVE_MATLAB
MEXDEBUG=mwdebug.c mexversion.c explore.c
libisisxmatlabdb_la_SOURCES = $(MEXDEBUG)
libisisxmatlabdb_la_CPPFLAGS = -DmexFunction=dummyMexFunction $(AM_CPPFLAGS) $(CPPFLAGS)
libisisxmatlabdb_la_LIBADD = $(MATLAB_LIBS)
libisisxmatlabdb_la_LDFLAGS = -no-undefined -R$(MATLABLIBDIR) -L$(MATLABLIBDIR) $(LDFLAGS) # -Wl,--no-allow-shlib-undefined
mwdebug.c : $(MATLAB_ROOT)/extern/src/mwdebug.c
	cp  $< $@

mexversion.c : $(MATLAB_ROOT)/extern/src/mexversion.c
	cp  $< $@

explore.c : $(MATLAB_ROOT)/extern/examples/mex/explore.c
	cp  $< $@
endif

libisisbin_PROGRAMS = test_link matlab_main
test_link_SOURCES = test_link.c
test_link_LINK = $(FCLINK)
test_link_LDADD = libisisxmatlab.la $(LIBCLASSES) $(LIBCORE) $(MATLAB_LIBS) $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(FCLIBS)
test_link_LDFLAGS = -no-undefined -R$(MATLABLIBDIR) -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib -L$(MATLABLIBDIR) $(FORCE_SYMBOLS) $(LDFLAGS) # -Wl,--no-allow-shlib-undefined
# -Wl,--no-allow-shlib-undefined

matlab_main_SOURCES = matlab_main.f90
matlab_main_LINK = $(FCLINK)
matlab_main_LDADD = libisisxmatlab.la $(LIBCLASSES) $(LIBCORE) $(MATLAB_LIBS) -leng $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(FCLIBS)
matlab_main_LDFLAGS = -no-undefined -R$(MATLABLIBDIR) -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib -L$(MATLABLIBDIR) $(FORCE_SYMBOLS) $(LDFLAGS) # -Wl,--no-allow-shlib-undefined

libisisxmatlab_la_SOURCES = \
IXMmatlab_interface.f90 \
bindings.f90 \
IXMfileio_m.f90 \
IXMpath_m.f90 \
IXMbase_m.f90 \
IXMoptions_m.f90 \
IXMdata_source_m.f90 \
IXMhistory_m.f90 \
IXMstring_list_m.f90 \
IXMdatum_m.f90 \
IXMmap_m.f90 \
IXMmask_m.f90 \
IXMdatum_array_m.f90 \
IXMdataset_1d_m.f90 \
IXMdataset_2d_m.f90 \
IXMdataset_3d_m.f90 \
IXMdataset_4d_m.f90 \
IXMfermi_chopper_m.f90 \
IXMtestclass_m.f90 \
IXMtranslation_m.f90 \
IXMorientation_m.f90 \
IXMgeometry_m.f90 \
IXMraw_file_m.f90 \
IXMshape_m.f90 \
IXMgroup_m.f90 \
IXMgroups_m.f90 \
IXMaperture_m.f90 \
IXMattenuator_m.f90 \
IXMbridge_m.f90 \
IXMchopper_instrument_m.f90 \
IXMdiffraction_instrument_m.f90 \
IXMinstrument_m.f90 \
IXMdata_m.f90 \
IXMdetector_m.f90 \
IXMdet_he3_m.f90 \
IXMdet_solid_m.f90 \
IXMeffdet_index_m.f90 \
IXMlattice_m.f90 \
IXMmoderator_m.f90 \
IXMmoments_m.f90 \
IXMpeaks_m.f90 \
IXMaxis_m.f90 \
IXMrunfile_m.f90 \
IXMsample_m.f90 \
IXMsource_m.f90 \
IXMspectra_m.f90 \
IXMsw_bridge_m.f90 \
IXMuser_m.f90 \
IXMworkspace_m.f90 \
IXMws_bridge_m.f90 \
IXMutility_m.f90 \
matint.c \
libisisexc.c 
#simons_diag.f90 

#nodist_libisisxmatlab_la_SOURCES = $(MEXDEBUG)

BUILT_SOURCES=libisisexc.h $(MEXDEBUG)

EXTRA_DIST=unary_ops.f90 binary_ops.f90 bindings_header.f90 \
           bindings_base.f90 bindings_extra.f90 matlab_interface_routines.f90 \
	   binaryd1d.f90 advanced_unary_ops.f90 \
           libisisexc.txt libisisexc.sh \
	   $(MATLAB_MDIST) # matlab .m file directories

classes/libisisexc.$(MATLAB_MEXSUFFIX) : libisisxmatlab.la
	( if test -d classes; then cd classes; rm -f libisisexc.$(MATLAB_MEXSUFFIX); cp ../.libs/libisisxmatlab.$(SHARED_SUFFIX) libisisexc.$(MATLAB_MEXSUFFIX); fi )

libisisexc.h: $(srcdir)/libisisexc.txt $(srcdir)/libisisexc.sh
	sh $(srcdir)/libisisexc.sh $(srcdir)/libisisexc.txt

#libisisxclasses = $(top_builddir)/bindings/f77/libNeXus77.la \
#			$(top_builddir)/src/libNeXus.la
#libisisxmatlab_la_LDFLAGS = @SHARED_LDFLAGS@
libisisxmatlab_la_LINK = $(FCLINK)
libisisxmatlab_la_LIBADD = $(LIBCLASSES) $(LIBCORE) libisisxmatlabdb.la $(MATLAB_LIBS) $(HDF4_LDFLAGS) $(HDF5_LDFLAGS) $(FCLIBS)
libisisxmatlab_la_LDFLAGS = -no-undefined -R$(MATLABLIBDIR) -R$(FC_LOAD_PATH) -R/usr/local/hdf5/lib -R/usr/local/lib -L$(MATLABLIBDIR) $(FORCE_SYMBOLS) $(LDFLAGS) # -Wl,--no-allow-shlib-undefined

# -Wl,--no-allow-shlib-undefined
#
# This is needed as f90 is not supported as a valid tag in libtool
# and the relink command (used during an install) which is written to
# libisisxclasses.la will not work. 
#all: all-am libisisxmatlab.la_orig

#libisisxmatlab.la_orig: libisisxmatlab.la
#	mv libisisxmatlab.la $@
#	sed -e 's/--mode=relink/--mode=relink @LTF90LINKTAG@/' < $@ > libisisxmatlab.la
#	touch $@

all-local : libisisxmatlab.la classes/libisisexc.$(MATLAB_MEXSUFFIX)
	if test -d classes; then \
	    for i in classes/\@?*; do \
                cp classes/generic_methods/*.m $$i; \
            done; \
	fi

dist-hook :
	for mdir in $(MATLAB_MDIST); do \
	    find $(distdir)/$$mdir -depth -type d -name 'CVS' -exec rm -fr {} \; ;\
	    find $(distdir)/$$mdir -depth -type d -name '.svn' -exec rm -fr {} \; ;\
	    find $(distdir)/$$mdir -type f ! -name '*.m' ! -name '*.txt' ! -name '*.JPG' -exec rm -f {} \; ;\
	done

install-data-local :
	$(mkinstalldirs) $(DESTDIR)$(libisismatlabdir)
	for mdir in $(MATLAB_MDIST); do \
	    cp -r $(srcdir)/$$mdir $(DESTDIR)$(libisismatlabdir); \
	done
	find $(DESTDIR)$(libisismatlabdir) -type f -exec chmod 0644 {} \;
	find $(DESTDIR)$(libisismatlabdir) -type d -exec chmod 0755 {} \;

install-data-hook :
	( cd $(DESTDIR)$(libisismatlabdir)/classes; cp $(DESTDIR)$(libisislibdir)/libisisxmatlab.$(SHARED_SUFFIX) libisisexc.$(MATLAB_MEXSUFFIX) )

uninstall-local :
	rm -fr $(DESTDIR)$(libisismatlabdir)

doc : $(SOURCES)
	for i in $(SOURCES); do echo "INPUT += \"`pwd`/$$i\"" >> $(top_builddir)/sources.doxy; done

include $(top_srcdir)/build_rules.am

CLEANFILES = libisisxmatlab.la_orig *.mod *_cpp.f90 libisisexc.h \
	classes/libisisexc.$(MATLAB_MEXSUFFIX) $(MEXDEBUG)

