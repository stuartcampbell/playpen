## Process this file with automake to produce Makefile.in
libisislib_LTLIBRARIES = libisisxclasses.la
FC_CPPFLAGS=-I$(top_builddir)/libcore
AM_FCFLAGS=$(FC_CPPFLAGS)
AM_CPPFLAGS = -DHDF4 @HDF4_CPPFLAGS@
AM_LDFLAGS=-static

libisisxclasses_la_SOURCES = $(FORTRAN_FILES) $(C_FILES)
FORTRAN_FILES = \
idc_fortran.f90 \
IXMwrappedvar.f90 \
NXmodule.f90 \
NXUmodule.f90 \
IXMpath.f90 \
IXMfileio.f90 \
IXMdata_source.f90 \
IXMoperation_interfaces.f90 \
IXMhistory.f90 \
IXMstring_list.f90 \
IXMoperation.f90 \
IXMbase.f90 \
IXMdatum.f90 \
IXMdatum_array.f90 \
IXMaxis.f90 \
IXMcut.f90 \
IXMslice.f90 \
IXMspe.f90 \
IXMoptions.f90 \
IXMdataset_common.f90 \
IXMdataset_1d.f90 \
IXMdataset_2d.f90 \
IXMdataset_3d.f90 \
IXMdataset_4d.f90 \
IXMdataset_nd.f90 \
IXMfermi_chopper.f90 \
IXMmoments.f90 \
IXMtranslation.f90 \
IXMorientation.f90 \
IXMshape.f90 \
IXMgeometry.f90 \
IXMaperture.f90 \
IXMattenuator.f90 \
IXMcrystalanalyser.f90 \
IXMdet_he3.f90 \
IXMdet_solid.f90 \
IXMraw_file.f90 \
IXMmask.f90 \
IXMmap.f90 \
IXMspectra.f90 \
IXMdetector.f90 \
IXMmoderator.f90 \
IXMlattice.f90 \
IXMsample.f90 \
IXMsource.f90 \
IXMuser.f90 \
IXMtestclass.f90 \
IXMdiffraction_instrument.f90 \
IXMchopper_instrument.f90 \
IXMinstrument.f90 \
IXMws_bridge.f90 \
IXMeffdet_index.f90 \
IXMworkspace.f90 \
IXMsw_bridge.f90 \
IXMbridge.f90 \
IXMdata.f90 \
IXMpeaks.f90 \
IXMrunfile.f90 \
IXMinput_source.f90 \
IXMgroup.f90 \
IXMgroups.f90

C_FILES = endian_convert.c endian_convert.h idc.c idc.h \
	isisds_command.c isisds_command.h

EXTRA_DIST=class_base.f90 class_header.f90 unary_ops.f90 binary_ops.f90 \
	IXMoperation_interfaces_send.f90 IXMoperation_interfaces_get.f90 \
	IXMoperation_routines.f90 IXMoperation_routines2.f90 \
	wrappedvar_routines.f90 fileio_routines.f90 \
	fileio_routines2.f90 input_source_routines.f90

#libisisxclasses = $(top_builddir)/bindings/f77/libNeXus77.la \
#			$(top_builddir)/src/libNeXus.la
libisisxclasses_la_LINK = $(FCLINK)
libisisxclasses_la_LDFLAGS = -no-undefined @SHARED_LDFLAGS@ $(LDFLAGS)

# This is needed as f90 is not supported as a valid tag in libtool
# and the relink command (used during an install) which is written to
# libisisxclasses.la will not work. 
#all: all-am libisisxclasses.la_orig

#libisisxclasses.la_orig: libisisxclasses.la
#	mv libisisxclasses.la $@
#	sed -e 's/--mode=relink/--mode=relink @LTF90LINKTAG@/' < $@ > libisisxclasses.la
#	touch $@

doc : $(SOURCES)
	for i in $(SOURCES); do echo "INPUT += \"`pwd`/$$i\"" >> $(top_builddir)/sources.doxy; done

include $(top_srcdir)/build_rules.am

CLEANFILES = libisisxclasses.la_orig *.mod *_cpp.f90

# g95depend : $(FORTRAN_FILES)
# 	$(FCCOMPILE) -M $(FORTRAN_FILES) > deps 2>/dev/null
# 
# @am__include@ deps
