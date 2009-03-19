# Microsoft Developer Studio Project File - Name="classes_f90" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=classes_f90 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "classes_f90.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "classes_f90.mak" CFG="classes_f90 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "classes_f90 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "classes_f90 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "classes_f90 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"..\libcore\Release" /libs:dll /nologo /threads /warn:nofileopt /fpp:"/m"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
IntDir=.\Release
SOURCE="$(InputPath)"
PostBuild_Cmds=copy $(IntDir)\*.mod ..\bin\release	del ..\bin\release\libisisx.lib
# End Special Build Tool

!ELSEIF  "$(CFG)" == "classes_f90 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /include:"..\libcore\Debug" /libs:dll /nologo /threads /traceback /warn:argument_checking /warn:nofileopt /fpp:"/m" /keep
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
IntDir=.\Debug
SOURCE="$(InputPath)"
PostBuild_Cmds=copy $(IntDir)\*.mod ..\bin\debug	del ..\bin\debug\libisisxd.lib
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "classes_f90 - Win32 Release"
# Name "classes_f90 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\isisexc_classes.f90
NODEP_F90_ISISE=\
	".\Release\IXMbase.mod"\
	".\Release\IXMdataset_1d.mod"\
	".\Release\IXMdataset_2d.mod"\
	".\Release\IXMdataset_nd.mod"\
	".\Release\IXMdatum.mod"\
	".\Release\IXMmoments.mod"\
	".\Release\IXMtype_definitions.mod"\
	

!IF  "$(CFG)" == "classes_f90 - Win32 Release"

!ELSEIF  "$(CFG)" == "classes_f90 - Win32 Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\IXMbase.f90
DEP_F90_IXMBA=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMoperation.mod"\
	".\Debug\IXMwrapped_var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_1d.f90
DEP_F90_IXMDA=\
	"..\libcore\Debug\IXMarraymanips.mod"\
	"..\libcore\Debug\IXMderivative.mod"\
	"..\libcore\Debug\IXMintegrate.mod"\
	"..\libcore\Debug\IXMrebin.mod"\
	"..\libcore\Debug\IXMrebunch.mod"\
	"..\libcore\Debug\IXMregroup.mod"\
	"..\libcore\Debug\IXMshift.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	".\Debug\IXMdataset_common.mod"\
	".\Debug\IXMdatum.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_2d.f90
DEP_F90_IXMDAT=\
	"..\libcore\Debug\IXMarraymanips.mod"\
	"..\libcore\Debug\IXMintegrate.mod"\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMrebin.mod"\
	"..\libcore\Debug\IXMrebunch.mod"\
	"..\libcore\Debug\IXMregroup.mod"\
	"..\libcore\Debug\IXMshift.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	".\Debug\IXMdataset_1d.mod"\
	".\Debug\IXMdataset_common.mod"\
	".\Debug\IXMdatum.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_3d.f90
DEP_F90_IXMDATA=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMdataset_common.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_4d.f90
DEP_F90_IXMDATAS=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMdataset_common.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_common.f90
DEP_F90_IXMDATASE=\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_nd.f90
DEP_F90_IXMDATASET=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMpointer_to_array.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMdataset_common.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdatum.f90
DEP_F90_IXMDATU=\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdatum_array.f90
DEP_F90_IXMDATUM=\
	"..\libcore\Debug\IXMarraymanips.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMfermi_chopper.f90
DEP_F90_IXMFE=\
	"..\libcore\Debug\IXMneutron_constants.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMisis_raw_file.f90
DEP_F90_IXMIS=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	".\Debug\IXMdataset_1d.mod"\
	".\Debug\IXMdataset_2d.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMmoments.f90
DEP_F90_IXMMO=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMdataset_common.mod"\
	".\Debug\IXMdatum.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMoperation.f90
DEP_F90_IXMOP=\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMoperation_interfaces.mod"\
	".\Debug\IXMwrapped_var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMoperation_interfaces.f90
# End Source File
# Begin Source File

SOURCE=.\IXMorientation.f90
DEP_F90_IXMOR=\
	"..\libcore\Debug\IXMbasis.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMshape.f90
DEP_F90_IXMSH=\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMtestclass.f90
DEP_F90_IXMTE=\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMtranslation.f90
DEP_F90_IXMTR=\
	"..\libcore\Debug\IXMbasis.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	".\Debug\IXMbase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMwrappedvar.f90
DEP_F90_IXMWR=\
	"..\libcore\Debug\IXMmemory.mod"\
	"..\libcore\Debug\IXMstatus.mod"\
	"..\libcore\Debug\IXMtype_definitions.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\binary_ops.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\class_base.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\class_header.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\IXMoperation_interfaces_get.f90
NODEP_F90_IXMOPE=\
	".\Release\IXMstatus.mod"\
	".\Release\IXMtype_definitions.mod"\
	
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\IXMoperation_interfaces_send.f90
NODEP_F90_IXMOPER=\
	".\Release\IXMstatus.mod"\
	".\Release\IXMtype_definitions.mod"\
	
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\IXMoperation_routines.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\IXMoperation_routines2.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\unary_ops.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\wrappedvar_routines.f90
# PROP Exclude_From_Build 1
# End Source File
# End Group
# End Target
# End Project
