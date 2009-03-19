# Microsoft Developer Studio Project File - Name="matlab_f90" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=matlab_f90 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "matlab_f90.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "matlab_f90.mak" CFG="matlab_f90 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "matlab_f90 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "matlab_f90 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "matlab_f90 - Win32 Release"

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
# ADD F90 /compile_only /include:"../../libclasses/release" /include:"../../libcore/release" /libs:dll /nologo /threads /warn:nofileopt /fpp:"/m"
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

!ELSEIF  "$(CFG)" == "matlab_f90 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "matlab_f90___Win32_Debug"
# PROP BASE Intermediate_Dir "matlab_f90___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "matlab_f90___Win32_Debug"
# PROP Intermediate_Dir "matlab_f90___Win32_Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /include:"../../libclasses/debug" /include:"../../libcore/debug" /libs:dll /nologo /threads /traceback /warn:argument_checking /warn:nofileopt /fpp:"/m" /keep
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

!ENDIF 

# Begin Target

# Name "matlab_f90 - Win32 Release"
# Name "matlab_f90 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\bindings.f90
DEP_F90_BINDI=\
	"..\..\libcore\Debug\IXMstatus.mod"\
	"..\..\libcore\Debug\IXMtype_definitions.mod"\
	
NODEP_F90_BINDI=\
	".\matlab_f90___Win32_Debug\IXMmatlab_interface.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMbase_m.f90
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_1d_m.f90
NODEP_F90_IXMDA=\
	".\matlab_f90___Win32_Debug\IXMm_dataset_1d.mod"\
	".\matlab_f90___Win32_Debug\IXMm_datum.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdataset_2d_m.f90
NODEP_F90_IXMDAT=\
	".\matlab_f90___Win32_Debug\IXMm_dataset_1d.mod"\
	".\matlab_f90___Win32_Debug\IXMm_dataset_2d.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdatum_array_m.f90
NODEP_F90_IXMDATU=\
	".\matlab_f90___Win32_Debug\IXMm_datum_array.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMdatum_m.f90
# End Source File
# Begin Source File

SOURCE=.\IXMfermi_chopper_m.f90
NODEP_F90_IXMFE=\
	".\matlab_f90___Win32_Debug\IXMm_fermi_chopper.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMisis_raw_file_m.f90
NODEP_F90_IXMIS=\
	".\matlab_f90___Win32_Debug\IXMm_dataset_1d.mod"\
	".\matlab_f90___Win32_Debug\IXMm_dataset_2d.mod"\
	".\matlab_f90___Win32_Debug\IXMm_isis_raw_file.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMmatlab_interface.f90
DEP_F90_IXMMA=\
	"..\..\libclasses\Debug\IXMoperation_interfaces.mod"\
	"..\..\libcore\debug\IXMlibcore.mod"\
	"..\..\libcore\Debug\IXMmemory.mod"\
	"..\..\libcore\Debug\IXMstatus.mod"\
	"..\..\libcore\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMshape_m.f90
NODEP_F90_IXMSH=\
	".\matlab_f90___Win32_Debug\IXMm_shape.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMtestclass_m.f90
NODEP_F90_IXMTE=\
	".\matlab_f90___Win32_Debug\IXMm_testclass.mod"\
	".\matlab_f90___Win32_Debug\IXMmatlab_interface.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMtranslation_m.f90
NODEP_F90_IXMTR=\
	".\matlab_f90___Win32_Debug\IXMm_translation.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\testclas_diag2.f90
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

SOURCE=.\matlab_interface_routines.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\unary_ops.f90
# PROP Exclude_From_Build 1
# End Source File
# End Group
# End Target
# End Project
