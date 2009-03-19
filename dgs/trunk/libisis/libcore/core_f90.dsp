# Microsoft Developer Studio Project File - Name="core_f90" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=core_f90 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "core_f90.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "core_f90.mak" CFG="core_f90 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "core_f90 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "core_f90 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "core_f90 - Win32 Release"

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
# ADD F90 /compile_only /libs:dll /nologo /threads /warn:nofileopt /fpp:"/m"
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

!ELSEIF  "$(CFG)" == "core_f90 - Win32 Debug"

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
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /libs:dll /nologo /threads /traceback /warn:argument_checking /warn:nofileopt /fpp:"/m" /keep
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

# Name "core_f90 - Win32 Release"
# Name "core_f90 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\IXMarraymanips.f90
NODEP_F90_IXMAR=\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMbasis.f90
DEP_F90_IXMBA=\
	".\Debug\IXMtype_definitions.mod"\
	
NODEP_F90_IXMBA=\
	".\Debug\IXMutils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMderivative.f90
NODEP_F90_IXMDE=\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMerrorcodes.f90
# End Source File
# Begin Source File

SOURCE=.\IXMindex.f90
NODEP_F90_IXMIN=\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMintegrate.f90
NODEP_F90_IXMINT=\
	".\Debug\IXMarraymanips.mod"\
	".\Debug\IXMindex.mod"\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMio.f90
NODEP_F90_IXMIO=\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMlibcore.f90
NODEP_F90_IXMLI=\
	".\Debug\IXMmemory.mod"\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMmemory.f90
NODEP_F90_IXMME=\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMneutron_constants.f90
NODEP_F90_IXMNE=\
	".\Debug\IXMphysical_constants.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMphysical_constants.f90
NODEP_F90_IXMPH=\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMpointer_to_array.f90
NODEP_F90_IXMPO=\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMrebin.f90
NODEP_F90_IXMRE=\
	".\Debug\IXMindex.mod"\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMrebunch.f90
NODEP_F90_IXMREB=\
	".\Debug\IXMmemory.mod"\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMregroup.f90
NODEP_F90_IXMREG=\
	".\Debug\IXMindex.mod"\
	".\Debug\IXMstatus.mod"\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMshift.f90
NODEP_F90_IXMSH=\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMstatus.f90
NODEP_F90_IXMST=\
	".\Debug\IXMerrorcodes.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\IXMtype_definitions.f90
# End Source File
# Begin Source File

SOURCE=.\IXMutils.f90
DEP_F90_IXMUT=\
	".\Debug\IXMtype_definitions.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\IXMarraymanips_routines.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\IXMmemory_interface.f90
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\IXMmemory_routines.f90
# PROP Exclude_From_Build 1
# End Source File
# End Group
# End Target
# End Project
