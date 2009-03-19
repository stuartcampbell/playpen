The following instructions refer to DEBUG builds only - refer to README.txt in the Release
folder for Release builds

This directory stores *.mod (FORTRAN Module files) and *.lib files created from building libisisexc, 
in particular the file  libisisxd.lib  which contains the core, classes and fortran bindings for
the complete library (the final "d" in the library name indicates this is a debug build)


COMPAQ Visual Fortran
---------------------

To link against these routines do the following:

- Create a new Fortran console project

- In the project settings for FORTRAN change the following:
    * in Libraries: choose "Debug multithreaded DLL"
    * In preprocessor: set the include and module path to the full "libisisexc\bin\debug" 
      path (e.g. c:\libisisexc\bin\debug )

- In the project setting for Linking:
    * add the same full "libisiexc\bin\debug" path used above to the "additional library path" box
    * add either  libisisxd.lib  to the end of the "object/library modules:" list
    * in the "ignore libraries list" add   msvcrt.lib

$Id: README.txt 194 2004-07-16 16:39:35Z faa59 $