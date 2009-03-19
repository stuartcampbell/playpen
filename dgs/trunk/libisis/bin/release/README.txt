The following instructions refer to RELEASE builds only - refer to README.txt in the Debug
folder for Debug builds

This directory stores *.mod (FORTRAN Module files) and *.lib files created from building libisisexc, 
in particular the file  libisisx.lib  which contains the core, classes and fortran bindings for
the complete library


COMPAQ Visual Fortran
---------------------

To link against these routines do the following:

- Create a new Fortran console project

- In the project settings for FORTRAN change the following:
    * in Libraries: choose "Multithreaded DLL"
    * In preprocessor: set the include and module path to the full "libisisexc\bin\release" 
      path (e.g. c:\libisisexc\bin\release )

- In the project setting for Linking:
    * add the same full "libisiexc\bin\release" path used above to the "additional library path" box
    * add either  libisisx.lib  to the end of the "object/library modules:" list

$Id: README.txt 194 2004-07-16 16:39:35Z faa59 $