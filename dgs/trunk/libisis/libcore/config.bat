del ixctypes.f90
FOR /F "tokens=*" %%i in ( ixctypes.f90.in ) do call :process "%%i"
goto :eof

:process
set temp=%~1
set line=%temp:@F90_POINTER_KIND@=9%
@echo %line% >> ixctypes.f90
goto :eof

