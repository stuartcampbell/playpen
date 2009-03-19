@echo off
rem
rem $Id: update_generic_methods.bat 241 2004-07-30 16:33:39Z faa59 $
rem
rem update generic methods from  classes\generic_methods\*.m
rem
rem called in post-build step of matlab_c project build
rem
for /d %%i in ( classes\@* ) do xcopy /q /y /r classes\generic_methods\*.m %%i > nl: