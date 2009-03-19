echo off
rem
rem $Id: libisisexc.bat 217 2004-07-26 12:58:07Z faa59 $
rem
rem Generate libisisexc.h from libisisexc.txt
rem
echo #ifndef LIBISISEXC_H > libisisexc.h
echo #define LIBISISEXC_H >> libisisexc.h
echo /* >> libisisexc.h
echo  * !!! DO NOT EDIT !!! >> libisisexc.h
echo  * !!! Automatically generated from libisisexc.txt by $Id: libisisexc.bat 217 2004-07-26 12:58:07Z faa59 $ !!! >> libisisexc.h
echo  */ >> libisisexc.h

FOR /F "eol=# tokens=1,2* " %%i in ( libisisexc.txt ) do call libisisexc_1 %%i %%j

echo static mexfunc_s_t mex_functions[] = { >> libisisexc.h
FOR /F "eol=# tokens=1,2* " %%i in ( libisisexc.txt ) do call libisisexc_2 %%i %%j
echo { NULL, NULL } >> libisisexc.h
echo }; >> libisisexc.h

echo #endif /* LIBISISEXC_H */ >> libisisexc.h