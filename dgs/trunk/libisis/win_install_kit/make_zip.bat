set libisis=c:\libisis_new
set wininst=%libisis%\win_install_kit
del %wininst%\mfiles.zip
del %wininst%\mfiles.exe
cd %libisis%\bindings\matlab
%wininst%\zip -r %wininst%\mfiles.zip classes -i *.m
%wininst%\zip -r -u %wininst%\mfiles.zip graphics -i *.m
%wininst%\zip -r -u %wininst%\mfiles.zip utilities -i *.m
copy/b %wininst%\unzipsfx.exe+%wininst%\mfiles.zip %wininst%\mfiles.exe
%wininst%\zip -A %wininst%\mfiles.exe
del %wininst%\mfiles.zip
pause