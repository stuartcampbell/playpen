set BASE=c:\libisisexc\win_install_kit
msidb.exe -d %BASE%\debug\libisisexc.msi -f %BASE%\tables -i Environment.idt
msidb.exe -d %BASE%\release\libisisexc.msi -f %BASE%\tables -i Environment.idt