% To change locations etc. edit this file - ..\homer_standalone_src should
% be replaced with wherever source files should go,
% ..\homer_standalone\dist for distribution files, homer is the name of the
% .exe file (and thus ctf file)

% use relative folders so that it's more robust against folder changes.

% First work out where the homer_compile commmand is located and move
% relative to that.
parent_path = which('homer_compile_win32');
parent_dir = fileparts(parent_path);
cd(parent_dir)

% make directories for source and distribution files

% Compile into src, move relevant files to zip into dist. Zip everything in
% dist and leave in the _standalone folder. Then delete the temporary
% folders.
mkdir('..\homer_standalone')
mkdir('..\homer_standalone\src')
mkdir('..\homer_standalone\dist')

% compile into source folder
% mcc -o <name of .exe file> -W 'main' -d <folder location to put .exe and
% .ctf files> -T 'link:exe' -v -N <name of mfile to compile (can omit
% '.m')>
mcc -o 'homer' -W 'main' -d '..\homer_standalone\src' -T 'link:exe' -v -N 'Homer_ver0.m' 

% Copy instrument files
mkdir('..\homer_standalone\dist\instrument_setup')
copyfile('instrument_setup\*.txt','..\homer_standalone\dist\instrument_setup','f')

% move to the source folder
cd ..
cd homer_standalone
cd src

% move license and DLL folders from the root directory
mkdir('..\dist\DLL')
mkdir('..\dist\License')
copyfile('homer.exe','..\dist','f')
copyfile('homer.ctf','..\dist','f')

% using *.txt etc. prevents the .svn folder being copied.
copyfile('..\..\..\..\win32_distref\*.dll','..\dist\DLL','f')
copyfile('..\..\..\..\win32_distref\*.lib','..\dist\DLL','f')
copyfile('..\..\..\..\License\*.txt','..\dist\License','f')
copyfile('..\..\..\..\License\*.rtf','..\dist\License','f')

cd ..
cd dist
% zip files into homer_standalone directory, remove if it's already there.
if exist('..\homer_standalone.zip')
    delete ..\homer_standalone.zip
end
zip('..\homer_standalone.zip','*')


% move the MCRinstaller into the directory
mystring = fullfile(matlabroot, 'toolbox','compiler','deploy','win32');
copyfile([mystring filesep 'MCRInstaller.exe'],'..\MCRInstaller.exe','f')

% move to the location of the distributed files and remove unecessary
% files.
cd ..
rmdir('dist','s')
rmdir('src','s')

% Give a confirmation text file for this

tstamp = clock;

datestr = ['Homer Compiled Successfully On ' num2str(tstamp(3)) '/' num2str(tstamp(2)) '/' num2str(tstamp(1)) ' at time ' num2str(tstamp(4)) ':' num2str(tstamp(5)) ':' num2str(tstamp(6))];

fid = fopen('completed.txt', 'w');

fwrite(fid, datestr);

fclose(fid);

