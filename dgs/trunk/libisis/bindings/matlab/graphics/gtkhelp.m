function gtkhelp
%--------------------------------------------------------
%function syntax: gtkhelp
%purpose: to show function index for all functions in gtk
%input: none
%output:none
%example: gtkhelp
%--------------------------------------------------------

%global structure
IXG_GTK_INSTALLATION_PATH = ixf_global_var('libisis_graphics','get','IXG_GTK_INSTALLATION_PATH');

name = 'gtkhelp.txt';
filename = [IXG_GTK_INSTALLATION_PATH '\' name];
fid=fopen(filename);
while ~feof(fid)
    tline = fgetl(fid);
    if ~ischar(tline), break, end
    disp(tline)
end
fclose(fid);

%check additional help is there if not stop
addhelp = 'addhelp.txt';
filename = [IXG_GTK_INSTALLATION_PATH '\' addhelp];
fid=fopen(filename,'r');
if (fid > 0)
    while ~feof(fid)
        tline = fgetl(fid);
        if ~ischar(tline), break, end
        disp(tline)
    end
    fclose(fid);
end