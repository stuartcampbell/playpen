function libisis_init
% Adds the paths needed by Libisis.
% This version is for the pre-compiled installation.
%
% In your startup.m, add the libisis root path and call libisis_init, e.g.
%   addpath('c:\mprogs\libisis')
%   libisis_init
%
% Is PC and Unix compatible.

% T.G.Perring

% root directory is assumed to be that in which this function resides
rootpath = fileparts(which('libisis_init'));
addpath(rootpath)  % MUST have rootpath so that libisis_init, libisis_off included

% Now add to path. Note that folders are different from SVN install version of Libisis
% and also includes DLL folder.
addpath_message (rootpath,'DLL');
addpath_message (rootpath,'classes');
addpath_message (rootpath,'classes','no_class');
addpath_message (rootpath,'matlab','utilities');
addpath_message (rootpath,'matlab','utilities','mfit_funcs');
addpath_message (rootpath,'matlab','utilities','multifit');
addpath_message (rootpath,'graphics');
gtk_init

addpath_message (rootpath,'matlab','homer');
addpath_message (rootpath,'matlab','homer_gui');
addpath_message (rootpath,'matlab','mgeniefuncs');
addpath_message (rootpath,'matlab','MARI');
addpath_message (rootpath,'matlab','MAPS');
addpath_message (rootpath,'matlab','HET');
addpath_message (rootpath,'matlab','MERLIN');

%--------------------------------------------------------------------------
function addpath_message (varargin)
% Add a path from the component directory names, printing a message if the
% directory does not exist.
% e.g.
%   >> addpath_message('c:\mprogs\libisis','bindings','matlab','classes')

% T.G.Perring

string=fullfile(varargin{:});
if exist(string,'dir')==7
    addpath (string);
else
    warning('"%s" is not a directory - not added to path',string)
end
