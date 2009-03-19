function libisis_init
% Adds the paths needed by Libisis.
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

% From bindings directory
addpath_message (rootpath,'bindings','matlab','classes');
addpath_message (rootpath,'bindings','matlab','classes','no_class');
addpath_message (rootpath,'bindings','matlab','utilities');
addpath_message (rootpath,'bindings','matlab','utilities','mfit_funcs');
addpath_message (rootpath,'bindings','matlab','utilities','multifit');
addpath_message (rootpath,'bindings','matlab','graphics');
gtk_init    % resides in 'bindings','matlab','graphics'

% From applications directory
addpath_message (rootpath,'applications','matlab','homer');
addpath_message (rootpath,'applications','matlab','homer_gui');
addpath_message (rootpath,'applications','matlab','mgeniefuncs');
addpath_message (rootpath,'applications','matlab','MARI');
addpath_message (rootpath,'applications','matlab','MAPS');
addpath_message (rootpath,'applications','matlab','HET');
addpath_message (rootpath,'applications','matlab','MERLIN');

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
