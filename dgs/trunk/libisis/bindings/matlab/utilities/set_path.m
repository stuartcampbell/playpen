function [ output_args ] = set_path(varargin)
% mgenie style set_dir command for libisis. Sets the directory where data is
% stored that will be used in Libisis if megenie style commands are to be
% used
% 
% >> set_dir(dir, dir2, dir3, dir4)
%
% inputs
%------------
%
% input:       Char     The disk that is going to be used e.g. 'C:'
%
% outputs
%------------
%
% none
%
% example: set_dir('c:', 'maps_files', 'rawfiles')
%
% will set the path C:/maps_files/rawfiles
%
% separation of arguments makes this function platform independent,
% however one can use a single string i.e.
%
% >> set_dir('c:/maps_files/rawfiles')
%
% if preferred.


if nargin > 1
    dir_string = fullfile(varargin{:});   % This insures that things are platform independent

else 
    
    dir_string = varargin{1};
end



ixf_global_var('data_source','set','path',dir_string);