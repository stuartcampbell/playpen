
function w = ui_get_fromfile(filename, format,varargin)
%------help for gtk ui_get_fromfile get from ascii file function-----------
%
% syntax: w = ui_get_fromfile(filename, format,property, value,...)
%
% inputs: filename - the name of the file that contains the ascii data,
% format, either the standard value for xye, or the standard value for xyze
% data, property value pairs (eg. xlabel, ylabel, title, etc.)
%
% output: w - dataset_1d for xye data, dataset_2d for xyze data
%
% purpose: turn ascii files into datasets
%
%---------Dean Whittaker 23/10/2006---------------------

IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');


if format == IXG_ST_STDVALUES.xye 
    w = ixf_gen_interface('iname','file_interface','fname','get_xye',filename,varargin{:});
elseif format == IXG_ST_STDVALUES.xyze
    w = ixf_gen_interface('iname','file_interface','fname','get_xyze',filename,varargin{:});
else 
    error('incorrect format type given to ui_get_fromfile');
end
