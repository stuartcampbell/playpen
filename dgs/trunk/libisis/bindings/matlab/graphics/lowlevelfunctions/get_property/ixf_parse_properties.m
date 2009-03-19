function [string_vals, num_vals, string_index, num_index] = ixf_parse_properties(varargin)
% ixf_parse_properties for libisis gtk graphics package separates valid
% strings (i.e. strings that are valid properties) and numbers, the indexes
% are also given in binary format. 
%
% >> [strings, numbers, string_index, numeric_index] =
%                   ixf_parse_properties(input1, input2, input3, input4,...)
%
% any strings (or other data types) which are not contained within the list
% of allowed properties (given in ixf_global_default.m) for markers, colors
% and lines will be ignored. 
%
% inputs:   valid property strings, numbers and other items
%
% outputs:  strings = cell array of any valid string values given in inputs
%           numbers = cell array of any numeric values given in inputs
%           string_index = the binary index of the string values in the
%           input
%           number_index = the binary index of numeric values in the input
%
% example:
% 
% >> [s, n, si, ni] = ixf_parse_properties('o', 4, '--', 2, 'arg')
%
% s will be {'o', '--'}
% n will be {4, 2}
% si will be [1,0,1,0,0]
% ni will be [0,1,0,1,0]
 
IXG_ALL_VALIDVALUES = ixf_global_var('libisis_graphics','get','IXG_ALL_VALIDVALUES');


num_index  = false * ones(size(varargin));
string_index = false * ones(size(varargin));
string_vals = {};
num_vals = {};
for i = 1:length(varargin)
    if isnumeric(varargin{i})
        num_index(i) = true;
        num_vals = {num_vals{:}, varargin{i}};
    elseif ischar(varargin{i})&& any(strmatch(varargin{i}, IXG_ALL_VALIDVALUES)) && ~isempty(varargin{i}) 
        string_index(i) = true;
        string_vals = {string_vals{:}, varargin{i}};
    end
end