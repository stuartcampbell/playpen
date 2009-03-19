function hdl = sat(varargin)
%-------help for gtk set application name and tag command sat--------------
%Function syntax: ret = SAT(fig_handle,app_name,app_tag)
%
%Purpose: set application name and tag for the figure handle passed
%
%Input: figure handle,app name,app tag
%Output: handle is returned if the figure is set else error
%
%Example: SAT(2,'tobie','1d') 
%The above example sets 2 is figure handle, tobie is name and 1d is tag
%
%--------------------------------------------------------------------------
hdl = ui_setappname(varargin{:});

