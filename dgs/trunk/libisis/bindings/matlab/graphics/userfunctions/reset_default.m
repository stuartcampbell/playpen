function reset_default(varargin)
%---help for gtk reset default command, rd---------------------------------
%Function Syntax: RD
%Purpose: reset default properties for current graph
%Output: success or error
%Input: none
%Example: 
%res = RD
%-------------------------------------------------------------------------

% Dean Whittaker 2008

ret = ui_resetdefault(varargin{:});

