function scf(varargin)
%--------help for gtk set current figure, scf command----------------------
% Function syntax: SCF(fig_handle,[app name, app tag])
% Purpose: set current figure
% Input: figure handle or name,tag or handle,name and tag
% Output: handle is returned if the figure is set else error
% Example: SCF(2) 
% The above example sets figure handle 2 as current
% SCF(2,'tobie','1d')
% The above example sets figure handle 2 with name tobie and tag 1d as current
%--------------------------------------------------------------------------

% Dean Whittaker 2008

res = ui_setcurrfig(varargin{:});
