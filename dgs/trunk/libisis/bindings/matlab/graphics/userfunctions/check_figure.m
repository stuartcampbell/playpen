function hdl = check_figure(varargin)
%-----help for gtk check figure command chkf-------------------------------
% Function Syntax: figureHandle_ = CHKF(fig_handle, app_name, app_tag)
% Purpose: To check any figure exists which matches passed property values
%
% Input: figure handle, application name, application tag
% Output: return figure handle. if 0 is returned consider it as empty or NO
% figure exists [ 0 stands for NONE ].
%
% Examples: 
% fh = CHKF(1,'tobie','1d')
% the abov examples check whether figure with handle 1, name tobie and tag
% as 1d exists or not
%--------------------------------------------------------------------------
% Dean Whittaker 2007

hdl = ui_checkfigure(varargin{:});

