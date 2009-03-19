function hdl = get_handles(varargin)
%-------help for gtk get handle - gh command-------------------------------
%Function Syntax: figureHandle_ = GH(app_name,app_tag)
%Purpose: To return handle if any figure matches passed property values
%Output: return figure handle. if 0 is returned consider it as empty or NO
%figure exists [ 0 stands for NONE].
%Input: application name and tag
%Example:  fh = GH('tobie','1d')
%the above example returns figure handle if any figure matches name as
%tobie and tag as 1d
%--------------------------------------------------------------------------
% Dean Whittaker 2007

hdl = ui_gethandle(varargin{:});

