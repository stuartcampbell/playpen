function set_user_preferences(varargin)
%----------help for gtk set user preferences command sup-------------------
%Function Syntax: ret = SUP(property_name,property_value,...)
%Purpose: set default properties
%Output: success or error
%Input: name and value
%Example: 
% SUP('color','red') --> plot color
% SUP('acolor','red') --> axes color 
% SUP('fcolor','red') --> figure color 
%-------------------------------------------------------------------------

% Dean Whittaker 2008
ret = uinv_setuserpref(varargin{:});

