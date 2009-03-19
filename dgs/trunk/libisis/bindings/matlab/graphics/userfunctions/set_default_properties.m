function ret_out = set_default_properties(varargin)
%---------help for gtk set default properties sdp command------------------
%Function Syntax: SDP('name', 'tag', property_name, property_value, ...)
%Purpose: set default properties for names and tags
%Output: success or error
%Input: plot name, plot tag, property-value pairs
%Example: 
% SDP('name','tag','color','red') --> plot color
% SDP('deans','plot','acolor','red') --> axes color 
% SDP('my','surface','fcolor','red','tag','tobie') --> figure color and
% name as tobie
%
%-------------------------------------------------------------------------

% Dean Whittaker 2008

ret = uinv_setdefaultprop(varargin{:});

if nargout > 0
    ret_out = ret;
end