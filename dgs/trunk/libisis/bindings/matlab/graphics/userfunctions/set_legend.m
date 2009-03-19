function ret_out = set_legend(varargin)
%----help for gtk set legend command sl--------------
%   Function Syntax: ret = SL(string_1,string_2,...)
%   Purpose: Sets a legend on the current plot usign the input strings
%   Output: flag to denote property set (true or false)
%   Input: strings 
%   Example: 
%   ret = SL('y=sinx','y=cosx')
%
%   Note: give the strings according to plot in descending order
%   (recent plot first and then onwards...)
%
%   Hence if you have plot first error bar in red, second histogram 
%   in green, then execute the legend command as
%   ret = SL('y=histogram','y=errorbar')
%-----------------------------------------------------

% Dean Whittaker 2008

ret = ui_setlegend(varargin{:});

if nargout > 0
    ret_out = ret;
end
