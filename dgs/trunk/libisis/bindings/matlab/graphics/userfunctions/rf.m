function rf(varargin)
%----------help for gtk release figure command rf ----------------------------
% Function Syntax:    RF(fig_handle)        or
%                    RF()
% Purpose:   release figure, if the figure was being held (through the command
%           kf) then it will no longer be held. 
%
% Input: 
%       figure handle:
%           may be a single value or array.
%
% Output: none
%
% Example: RF(1) where 1 is figure handle or simply RF, where the figure is
% the current figure.

% updated 07/09/2007 by Dean Whittaker, Originally Pranav Amin
%---------/----------------------------------------------------------------
if nargin==0                    % if no argument
    if isempty(findall(0,'type','figure'))
        error('No figure exists');      % if no active figure, error and close
    else
        h=gcf;
        ui_releasecmd(h);          % if figure exists, then perform keepcmd on it
    end
else
    ui_releasecmd(varargin{:});    % else do standard keepcmd 
end
