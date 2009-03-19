function keep_figure(varargin)
%----------help for gtk keep figure command kf ----------------------------
%Function Syntax:    KF(fig_handle)        or
%                    kf()
%Purpose: keep figure, it will not be used for new plots
%
%Input: 
%       figure handle:
%           may be a single value or array.
%
%Output: none
%
%Example: KF(1) where 1 is figure handle or simply KF, where the figure is
%the current figure.

% updated 20/03/2007 by Dean Whittaker
%---------/----------------------------------------------------------------
if nargin==0                    % if no argument
    if isempty(findall(0,'type','figure'))
        error('No figure exists');      % if no active figure, error and close
    else
        h=gcf;
        ui_keepcmd(h);          % if figure exists, then perform keepcmd on it
    end
else
    ui_keepcmd(varargin{:});    % else do standard keepcmd 
end
