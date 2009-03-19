
function ixf_figureclose()
%-------help for GTK IXF_figureclose---------------------------------------
%
% purpose: to run when a figure closes within the GTK program, should only
% be called from a figure CloseRequestFcn callback.
%
% call syntax: IXF_figureclose
%
% inputs: none,     outputs: none
%
%--------------------------------------------------------------------------

% get globals

%idea - dialogue asking if the user wants to delete the name tag too.
%Though for constructing other applications on top this might be a bad
%idea. Perhaps an option.
delete(gcf);

% set globals
