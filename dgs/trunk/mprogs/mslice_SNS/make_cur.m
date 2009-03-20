function make_cur(fig)

% function make_cur(fig)
% makes current figure the next plotting figure (for plot_cut, disp_spe, plot_slice)

% === determine which figure to make current
if ~exist('fig','var')|(isempty(fig)),
   fig=gcf;
end

% === read figure tag, return if empty of if old figure tab could not be identified
tag=get(fig,'Tag');
tag=tag(~isspace(tag));	% remove all white spaces
if isempty(tag),
   disp('Figure has no current tag. Can not determine which type of plot it contains.');
   disp('No action taken.');
   return;
end
index=findstr(tag,'old_');
if isempty(index),
	disp('The figure tag is not the format old_<figure tag>. No action taken.');
   return;
end   

% === extract original tag of figure 
tag=tag((index(length(index))+4):length(tag));
if isempty(tag),
   disp('The original figure tag appears to be empty. No action taken.');
   return;
end

% === keep all other figures with the same tag
h=findobj('Type','figure','Tag',tag);
if ~isempty(h),
   for i=1:length(h),
      keep(h(i));
   end
end

% === put original tag and enable keep option on current figure
set(fig,'Tag',tag);
h=findobj(fig,'Type','uimenu','Tag','keep');
if ~isempty(h),
   set(h,'Enable','on'),
end

% === disable Make Current uimenu option for current figure
h=findobj(fig,'Type','uimenu','Tag','make_cur');
if ~isempty(h),
   set(h,'Enable','off'),
end

