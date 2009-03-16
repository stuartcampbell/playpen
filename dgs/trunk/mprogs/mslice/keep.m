function keep(fig)

% function keep(fig)
% keeps current figure (default) or given figure 
% and next plot will appear on a different figure 

% === determine which figure to keep
if ~exist('fig','var')|(isempty(fig)),
   fig=gcf;
end

% === read figure tag, return if empty of if figure already kept
tag=get(fig,'Tag');
tag=tag(~isspace(tag));
if isempty(tag),
   disp('Figure has no current tag. No action taken.');
   return;
end
if (length(tag)>=4) & strcmp('old_',tag(1:4))
   disp('A figure can not be kept twice. No action taken.');
   return;
end   

% === keep figure, remember old tag and disable keep uimenu option
set(fig,'Tag',['old_' tag]);
h=findobj(fig,'Type','uimenu','Tag','keep');
if ~isempty(h),
   set(h,'Enable','off'),
end

% === enable Make Current uimenu option, if present
h=findobj(fig,'Type','uimenu','Tag','make_cur');
if ~isempty(h),
   set(h,'Enable','on'),
end

