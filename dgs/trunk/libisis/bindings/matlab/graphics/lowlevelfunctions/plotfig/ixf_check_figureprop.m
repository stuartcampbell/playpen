function ixf_check_figureprop(name,data)
%---------------------------------------
%check the figure properties
%
%purpose: to check figure properties, and decide
%whether to create new or plot on old figure only
%input : name and tag of figure
%output : none
%example : ixf_check_figureprop('tobie','1d')
%---------------------------------------

IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
currflag = ixf_checkinit(IXG_ST_STDVALUES.currentfigure);

current_plots=ixf_checkforfigure(name,data);
if current_plots
    figure(current_plots(1));
else
    ixf_create_figure(name,data);
end


%---------------------------------------
function res = ixf_oldnew_figure(IXG_ST_STDVALUES)
%whether to create new or replace on old figure
%---------------------------------------
IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
%get keep property
keep = getappdata(gcf,'keep');

if keep == IXG_ST_STDVALUES.false
    %if it is false, you can use it    
    %before using
    %check
    %the data which you are drawing and the figure properties
    
    res = IXG_ST_STDVALUES.old;
else
    %if it is true, you cannot
    %this means new figure needs to be created
    res = IXG_ST_STDVALUES.new;
end

%---------------------------------------
function res = ixf_check_figuredata(name,data)
%check figure data (name and tag)
%---------------------------------------
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
%tag
% name = get(gcf,'name');
id = get(gcf,'tag');
%object type
appname = get(gcf,'name');

if ( strcmp(id,data) && strcmpi(appname,name))
    %     onedflag = IXG_ST_STDVALUES.oned;
    %     %convert type to 1d or 2d
    %       
    %         %plot it on gcf
    %returning old says figure is 1d or 2d
    res = IXG_ST_STDVALUES.old;
else
    %plot on new figure
    res = IXG_ST_STDVALUES.new;
end

%---------------------------------------
%this creates new figure with properties
function res = ixf_create_figure(name,data)
%depending on data it will assign tag
%this figure must have the menu of chng/keep items
%---------------------------------------
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
%chk current figure
chkf = get(0,'currentfigure');
if (~isempty(chkf)) && ixf_check_graphicfigure('handle',chkf)
    position = get(chkf,'position');
    fh = figure('name',name,'tag',data,'position',position);
else
    fh = figure('name',name,'tag',data);
end
ixf_menutb_crosshair;
ixf_menutb_rectxy;
ixf_keepcurr_menu('hdl',gcf);
ixf_setfigureidentifier('hdl',gcf);
res = IXG_ST_STDVALUES.true;
return;


%----------------------------------------------------------------
function [name,data] = ixf_ret_data(varargin)
%return name and data from the arguments passed in
%----------------------------------------------------------------

totArg = numel(varargin);
data = '';
name = '';
%check total args
for iArg = 1:totArg
    %parse, args must be character
    if (ischar(varargin{iArg}))
        switch(lower(varargin{iArg}))
            case 'tag'
                data = varargin{iArg + 1};    
            case 'name'
                name = varargin{iArg + 1};    
        end
    end
end


