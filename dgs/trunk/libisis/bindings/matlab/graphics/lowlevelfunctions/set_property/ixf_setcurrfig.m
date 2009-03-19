
function [res] = ixf_setcurrfig(varargin)
%--------------------------------------------------------------------
%Function Syntax: res = ixf_setcurrfig()
%Output: true or false depending on whether the curr fig can be set or not
%Input: figure handle, figure/application name and dim(1d or 2d figure). 
%Mostly figure handle are like 1 or 2 or 3 etc
%Usage: 
%ixf_setcurrfig(1) --> set figure handle 1
%ixf_setcurrfig(1,'Genie') --> makes figure handle 1 as current figure
%ixf_setcurrfig(1,'Genie','1d') --> makes figure handle 1 as current figure if
%1dimension figure is there and name is Genie
%--------------------------------------------------------------------


%global structure

[IXG_ST_STDVALUES, IXG_ST_INTERFACEVALIDATION]= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES','IXG_ST_INTERFACEVALIDATION');

totArg = numel(varargin);


iArg = 1;
%decider
figureHandle_ = get(0,'currentfigure');
if (isempty(figureHandle_))
    % no figure
    res = IXG_ST_STDVALUES.false;
    return;
end       
    
% switch(svalue)
%     case IXG_ST_ISTYLE.namevalue
%         ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig_nv,totArg,'range');
if (totArg == IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig_nv(iArg))
    [res] = ixf_setcurrfig_handle(varargin{:});
elseif (totArg == IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig_nv(iArg+1))
    [res] = ixf_setcurrfig_nametag(varargin{:});
elseif (totArg == IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig_nv(iArg+2))
    [res] = ixf_setcurrfig_handlenametag(varargin{:});
else
    res = IXG_ST_STDVALUES.false;
end
%     case IXG_ST_ISTYLE.default
% %         ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig,totArg,'range');
%         if (totArg == IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig(iArg))
%             [res] = ixf_setcurrfig_handle('hdl',varargin{:});    
%         elseif (totArg == IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig(iArg+1))
%             [res] = ixf_setcurrfig_nametag('appname',varargin{iArg},'tag',varargin{iArg+1});        
%         elseif (totArg == IXG_ST_INTERFACEVALIDATION.ixf_setcurrfig(iArg+2))
%             [res] = ixf_setcurrfig_handlenametag('hdl',varargin{iArg},'appname',varargin{iArg+1},'tag',varargin{iArg+2});  
%         else
%             res = IXG_ST_STDVALUES.false;
%         end
%     otherwise
% %         ixf_display_error(IXG_ST_ERROR.wrong_value,'istyle');   
% end

%subfunctions list
%-----------------------------------
function hdlret = ixf_setcurrfig_handle(id,hdl)
%on handle 
%-----------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');
one = 1;

if (~isnumeric(hdl))
    ixf_display_error(IXG_ST_ERROR.invalid_number,'hdl');
end
lfigureHandle_ = findobj(0,'Type','Figure');
%check range of available figure handles, then set it
%else give error
index = find(lfigureHandle_ == hdl);

%if yes then make the handle current else return false
if (isempty(index))
    hdl = IXG_ST_STDVALUES.false;
else
    %chk is it my graphic window or not
    chkflag = ixf_checkandset('hdl',hdl);
end    
hdlret = hdl;


%-----------------------------------
function hdlret = ixf_setcurrfig_nametag(app,appname,t,tag)
%on name,tag
%-----------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES]= ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');
one = 1;
if (~ischar(appname))
    ixf_display_error(IXG_ST_ERROR.invalid_character,'appname');
end
lfigureHandle_ = findobj(0,'Type','Figure','Name',appname,'Tag',tag);

if (isempty(lfigureHandle_))
    hdl = IXG_ST_STDVALUES.false;
else
    hdl = lfigureHandle_(one);
    chkflag = ixf_checkandset('hdl',hdl);
end   
hdlret = hdl;


%-----------------------------------
function hdlret = ixf_setcurrfig_handlenametag(id,hdl,app,appname,t,tag)
%on name,tag and handle
%-----------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES]= ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');
one = 1;
if (~ischar(tag))
    ixf_display_error(IXG_ST_ERROR.invalid_character,'tag');
end

lfigureHandle_ = findobj(0,'Type','Figure','Name',appname,'Tag',tag);
%check range of available figure handles, then set it
%else give error
index = find(lfigureHandle_ == hdl);

%if yes then make the handle current else return false
if (isempty(index))
    hdl = IXG_ST_STDVALUES.false;
else
    chkflag = ixf_checkandset('hdl',hdl);
end    
hdlret = hdl;

%-----------------------------------
function chkflag = ixf_checkandset(oid,hdl)
%check my graphic window
%if yes then set it as current
%if no return false
%-----------------------------------
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

chkflag = ixf_check_graphicfigure('hdl',hdl);
if (chkflag == IXG_ST_STDVALUES.true)
    set(0,'Currentfigure',hdl);   
    %keep is now false
    ixf_setapplicationdata('hdl',hdl,'keep',IXG_ST_STDVALUES.false);
    %remove hold
    ixf_setlabel_hold('hdl',gcf,'flag','false');
    chkflag = hdl;
else
    chkflag = IXG_ST_STDVALUES.false;
end