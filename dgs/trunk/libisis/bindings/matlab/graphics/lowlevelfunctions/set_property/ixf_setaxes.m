function ixf_setaxes(is_x_y_z,isXYZ,lo,logap,hi,higap)
%----------------------------------------------------
%Function Syntax: ixf_setaxes(is_x_y,isXY,lo,logap,hi,higap)
%Purpose: Shift X or Y axes by gap specified
%Output: None
%Input: isXY for denoting for (0)X or (1)ediY axes, gap to denote
%how much to shift
%Example: 
% ixf_setaxes('isXY',1,'logap',1,'higap',2) --> for y axes ylo = 1 yhi = 2
% ixf_setaxes('isXY',0,'logap',1,'higap',2) --> for x axes xlo = 1 xhi = 2
%-----------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

% ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_setaxes,nargin);

flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.true)
%figure is there
%store initial values of limits
    inix = get(gca,'xlim');
    iniy = get(gca,'ylim');
    iniz = get(gca,'zlim');
    inic = get(gca,'clim');
else
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

if logap==higap
    useOldLimits=IXG_ST_STDVALUES.true;
else
    useOldLimits=IXG_ST_STDVALUES.false;
end

if (isXYZ == IXG_ST_STDVALUES.x)
    %denotes X axes
    [xloxhi] = xlim;
    if useOldLimits==IXG_ST_STDVALUES.true;
        xlo=min(inix);
        xhi=max(inix);
    else
     xlo = logap;
     xhi = higap;
    end
    %set new limits
    set(gca,'xlim',[xlo xhi]);
    %store if new figure
    if (flag == IXG_ST_STDVALUES.false) %figure was not there, hence store the new figure dimensions
        inix = get(gca,'xlim');
        iniy = get(gca,'ylim');
        iniz = get(gca,'zlim');
        inic = get(gca,'clim');
    end
    %store the initial limits
    %set(gca,'ylim',[iniy(1) iniy(2)]);
elseif (isXYZ == IXG_ST_STDVALUES.y)
    [yloyhi] = ylim;
    if useOldLimits==IXG_ST_STDVALUES.true;
        ylo=min(iniy);
        yhi=max(iniy);
    else
     ylo = logap;
     yhi = higap;
    end
    set(gca,'ylim',[ylo yhi]);
    if (flag == IXG_ST_STDVALUES.false) %figure was not there, hence store the new figure dimensions
        inix = get(gca,'xlim');
        iniy = get(gca,'ylim');
        iniz = get(gca,'zlim');
        inic = get(gca,'clim');
    end

elseif (isXYZ == IXG_ST_STDVALUES.z)
    [zlozhi] = zlim;
    if useOldLimits==IXG_ST_STDVALUES.true;
        zlo=min(iniz);
        zhi=max(iniz);
    else
         zlo = logap;
         zhi = higap;
    end
    set(gca,'zlim',[zlo zhi]);
    if (flag == IXG_ST_STDVALUES.false) %figure was not there, hence store the new figure dimensions
        inix = get(gca,'xlim');
        iniy = get(gca,'ylim');
        iniz = get(gca,'zlim');
        inic = get(gca,'clim');
    end
elseif (isXYZ == IXG_ST_STDVALUES.c)
    if useOldLimits==IXG_ST_STDVALUES.true;
        clo=min(inic);
        chi=max(inic);
    else
     clo = logap;
     chi = higap;
    end
    set(gca,'clim',[clo chi]);
    if (flag == IXG_ST_STDVALUES.false) %figure was not there, hence store the new figure dimensions
        inix = get(gca,'xlim');
        iniy = get(gca,'ylim');
        iniz = get(gca,'zlim');
        inic = get(gca,'clim');
    end
    %store the initial limits
    %set(gca,'xlim',[inix(1)     inix(2)]);
end
