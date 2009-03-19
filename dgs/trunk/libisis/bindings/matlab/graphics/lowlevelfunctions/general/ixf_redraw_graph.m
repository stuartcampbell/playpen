function ixf_redraw_graph(axesHandle_)
%-----------help for GTK ixf_redraw_graph----------------------------------
% purpose: actions to perform to redraw graphs after alterations. Currently
% used when alterations are made to the axes limits only but can be
% extended 
%
% call syntax: ixf_redraw_graph(axesHandle_)
%
% inputs: axesHandle_ - axes to be redrawn,     outputs: none
%
%------------updated 25/08/2006, Dean Whittaker----------------------------

%---------------error checking, get handles -------------------------------

IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

if nargin==0
    error('must provide handle to redraw')
end

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(axesHandle_);

for i=1:length(plotHandle_)
    [xyzdata, plot_type] = ixf_plotdata('get',plotHandle_(i),'xyzdata', 'plot_type');
    
   switch plot_type
       case IXG_ST_STDVALUES.contour_type
           contour_replot(plotHandle_(i), axesHandle_, xyzdata)
           break
       case {IXG_ST_STDVALUES.surface_type IXG_ST_STDVALUES.waterfall_type}
           surface_replot(plotHandle_(i), axesHandle_, xyzdata)
       case IXG_ST_STDVALUES.area_type
           area_replot(plotHandle_(i), axesHandle_, xyzdata)
       case {IXG_ST_STDVALUES.points_type IXG_ST_STDVALUES.stem_type IXG_ST_STDVALUES.twod_line_type}
           stem_replot(plotHandle_(i), axesHandle_, xyzdata);
       case IXG_ST_STDVALUES.oned_type
           oned_replot(plotHandle_(i), axesHandle_, xyzdata);
       case {IXG_ST_STDVALUES.patch_type IXG_ST_STDVALUES.surface_patch_type}
           patch_replot(plotHandle_(i),axesHandle_,xyzdata);
       otherwise
           warning('ixf_redraw_graph does not recognise the graph type, it has not been redrawn')
   end
   
  
   % else don't do anything
end

% check for log color, and log if necessary.

clogflag = ixf_plotdata('get',axesHandle_,'clog_flag');

if ~isempty(clogflag) && clogflag
       ixf_plotdata('set',axesHandle_,'clog_flag',false)
       ui_set_cscale(axesHandle_,'log');
end

% check for any ui_controls in the figure and set as necessary.

% the set of sliders exists 
ixf_color_slider('figure',figureHandle_,'option','update');

%------------- contour replot----------------------------------------------
%
% replots a contour plot in GTK
%
% contour replot(plotHandle_,axesHandle,userdata)
%
% userdata must contain x,y and z data.. handles are to the plots to be
% altered
%
%--------------------------------------------------------------------------

function contour_replot(plotHandle_,axesHandle_,userdata)

xlim = get(axesHandle_,'xlim');     ylim=get(axesHandle_,'ylim');
zlim = get(axesHandle_,'zlim');
[xdata, ydata, zdata, cdata]=get_values(userdata.x,userdata.y,userdata.z,userdata.z,xlim,ylim,zlim);


%------------- surface_replot----------------------------------------------
%
% replots a suraface plot in GTK
%
% surface_replot(plotHandle_,axesHandle,userdata)
%
% userdata must contain x,y and z data.. handles are to the plots to be
% altered
%
%--------------------------------------------------------------------------

function surface_replot(plotHandle_,axesHandle_,userdata)
xlim = get(axesHandle_,'xlim');     ylim=get(axesHandle_,'ylim');
zlim = get(axesHandle_,'zlim');
[xdata, ydata, zdata, cdata]=get_values(userdata.x,userdata.y,userdata.z,userdata.z,xlim,ylim,zlim);
set(plotHandle_,'xdata',xdata,'ydata',ydata,'zdata',zdata,'cdata',cdata);

%---------------- area_replot----------------------------------------------
%
% replots an area plot in GTK
%
% area_replot(plotHandle_,axesHandle,userdata)
%
% userdata must contain x,y and z data.. handles are to the plots to be
% altered
%
%--------------------------------------------------------------------------

function area_replot(plotHandle_,axesHandle_,userdata)
xlim = get(axesHandle_,'xlim');     ylim=get(axesHandle_,'ylim');
zlim = get(axesHandle_,'zlim');     zdata = 0*userdata.z;

[xdata, ydata, zdata, cdata]=get_values(userdata.x,userdata.y,zdata,userdata.z,xlim,ylim,zlim);
set(plotHandle_,'xdata',xdata,'ydata',ydata,'cdata',cdata);



 %-----------------oned_replot---------------------------------------------
 %
 % replots a oned graph in GTK
 %
 % oned_replot(plotHandle_,axesHandle_,userdata)
 %
 % userdata must contain x, y data.. handles are to the plots to be altered
 %
 %-------------------------------------------------------------------------
 
function oned_replot(plotHandle_,axesHandle_,userdata)
        xlim = get(axesHandle_,'xlim');     ylim = get(axesHandle_,'ylim');
        xdata = userdata.x;                 ydata = userdata.y;
        
        index=find(~(xdata<=max(xlim) & xdata>=min(xlim)));
        xdata(index) = NaN;
        ydata(index) = NaN;
        
        set(plotHandle_,'xdata',xdata,'ydata',ydata);
        
%----------------stem_replot-----------------------------------------------
%
% replots a stem graph in GTK
%
% stem_replot(plotHandle, axesHandle_, userdata)
%
% userdata must contain x,y,z data... Handles are to the plots to be
% altered
%
%--------------------------------------------------------------------------

function stem_replot(plotHandle_,axesHandle_,userdata)
       xlim = get(axesHandle_,'xlim');      ylim = get(axesHandle_,'ylim');
       zlim = get(axesHandle_,'zlim');   
       
       [xdata, ydata, zdata] = get_values(userdata.x,userdata.y, userdata.z, userdata.z,...
          xlim, ylim, zlim);
            
      set(plotHandle_,'xdata',xdata,'ydata',ydata,'zdata',zdata);


%---------------- patch_replot----------------------------------------------
%
% replots an patch area plot in GTK
%
% patch_replot(plotHandle_,axesHandle,userdata)
%
% userdata must contain vertex, faces and z data.. handles are to the plots to be
% altered
%
%--------------------------------------------------------------------------

function patch_replot(plotHandle_,axesHandle_,xyz)

set(plotHandle_,'faces',xyz.faces,'vertices',xyz.vertices,'FaceVertexCData',xyz.z);

% patch does not require a replot 


%------------- get_values--------------------------------------------------
%
% gets the values of data outside the limits and changes them to NaN
%
% get_values(xdata,ydata,zdata,xlim,ylim,zlim)
%
% xdata,ydata and zdata must be the same size, the limits must be a 2 number array 
%
%--------------------------------------------------------------------------
            
function [xdata,ydata,zdata,cdata] = get_values(xdata,ydata,zdata,cdata,xlim,ylim,zlim)
       
        % get index of all data outside of x,y,z boundaries
        
        index=find(~(xdata<=max(xlim) & xdata>=min(xlim) & ...
           ydata<=max(ylim) & ydata>=min(ylim) )); 
   
        % Reset the data to match the limits
         %   cdata(index)=NaN;       
            xdata(index)=NaN;
            ydata(index)=NaN;       
            zdata(index)=NaN;
            cdata(index)=NaN;
            
        % don't reset z data      
