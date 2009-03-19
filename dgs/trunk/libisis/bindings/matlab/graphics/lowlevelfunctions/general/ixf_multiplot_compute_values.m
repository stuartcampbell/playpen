%-------------------------------------------------------------------
function [xdata,ydata,zdata] = ixf_multiplot_compute_values(specData)
%function syntax:ixf_multiplot_compute_values(specData)
%purpose:compute values for multiplot purpose
%input:1d dataset object
%output:x,y,z values
%example: ixf_multiplot_compute_values(onedataset_object)
%-------------------------------------------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%calculate size of spectrum
chkSize = size(specData);
if (isempty(chkSize))
    ixf_display_error(IXG_ST_ERROR.no_size);
end
%calc size
nx = length(specData(IXG_ST_STDVALUES.start).x);
ny = length(specData(IXG_ST_STDVALUES.start).signal);


%assign initial value to ydata
ydata = [IXG_ST_STDVALUES.ystart];
%if spec data size greater than 1, assign ydata additional values
if (chkSize(IXG_ST_STDVALUES.col) > 1)        
    for iValue=1:chkSize(IXG_ST_STDVALUES.col)-1
        ydata = [ydata iValue+1];
    end    
end

%calc. zdata values
for iValue = IXG_ST_STDVALUES.start:chkSize(IXG_ST_STDVALUES.col)
    try%get zdata
        zdata(iValue,:) = specData(iValue).signal;    
    catch
        display('Error: Dimensions of the data in the IXTdataset_1d objects are inconsistent, multiplot values can not be calculated')
        xdata = 0; ydata = 0; zdata = 0;
        return
    end
end

%get x data from spectrum
%this is for average value
if ( nx ~= ny) %hist data
    xsize = size(specData(IXG_ST_STDVALUES.start).x);
    xdata(IXG_ST_STDVALUES.start,:) = ( specData(IXG_ST_STDVALUES.start).x(IXG_ST_STDVALUES.start + 1:xsize(IXG_ST_STDVALUES.col)) + specData(IXG_ST_STDVALUES.start).x(IXG_ST_STDVALUES.start:xsize(IXG_ST_STDVALUES.col)-1) ) / 2;
elseif (nx == ny) %point data
    xdata = specData(IXG_ST_STDVALUES.start).x;
end
