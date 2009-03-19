function data = IXTdata( varargin )
% ! Create an IXTdata object
% ! REQUIRED INPUT PARAMETERS
% ! data  = IXTdata(base, IXTdataset_2d, IXTworkspace, IXTbridge); 
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTdata
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
%     datasets  IXTdataset_2d
%     workspace  IXTworkspace
%     bridge  IXTbridge
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	

    % if no input arguments, create a default object
    data.base = IXTbase;
    data.datasets = [IXTdataset_2d];
    data.workspace = IXTworkspace;
    data.bridge = IXTbridge;
    data = class(data,'IXTdata');
if (nargin > 0)
    data = libisisexc('IXTdata','create',data,varargin);
end
