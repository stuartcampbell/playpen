function bridge = IXTbridge( varargin )
% ! Create an IXTbridge object
% ! REQUIRED INPUT PARAMETERS
% ! bridge  = IXTbridge(base, IXTws_bridge,  IXTsw_bridge); 
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTbridge
% !
% !	base			IXTbase				Name of entry in NeXus file
%     ws_bridge  IXTws_bridge
%     sw_bridge  IXTsw_bridge
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	

    % if no input arguments, create a default object
    bridge.base = IXTbase;
    bridge.ws_bridge = IXTws_bridge;
    bridge.sw_bridge = IXTsw_bridge;
    bridge = class(bridge,'IXTbridge');
if (nargin > 0)
    bridge = libisisexc('IXTbridge','create',bridge,varargin);
end
