function ws_bridge = IXTws_bridge( varargin )
%  IXTws_bridge(IXTbase,
%  [work_no],[total_spec],[spec_ind],[bad_spectra_flag],[spec_no]);
ws_bridge.base = IXTbase;
ws_bridge.work_no=[int32(1)];
ws_bridge.total_spec=[int32(1)];
ws_bridge.spec_ind=[int32(1)];
ws_bridge.bad_spectra_flag=[int32(1)];
ws_bridge.spec_no=[int32(1)];
ws_bridge= class(ws_bridge,'IXTws_bridge');    
if nargin > 0           
    ws_bridge = libisisexc('IXTws_bridge','create',ws_bridge,varargin);
end