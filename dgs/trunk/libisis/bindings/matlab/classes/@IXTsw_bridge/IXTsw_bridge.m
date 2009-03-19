function sw_bridge = IXTsw_bridge( varargin )
%  IXTsw_bridge(IXTbase,
%  [spec_no],[total_work],[work_ind],[work_no]);
sw_bridge.base = IXTbase;
sw_bridge.spec_no=[int32(1)];
sw_bridge.total_work=[int32(1)];
sw_bridge.work_ind=[int32(1)];
sw_bridge.work_no=[int32(1)];
sw_bridge= class(sw_bridge,'IXTsw_bridge');    
if nargin > 0           
    sw_bridge = libisisexc('IXTsw_bridge','create',sw_bridge,varargin);
end