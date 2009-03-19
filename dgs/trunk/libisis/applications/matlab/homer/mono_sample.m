function rf=mono_sample(DSO,chopper_type,varargin)
DSO=add_chopinst(DSO);
if ischar(chopper_type)
  DSO=add_item(DSO,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',chopper_type);
end
ixf_global_var('homer_mono_sample','copy','homer')
rf=homer(DSO,varargin{:});
ixf_global_var('homer','remove');