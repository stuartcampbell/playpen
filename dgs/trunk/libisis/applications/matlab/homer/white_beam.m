function rf=white_beam(DSO,varargin)
DSO=add_diffinst(DSO);
ixf_global_var('homer_white_beam','copy','homer')
rf=homer(DSO,varargin{:});
ixf_global_var('homer','remove');
