function data_out=mask(data_in,det_groups)

% function data_out=mask(data_in,det_groups)

data_out=data_in;
msk=[];
for i=det_groups,
   index=find(i==data_in.det_group);
   if ~isempty(index),
      msk=[msk;index];
   end
end
if ~isempty(msk),
	data_out.S(msk,:)=[];
   data_out.ERR(msk,:)=[];
   data_out.det_theta(msk)=[];
   data_out.det_psi(msk)=[];
   data_out.det_dtheta(msk)=[];
   data_out.det_dpsi(msk)=[];
   if isfield(data_in,'det_num'),
      data_out.det_num(msk)=[]; 
   end
   data_out.det_group(msk)=[];
end
