function maskmore(sum_filename,msk_filename,threshold,final_msk)

% function data=maskmore(sum_filename,msk_filename,threshold,final_msk)

% === load white vanadium sum file
if ~exist('sum_filename','var'),
   help maskmore;
   return;
end
data_psd_ew=load_sum(sum_filename,'pix_ew');
data_psd_ns=load_sum(sum_filename,'pix_ns');
if isempty(data_psd_ew)&isempty(data_psd_ns),
   return;
end

% === load mask file 
if ~exist('msk_filename','var'),
   help maskmore;
   return;
end
msk=load_msk(msk_filename);
msk_psd_ew=load_msk(msk_filename,'pix_ew');
msk_psd_ns=load_msk(msk_filename,'pix_ns');

if ~isempty(data_psd_ew),
	spec_num=data_psd_ew(:,1);	% spectrum numbers for E&W psd banks
	index=zeros(max(spec_num),1);
	index(spec_num)=(1:length(spec_num))';
	data_psd_ew(index(msk_psd_ew),:)=[];	% eliminate masked spectra
   if ~isempty(data_psd_ew),
      av=mean(data_psd_ew(:,2));      
      disp(sprintf('E+W banks average %10.3g',av));
      i=(data_psd_ew(:,2)<(threshold*av));	%index spectra with intensity lower than a threshold value compared to the average 
      msk=[msk ; data_psd_ew(i,1)];
      data_psd_ew(i,:)=[];      
      av=mean(data_psd_ew(:,2));      
      disp(sprintf('After further masking E+W average %10.3g',av));   
   end
end

if ~isempty(data_psd_ns),
	spec_num=data_psd_ns(:,1);	% spectrum numbers for E&W psd banks
	index=zeros(max(spec_num),1);
	index(spec_num)=(1:length(spec_num))';
	data_psd_ns(index(msk_psd_ns),:)=[];	% eliminate masked spectra
   if ~isempty(data_psd_ns), 
      av=mean(data_psd_ns(:,2));      
      disp(sprintf('N+S banks average %10.3g',av));
      i=(data_psd_ns(:,2)<(threshold*av));	%index spectra with intensity lower than a threshold value compared to the average 
      msk=[msk ; data_psd_ns(i,1)];
      data_psd_ns(i,:)=[];
      av=mean(data_psd_ns(:,2));      
      disp(sprintf('After further masking N+S average %10.3g ',av));   
   end
end

save_msk(msk,final_msk);
sort_msk(final_msk,final_msk);

