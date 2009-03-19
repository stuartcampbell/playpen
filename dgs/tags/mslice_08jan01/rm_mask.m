function [index_masked_det,index_masked]=rm_mask(in_data)

% function [out_data,index_masked_det]=rm_mask(in_data)
% remove masked detectors using convention S(phi,w)<=-1e+30 (large negative number)
% index_masked (ndet,1) matrix of 0 and 1 to show which detectors have been masked

if ~exist('in_data','var'),
   help rm_mask;
   return
end
nulldata=-1e+30;
index_masked=(isnan(in_data.S)|(in_data.S<=nulldata)); % masked pixels
temp=sum(index_masked,2); % sum along energy direction
index_masked_det=(temp==size(in_data.S,2)); % whole detector masked
index_masked(index_masked_det,:)=[]; % remove completely these masked detectors
disp(['Unmasked : masked detector groups = ' num2str(size(in_data.S,1)-sum(index_masked_det)) ...
      ' : ' num2str(sum(index_masked_det))]);