function slice_out=add_slice(slices,weights)

% function slice_out=add_slice(slices,weights)
% slices={slice1, slice2}; weights=[w1 w2];
% adds slices with given weighting factors w1 w1 ... (could be \muAhrs) 
% Radu Coldea 02-Oct-1999

slice_out=[];
% === return if syntax wrong, slices are with different bins or if not all weights are provided
if ~exist('slices','var'),
   help add_slice;
   return;
end
if isempty(slices),
   disp('All slices are empty.');
   return;
end
if ~exist('weights','var')|isempty(weights),
   disp(['No weighting factors given. Add_slice command not executed.']);
   help add_slice;
   return;
end
if length(slices)~=length(weights),
   disp('Weights and slices parameters not compatible');
   return;
end
if ~iscell(slices)|~isnumeric(weights),
   disp('Wrong calling syntax. Add_slice command not executed.');
   help add_slice; 
   return;
end
  
% === add slices one by one
for i=1:length(weights),
	if ~isempty(slices{i}),      
      index=~isnan(slices{i}.intensity);	% (nvx,nvy) is 1 where bin has data and 0 where bin has no data in current slice
   	slices{i}.intensity(~index)=0;
   	slices{i}.error_int(~index)=0;
   	if ~exist('cumm_index','var'),	% if this is the first non-empty slice to be added, initialize cumm_index, cumm_intensity, cumm_err2 ...
      	slice_out=slices{i};
      	slice_out.title{1}=['combined slices ' firstword(slices{i}.title{1})];
      	cumm_index=index; 	% cummulative index 1 (data) 0 (nulldata) for all slices added so far  	
      	cumm_weight=weights(i)*index;	% (nvx,nvy) contains cummulative weights of each bin, 0 if bin is 'nulldata' in all slices added so far    
      	cumm_intensity=weights(i)*(index.*(slices{i}.intensity));	% cummulative summation of intensities, is 0 where cumm_index is 0
      	cumm_error2=(weights(i)^2)*(((slices{i}.error_int).^2).*index);	% cummulative ERR2 summation, has 0 where cumm_index is 0
      else
         if (length(slice_out.vx)~=length(slices{i}.vx))|any(slice_out.vx~=slices{i}.vx),
         	disp(['Warning: grid along x of slice ' num2str(i) ' not equivalent with grid of previous slices.']);
         	disp(['Add_slice command not executed.']);
         	return;   
      	end
         if (length(slice_out.vy)~=length(slices{i}.vy))|any(slice_out.vy~=slices{i}.vy),
         	disp(['Warning: grid along y of slice ' num2str(i) ' not equivalent with grid of previous slices.']);
         	disp(['Add_slice command not executed.']);
         	return;   
      	end            
      	cumm_index=cumm_index|index;	% one bin has data if it either has had data 
      		% in one of the previous slices or it has data in this current slice
      	cumm_weight=cumm_weight+weights(i)*index;	% (nvx,nvy)
      	cumm_intensity=cumm_intensity+weights(i)*(index.*(slices{i}.intensity));	% (nvx,nvy)
	   	cumm_error2=cumm_error2+(weights(i).^2)*(((slices{i}.error_int).^2).*index);	% (nvx,nvy)
      	slice_out.title{1}=[slice_out.title{1} '+' firstword(slices{i}.title{1})];      
      end
   end
end

% === put 'nulldata' with 0 error bar in final data set for bins which were 'nulldata' in ALL data sets
if ~isempty(slice_out),
	slice_out.intensity=NaN*ones(size(slices{i}.intensity));
	slice_out.intensity(cumm_index)=cumm_intensity(cumm_index)./(cumm_weight(cumm_index));
	slice_out.error_int=zeros(size(slices{i}.error_int));
	slice_out.error_int(cumm_index)=sqrt(cumm_error2(cumm_index))./(cumm_weight(cumm_index));
end