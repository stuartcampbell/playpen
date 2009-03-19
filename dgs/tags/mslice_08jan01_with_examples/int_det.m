function area=int_det(data,det_index,range)

% function area=int_det(data,det_index)
% integrate data in the det_index'th detector
% in the given energy range 

if ~exist('det_index','var'),
   det_index=1;
end
if ~exist('range','var'),
   range=[min(data.en) max(data.en)];
end
   
en_index=((data.en>=range(1))&(data.en<=range(2)));   
area=trapz(data.en(en_index),data.S(det_index,en_index),2);
