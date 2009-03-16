function sum_det=sum2det(det_num,sum_spec)

% function sum_det=sum2det(det_num,sum_spec)
% input    det_num (ndet,1)
%          sum_spec (spec_num, whitevan_int, whitevan_err)) (nspec,3)
% return   sum_det (det_num, whitevan_int, whitevan_err) (ndet,3)

sum_det=det_num;
ndet=length(det_num);
sum_det(:,2:3)=NaN*ones(ndet,2);

% alternative algorithm for PSD's only
%j=det2spec(det_num-400);
%sum_det(:,2)=sum_spec(j,2);
%sum_det(:,3)=sum_spec(j,3);

for i=1:ndet,
   j=find(sum_spec(:,1)==det2spec(det_num(i,:)));
   if ~isempty(j),
      sum_det(i,2)=sum_spec(j,2);
      sum_det(i,3)=sum_spec(j,3);
   else
      disp(['Detector ' num2str(phx(i,1)) ' not associated with spectrum with a white vanadium integral.']);
   end
end      