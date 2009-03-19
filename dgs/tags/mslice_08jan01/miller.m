function label_final=miller(hkl)

% produces string label with Miller indices for the (h,k,l) relection

label_final=[];
for i=1:size(hkl,1),
	label=['('];
	for j=1:3,
  		label=[label num2str(hkl(i,j)) ','];
	end
	label=[label(1:length(label)-1) ')'];
   if ~isempty(label_final),
      label_final=str2mat(label_final,label);
   else
      label_final=label;
   end
end