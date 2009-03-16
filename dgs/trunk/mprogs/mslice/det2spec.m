function spec_num=det2spec(det_num)

% function spec_num=det2spec(det_num)
% converts detector numbers into spectrum numbers for the HET psd's
% (ndet,1)

a=floor(det_num/100);
b=det_num-100*a;
west=((a>=10)&(a<=22));
east=((a>=60)&(a<=72));
index=(~(west|east))|(b<1)|(b>64);
if sum(index)>0,
   disp(['Detector numbers incompatible with HET psd detector numbers. Command not executed.']);
   det_num(index),
   spec_num=[];
   return
end
a(west)=a(west)-9; % a = tube number 
a(east)=a(east)-59;
spec_num=det_num;
spec_num(west)=(a(west)-1)*64+b(west)+400;
spec_num(east)=(a(east)-1)*64+b(east)+1232;