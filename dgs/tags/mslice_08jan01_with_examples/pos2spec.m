function s=pos2spec(data,spectramap)

% function s=pos2spec(x,y)
% transform position into spectrum number 
% data=[x y] (nspectra,2)
% map ='HET PSD-4m'

if isempty(data),
   disp(sprintf('Spectra data empty. Return.'));
   matrices=[];
   return;
end

if strcmp(spectramap,'HET PSD-4m'),   
	% HET PSD DISTANCES AND SPECTRA NUMBERS 
	l1=175;
	l2=27;
	channels=64;
	l3=914;
	l4=l3/channels;
	tubes=13;
	s_w1=401;
	s_e1=1233;

	tubes_ns=10;
	channels_ns=26;
	s_n1=2065;
	s_s1=2325;
	l3_ns=channels_ns/channels*l3;
	l4_ns=l3_ns/channels_ns;
	l1_ns=-tubes_ns/2*l2;
   
   s=zeros(size(data,1));
	for i=1:size(data,1),
	   x=data(i,1);
	   y=data(i,2);      
      if (y<l3/2)&(y>-l3/2)&(l1<x)&(x<(l1+tubes*l2)),
   		% east bank
   		n=floor((x-l1)/l2)+1;
   		m=floor((y+l3/2)/l4)+1;
   		s(i)=s_e1+(n-1)*channels+(m-1);           
      elseif (y<l3/2)&(y>-l3/2)&(x<-l1)&(x>-(l1+tubes*l2)),
   		% west bank
   		n=floor((-x-l1)/l2)+1;
   		m=floor((y+l3/2)/l4)+1;
   		s(i)=s_w1+(n-1)*channels+(m-1);   
		elseif  (y<l3/2)&(y>(l3/2-l3_ns))&(x>l1_ns)&(x<-l1_ns),
   		% north bank
   		n=floor((x-l1_ns)/l2)+1;
   		m=floor((y-(l3/2-l3_ns))/l4_ns)+1;
   		s(i)=s_n1+(n-1)*channels_ns+(m-1);   
		elseif  (y>-l3/2)&(y<(-l3/2+l3_ns))&(x>l1_ns)&(x<-l1_ns),
   		% south bank pix_991
   		%n=floor((x-l1_ns)/l2)+1;
   		%m=floor((-y-(l3/2-l3_ns))/l4_ns)+1;
         %s(i)=s_s1+(n-1)*channels_ns+(m-1);   
         % south bank pix_992
   		n=floor((x-l1_ns)/l2)+1;
   		m=channels_ns-floor((-y-(l3/2-l3_ns))/l4_ns);
         s(i)=s_s1+(n-1)*channels_ns+(m-1);            
		else   
   		% POINT CHOSEN IS NOT IN THE PSD BANKS
         s(i)=0;
      end
	end
else
   disp(sprintf('Unknown spectra map %s. Return.', spectramap));
   s=[];
end