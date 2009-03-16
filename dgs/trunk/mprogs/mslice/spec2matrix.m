function [matrices,ax,textpos]=spec2matrix(data,spectramap)

% function matrices=spec2matrix(data,spectramap)
% data=[spectra intensity error] (nspectra,3)
% map ='HET PSD-4m'

if isempty(data),
   disp(sprintf('Spectra data empty. Return.'));
   matrices=[];
   return;
end

if strcmp(spectramap,'HET PSD-4m'),   
   % === construct matrices with intensities for the WEST PSD low-angle bank on HET
   tubes=13;   % number of PSD tubes in the bank
   channels=64;% number of independent detectpr elements along the tube
   l0=4020;		% mm  sample-detector plane distance
   l1=175;		% mm  
   l2=27;		% mm  distance between adjacent detectors
   l3=914;		% mm	tube length
   % === put data in matrix form and 
   % === account for the numbering of detectors starting in bottom left-hand corner
   
   % === construct matrix with intensities for the WEST PSD low-angle bank on HET   
   s1=401;	% first spectrum number in the bank
   s2=s1-1+channels*tubes;	% last spectrum number in the bank
   [x_w,y_w,int_w,err_w]=put_in_matrix(data,s1,s2,tubes,channels,-l1-l2*tubes,-l3/2,l2,l3/channels,'fliplr');
   matrices=cell(1,2);
   matrices{1}=cat(3,x_w,y_w,int_w,err_w);
   
   % === construct matrix with intensities for the EAST PSD low-angle bank on HET
   s1=1233;	% first spectrum number in the bank
   s2=s1-1+channels*tubes;	% last spectrum number in the bank
   % === put data in matrix form and 
   [x_e,y_e,int_e,err_e]=put_in_matrix(data,s1,s2,tubes,channels,l1,-l3/2,l2,l3/channels);
   matrices{2}=cat(3,x_e,y_e,int_e,err_e);

   % === construct matrix with intensities for the NORTH PSD low-angle bank on HET
   channels_ns=26;	% channels along a tube in the N & S PSD 
   tubes_ns=10;
   l1_ns=-l2*tubes_ns/2;
   l3_ns=channels_ns/channels*l3;	% make vertical length for one detector element the same as for the E&W PSD
   s1=2065;	% first spectrum number in the bank
   s2=s1-1+channels_ns*tubes_ns;	% last spectrum number in the bank
   % === put data in matrix form and 
   [x_n,y_n,int_n,err_n]=put_in_matrix(data,s1,s2,tubes_ns,channels_ns,l1_ns,l3/2-l3_ns,l2,l3_ns/channels_ns);
   if ~isempty(x_n),
      matrices{end+1}=cat(3,x_n,y_n,int_n,err_n);
   end
   
   % === construct matrix with intensities for the SOUTH PSD low-angle bank on HET
   s1=2325;	% first spectrum number in the bank
   s2=s1-1+channels_ns*tubes_ns;	% last spectrum number in the bank
   % === put data in matrix form and 
   % === for pix_991
   %[x_s,y_s,int_s,err_s]=put_in_matrix(data,s1,s2,tubes_ns,channels_ns,l1_ns,-l3/2,l2,l3_ns/channels_ns,'flipud');  
   % === for pix_992
   [x_s,y_s,int_s,err_s]=put_in_matrix(data,s1,s2,tubes_ns,channels_ns,l1_ns,-l3/2,l2,l3_ns/channels_ns);     
   if ~isempty(x_n),
      matrices{end+1}=cat(3,x_s,y_s,int_s,err_s);
   end
   ax=[-l1-l2*tubes l1+tubes*l2 -l3/2 l3/2]; % axes limits
   textpos=[-l1*0.9 l3/2*0.3; -l1*0.9 l3/2*0]; % positions [x1 y1; x2 y2] for text pointer at : spectrum .. intensity ... 
else
   disp(sprintf('Unknown specra map %s',spectramap));
   matrices=[];
   ax=[];
   return;
end
