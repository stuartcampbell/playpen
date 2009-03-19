function [x,intensity,error_int,vvy,vvz]=cut3dxye_m(vx,vy,vz,pixel_int,pixel_err,grid)

% function [x,intensity,error_int,vvy,vvz]=cut3d_m(vx,vy,vz,pixel_int,pixel_err,grid)
% distribute all data points into bins, then take average over each bin
% vx(ndet,ne),vy(ndet,ne),vz(ndet,ne),pixel_int(ndet,ne),pixel_err(ndet,ne),
% grid(7)=[vx_min vx_max bin_vx vy_min vy_max vz_min vz_max]
% x(1,m),intensity(1,m),error_int(1,m)  arrays of x,y,error per bins with pixels
% vvy,vvz average vy,vz values per cut 
% initialize to zero the number of pixel per bin,x,intensity,error_int 

vx_min=grid(1);
vx_max=grid(2);
bin_vx=grid(3);
vy_min=grid(4);
vy_max=grid(5);
vz_min=grid(6);
vz_max=grid(7);
eps=1.0d-5*(vx_max-vx_min); % this will be a small number (0 for practical purposes) in units of vx-axis
% if bin_x=0 by convention do not bin data along vx axis, for each detector bin along the energy axis
[ndet,ne]=size(vx);
if bin_vx < eps, % use binning along the energy axis for each detector 
	n=ndet; % all detectors can contribute
   % keep vx_min and vx_max as they are
else % normal binnig along the vx axis
	n=floor((vx_max+bin_vx-(vx_min-bin_vx/2))/bin_vx); % n = total number of bins	
   % redefine vx_min and vx_max to include -bin_vx/2 on the left and +(bin_vx/2 or bin_vx) on the right
   % use these numbers below for faster comparisons of pixel inclusion criteria 
   vx_min=vx_min-bin_vx/2;
   vx_max=vx_min+n*bin_vx;
end 

x=zeros(1,n);
intensity=zeros(1,n);
error_int=zeros(1,n);
number_pix=zeros(1,n);

algorithm=2;
if algorithm==1,  
   disp('Matlab cut algorithm 1: distribute pixels in bins first, then do average');   
   index=zeros(ndet*ne,3);
   vvy=0; % will contain partial sum of vy values 
   vvz=0; % will contain partial sum of vz values 
   final_npixel=0;   % will contain final number of pixels going into the cut
   bin_index=zeros(1,n);
   
   % choose between normal binning along the vx axis (bin_vx >0) and binning along the energy axis only (bin_vx=0) 
	if bin_vx>eps,   % normal binning along the vx-axis    
   	% run through all pixels and if contributing then distribute then into bins
   	for j=1:ndet,
      	for l=1:ne,
         	k=j+ndet*(l-1); % global index in the (ndet,ne) matrix 
         	% run energy index first, then detector index 
         	if ((vx(k)>=vx_min)&(vx(k)< vx_max)&...
               (vy(k)>=vy_min)&(vy(k)<=vy_max)&...
               (vz(k)>=vz_min)&(vz(k)<=vz_max)&...            
               (pixel_int(k)>-1d+30)),   % also test if pixel is not masked  
            	i=floor((vx(k)-vx_min)/bin_vx+1); % index of bin along vx
            	number_pix(i)=number_pix(i)+1;    % number of pixels so far in bin i
            	final_npixel=final_npixel+1;      % increase total number of contributing pixels  
            	x(i)         =x(i)+vx(k);         % sum vx values
            	vvy          =vvy +vy(k);         % sum vy values
            	vvz          =vvz +vz(k);         % sum vz values
            	intensity(i) =intensity(i)+pixel_int(k);   % sum intensity values
            	error_int(i) =error_int(i)+pixel_err(k)^2; % sum errors squared
         	end 
      	end 	
      end
   else % binning along the energy axis for each contributing detector      
   	for j=1:ndet,
      	for l=1:ne,
         	k=j+ndet*(l-1); % global index in the (ndet,ne) matrix 
         	% run energy index first, then detector index 
         	if ((vx(k)>=vx_min)&(vx(k)< vx_max)&...
               (vy(k)>=vy_min)&(vy(k)<=vy_max)&...
               (vz(k)>=vz_min)&(vz(k)<=vz_max)&...            
               (pixel_int(k)>-1d+30)),   % also test if pixel is not masked  
            	number_pix(j)=number_pix(j)+1;    % number of pixels so far in bin j
            	final_npixel=final_npixel+1;      % increase total number of contributing pixels  
            	x(j)         =x(j)+vx(k);         % sum vx values
            	vvy          =vvy +vy(k);         % sum vy values
            	vvz          =vvz +vz(k);         % sum vz values
            	intensity(j) =intensity(j)+pixel_int(k);   % sum intensity values
            	error_int(j) =error_int(j)+pixel_err(k)^2; % sum errors squared
         	end 
      	end 	
      end
	end      
   m=0;  % will contain final number of bins with pixels in them 
   % return if no pixels in the cut
   if final_npixel==0,
      x=[];
      intensity=[];
      error_int=[];
      vvx=[];
      vvy=[];
      return;
   end   
   % take the average over each bin and count number of bins with data 	
   for i=1:n,
      if (number_pix(i)>=1),
         m=m+1;  % move on to next bin with pixels
         bin_index(i) =m;  % former bin i becomes true bin m 
         number_pix(m)=number_pix(i);  
         x(m)         =x(i)/number_pix(m); %  average vx values
         intensity(m) =intensity(i)/number_pix(m); % average intensity values
         error_int(m) =sqrt(error_int(i))/number_pix(m); % average errors squared
      end	
   end
   % determine average of vy and vz values per cut
   vvy=vvy/final_npixel;
   vvz=vvz/final_npixel;
   x=x(1:m);
   intensity=intensity(1:m);
   error_int=error_int(1:m);
   
elseif algorithm==2,
   disp('Matlab cut algorithm 2: for each bin find contributing pixels then average');
   % === Retain only pixels which will contribute to the cut   
   index_pix=((vx>=vx_min)&(vx< vx_max)&...
   	        (vy>=vy_min)&(vy<=vy_max)&...
              (vz>=vz_min)&(vz<=vz_max)); % matrix (ndet,ne), sum(index)=N elements
   final_npixel=sum(index_pix); % total number of contributing pixels
	% return if no pixels in the cut
   if final_npixel==0,
     	x=[];
    	intensity=[];
    	error_int=[];
      vvx=[];
      vvy=[];
     	return;
   end           
 	s.type='()';
 	s.subs={index_pix'}; % such that energy index runs faster, then detector index     
   % choose between normal binning along the vx axis (bin_vx >0) and binning along the energy axis only (bin_vx=0) 
	if bin_vx>eps,   % normal binning along the vx-axis    
   	v=subsref(vx',s);
   	int=subsref(pixel_int',s); % pixel y, column vector (N,1)
   	err=subsref(pixel_err',s); % pixel e, column vector (N,1)
   	vy_pix=subsref(vy',s);
   	vz_pix=subsref(vz',s);
   	% === allocate space for the x,intensity, error_int values in the cut, 
   	% === n is total number of bins
   	% === perform cut by running through all bins and averaging simultaneously 
   	% === the x, intensity and error_int values 
   	% === of all pixels in each bin
   	m=0;	% will index the current bin
   	vvy=0;
   	vvz=0;
   	for i=1:n,
      	index=((v>=(vx_min+(i-1)*bin_vx))&(v<(vx_min+i*bin_vx)));
   		% use above test to agree with Phoenix 4.1 binning algorithm [ )
			number=sum(index);	% number of pixels to contribute to bin 'i'
   		if number>=1, 	% have found a new bin with pixels
         	m=m+1;	% index of bins with data increases by one
         	x(m)=mean(v(index));	% simple average of pixel positions
         	intensity(m)=mean(int(index));	% simple average of pixel intensities
         	error_int(m)=norm(err(index))/number; 
         	number_pix(m)=number;
         	vvy=vvy+sum(vy_pix(index));
         	vvz=vvz+sum(vz_pix(index));
   		end     
		end	   
		% === eliminate bins with no data from the cut
		x=x(1:m);
		intensity=intensity(1:m);
   	error_int=error_int(1:m);      
   	number_pix=number_pix(1:m);
   	vvy=vvy/sum(number_pix);
   	vvz=vvz/sum(number_pix);
   else  % bin_vx=0, bin data in each contributing detector along the energy axis
   	number_pix=sum(index_pix,2); % sum along the energy axis
   	i=find(number_pix);	% find detectors with contributing pixels
   	number_pix=number_pix(i);
      x=sum(vx.*index_pix,2);
      x=x(i)./number_pix;
      vvy=sum(sum(vy.*index_pix,2))/sum(number_pix); % (global) average vy value
      vvz=sum(sum(vz.*index_pix,2))/sum(number_pix); % (global) average vz value      
      intensity=sum(pixel_int.*index_pix,2);
      intensity=intensity(i)./number_pix;
   	error_int=sum((pixel_err.^2).*index_pix,2);
      error_int=sqrt(error_int(i))./number_pix;   
   end  % bin_vx=0   
end % algorithm=2          