function [x,intensity,error_int,perm,number_pix,vvy]=cut2d_m(vx,vy,pixel_int,pixel_err,grid)

% function [x,intensity,error_int,perm,number_pix,vvy]=cut2d_m(vx,vy,pixel_int,pixel_err,grid)
% distribute all data points into bins, then take average over each bin
% vx(ndet,ne),vy(ndet,ne), pixel_int(ndet,ne),pixel_err(ndet,ne),
% grid(5)=[vx_min vx_max bin_vx vy_min vy_max]
% x(1,m),intensity(1,m),error_int(1,m)  arrays of x,y,error per bins with pixels
% perm(final_npixel,1),number_pix(1,m) binning recipe
% vvy average vy values per cut 


% initialize to zero the number of pixel per bin,x,intensity,error_int 
% === initialise variables
vx_min=grid(1);
vx_max=grid(2);
bin_vx=grid(3);
vy_min=grid(4);
vy_max=grid(5);
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
   final_npixel=0;   % will contain final number of pixels going into the cut
   bin_index=zeros(1,n);
   
	% choose between normal binning along the vx axis (bin_vx >0) and binning along the energy axis only (bin_vx=0) 
	if bin_vx>eps,   % normal binning along the vx-axis    
   	% run through all pixels and if contributing then distribute then into bins
   	for j=1:ndet,
      	for l=1:ne,
         	k=j+ndet*(l-1); % global index in the (ndet,ne) matrix 
                            % run energy index first, then detector index 
         	if ((vx(k)>=vx_min)&(vx(k)<vx_max)&...
             	(vy(k)>=vy_min)&(vy(k)<=vy_max)&...
             	(pixel_int(k)>-1d+30)),           % also test if pixel is not masked  
          		i=floor((vx(k)-vx_min)/bin_vx+1); % index of bin along vx
          		number_pix(i)=number_pix(i)+1;    % number of pixels so far in bin i
          		final_npixel=final_npixel+1;      % increase total number of contributing pixels  
          		x(i)         =x(i)+vx(k);         % sum vx values
          		vvy          =vvy +vy(k);         % sum vy values
          		intensity(i) =intensity(i)+pixel_int(k);   % sum intensity values
          		error_int(i) =error_int(i)+pixel_err(k)^2; % sum errors squared
          		index(final_npixel,1)=i;           % store bin index for current pixel 
          		index(final_npixel,2)=number_pix(i); % store order of pixel in bin i
          		index(final_npixel,3)=k;           % store global index in (det,en) matrix for current pixel
         	end 
      	end 	
      end
   else % binning along the energy axis for each contributing detector      
   	for j=1:ndet,
      	for l=1:ne,
         	k=j+ndet*(l-1); % global index in the (ndet,ne) matrix 
                      % run energy index first, then detector index 
         	if ((vx(k)>=vx_min)&(vx(k)<vx_max)&...
             	(vy(k)>=vy_min)&(vy(k)<=vy_max)&...
             	(pixel_int(k)>-1d+30)),   % also test if pixel is not masked  
          		number_pix(j)=number_pix(j)+1;    % number of pixels so far in bin j
          		final_npixel=final_npixel+1;      % increase total number of contributing pixels  
          		x(j)         =x(j)+vx(k);         % sum vx values
          		vvy          =vvy +vy(k);         % sum vy values
          		intensity(j) =intensity(j)+pixel_int(k);   % sum intensity values
          		error_int(j) =error_int(j)+pixel_err(k)^2; % sum errors squared
          		index(final_npixel,1)=j;           % store bin index for current pixel 
          		index(final_npixel,2)=number_pix(j); % store order of pixel in bin j
          		index(final_npixel,3)=k;           % store global index in (det,en) matrix for current pixel
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
      number_pix=[];
      perm=[];
      vvy=[];
      return;
   end
   % take the average over each bin and count number of bins with data 	   
   cumm_npixels(1)=0; 
   for i=1:n,
      if (number_pix(i)>=1),
         m=m+1;  % move on to next bin with pixels
         bin_index(i) =m;  % former bin i becomes true bin m 
         number_pix(m)=number_pix(i);  
         x(m)         =x(i)/number_pix(m); %  average vx values
         intensity(m) =intensity(i)/number_pix(m); % average intensity values
         error_int(m) =sqrt(error_int(i))/number_pix(m); % average errors squared
         if m >= 2,   % compute cummulative sums of contributing pixels up to the current bin
            cumm_npixels(m)=cumm_npixels(m-1)+number_pix(m-1);
         end
      end	
   end
   % determine average of vy values per cut
   vvy=vvy/final_npixel;
   % determine true pixel order 
   perm=zeros(final_npixel,1);
   for k=1:final_npixel,
      i=cumm_npixels(bin_index(index(k,1)))+index(k,2); % global index of current pixel into the 
      perm(i)=index(k,3);                               % sequence of contributing pixels 
   end 
   x=x(1:m);
   intensity=intensity(1:m);
   error_int=error_int(1:m);
   number_pix=number_pix(1:m);
   
elseif algorithm==2,
   
   disp('Matlab cut algorithm 2: for each bin find contributing pixels then average');
   % === Retain only pixels which will contribute to the cut   
   index_pix=((vx>=vx_min)&(vx< vx_max)&...
      		  (vy>=vy_min)&(vy<=vy_max)); % matrix (ndet,ne), sum(index)=N element       
   final_npixel=sum(index_pix); % total number of contributing pixels
	% return if no pixels in the cut
   if final_npixel==0,
     	x=[];
    	intensity=[];
    	error_int=[];
     	number_pix=[];
     	perm=[];
      vvy=[];  
      return;
   end  
 	s.type='()';
 	s.subs={index_pix'}; % such that energy index runs faster, then detector index     
 	det=(1:ndet)'*ones(1,ne);
 	det=subsref(det',s); % pixel detector number 
 	en=ones(ndet,1)*(1:ne);
 	en=subsref(en',s);   % pixel energy bin       
   % choose between normal binning along the vx axis (bin_vx >0) and binning along the energy axis only (bin_vx=0) 
	if bin_vx>eps,   % normal binning along the vx-axis    
   	v=subsref(vx',s);
   	int=subsref(pixel_int',s); % pixel y, column vector (N,1)
   	err=subsref(pixel_err',s); % pixel e, column vector (N,1)
   	vy_pix=subsref(vy',s);
   	% === allocate space for the x,intensity, error_int values in the cut, 
   	% === n is total number of bins
   	perm=[];
   	% === perform cut by running through all bins and averaging simultaneously 
   	% === the x, intensity and error_int values 
   	% === of all pixels in each bin
   	m=0;	% will index the current bin
   	vvy=0;
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
         	perm=[perm; det(index)+ndet*(en(index)-1)];
   		end     
		end	   
		% === eliminate bins with no data from the cut
		x=x(1:m);
		intensity=intensity(1:m);
   	error_int=error_int(1:m);      
   	number_pix=number_pix(1:m);
   	vvy=vvy/sum(number_pix); 
   else  % bin_vx=0, bin data in each contributing detector along the energy axis
   	number_pix=sum(index_pix,2); % sum along the energy axis
   	i=find(number_pix);	% find detectors with contributing pixels
   	number_pix=number_pix(i);
      x=sum(vx.*index_pix,2);
      x=x(i)./number_pix;
      vvy=sum(sum(vy.*index_pix,2))/sum(number_pix); % (global) average vy value
   	intensity=sum(pixel_int.*index_pix,2);
      intensity=intensity(i)./number_pix;
   	error_int=sum((pixel_err.^2).*index_pix,2);
      error_int=sqrt(error_int(i))./number_pix;   
	   % === sort data in ascending order of x values
   	[x,p]=sort(x);	% permute x
      intensity=intensity(p);	% permute intensity
      error_int=error_int(p); % permute error
      % determine pixel index matrix, energy index runs faster, then detector index  
      perm=det+ndet*(en-1); % pixel index in the order of increasing detector number 
   end  % bin_vx=0  
end % algorithm=2    