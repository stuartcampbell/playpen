function slice_d=slice_spe(data,z,vz_min,vz_max,vx_min,vx_max,bin_vx,vy_min,vy_max,bin_vy,i_min,i_max,shad,noplot)

% function slice_d=slice_spe(data,z,vz_min,vz_max,vx_min,vx_max,bin_vx,vy_min,vy_max,bin_vy,i_min,i_max,shad,noplot)
% required inputs:
% data structure 
%  	.v (ndet,ne,3)[either rlu or meV] projections of data on the viewing basis u1,u2,u3
%     .S (ndet,ne) intensities
%     .ERR (ndet,ne) corresponding errors
%     .axis_label (4,:)[string] = (1:3,:)labels of the u1,u2,u3 axes and (4,:) units of intensity axis
%     .filename [string]
%     .title_label [string]
%     2d grid  vx_min-bin_vx/2:bin_vx(bin_width):vx_max+bin_vx/2 [) and vy_min-bin_vy/2:bin_vy:vy_max+bin_vy/2
%     intensity range : i_min,i_max, default i_min=min(intensity(:)), i_max=max(intensity(:))
%     shad= shading option 'flat'(default), 'faceted' or 'interp'
%     uses binning routine in Digital FORTRAN 5.0B 
%     compile on local hard disk (not remote diks) with >> mex c:\matlab5\extern\examples\refbook\bin2d_df.for

% === check presence of required input parameters
if ~exist('z','var')|isempty(z)|~isnumeric(z)|(length(z)~=1)
   disp('Slice cannot be performed if plane is not specified.');
   cut_d=[];
   return;
end
flag=(~exist('vx_min','var'))|isempty(vx_min)|~isnumeric(vx_min)|(length(vx_min)~=1);
flag=flag&(~exist('vx_max','var'))|isempty(vx_max)|~isnumeric(vx_max)|(length(vx_max)~=1);
flag=flag&(~exist('bin_vx','var'))|isempty(bin_vx)|~isnumeric(bin_vx)|(length(bin_vx)~=1);
flag=flag&(~exist('vy_min','var'))|isempty(vy_min)|~isnumeric(vy_min)|(length(vy_min)~=1);
flag=flag&(~exist('vy_max','var'))|isempty(vy_max)|~isnumeric(vy_max)|(length(vy_max)~=1);
flag=flag&(~exist('bin_vy','var'))|isempty(bin_vy)|~isnumeric(bin_vy)|(length(bin_vy)~=1);
if flag,
   disp('One or more of the required parameters for slice is not correct. Slice not performed.');
   slice_d=[];
   return;
end  

% ==============================================================================
% Establish orientation of cutting plane with respect to the viewing axes u1,u2,u3
% ==============================================================================
if (vx_min>vx_max)|(vy_min>vy_max)|(vz_min>vz_max),
   disp('Warning: Range x, y or z with lower_limit > upper_limit. Slice not performed.');
   slice_d=[];
   return   
end
if z==1,
   x=2;
   y=3;
elseif z==2,
   x=3;
   y=1;
elseif z==3,
   x=1;
   y=2;
else
   disp(['Warning: Cannot perform slice perpendicular to axis number ' num2str(z)]);
   slice_d=[];
	return   
end

a=version;
if (a(1)<=5)&isvms,
   algorithm=2; 	% use the matlab binning routine for a fast DEc alpha (vms) 
    % or if problems with array size too big for the fortran code
else
  	algorithm=1;	% by default (on PC/unix/linux) use the faster fortran slicing/binning routine, 
end 
  
disp_names=(1<0);	% TRUE if name of cut algorithm is to be displayed in the command line 
%algorithm=2;
if algorithm==1,
	% ==============================================================================================
   % call slice_df.dll, a mex FORTRAN slicing/binning routine
   % input: pixel coordinates, intensity error and 2d grid information  
   % output: matrices with intensity and error on the 2d grid (error subsequently not used)
   % compile with >> mex slice_df.f 
   % ==============================================================================================	
   if disp_names,
   	disp(sprintf('fortran binning routine slice_df.f'));
   end
   
   grid=[vx_min vx_max bin_vx vy_min vy_max bin_vy vz_min vz_max];	% 2d grid information (1,8)
	[intensity,error_int]=slice_df(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),data.S,data.ERR,grid);
   clear mex slice_df ; 	% ensures clearing up of memory after binning 
	if isempty(intensity),
   	slice_d=[];
   	return;
   end
   [m,n]=size(intensity);
	index=(intensity<=-1e+30);	% in bins with nulldata put NaN 
	intensity(index)=NaN;
   error_int(index)=NaN;
   
elseif algorithm>=2,   
   % ==================================================================================================
   % Extract data contained within the boundaries of the thick cut plane, to be binned onto the 2d grid
   % ==================================================================================================
   v=reshape(data.v,prod(size(data.v(:,:,1))),3);   % matrix (ndet*ne,3) with projections
   int=data.S(:);
   err=data.ERR(:);
   index=((v(:,z)>=vz_min)&(v(:,z)<=vz_max));	% include [] on the boundaries to agree with Phoenix 4.1
   v=v(index,:);
   int=int(index);
   err=err(index);
   n=floor((vx_max+bin_vx-(vx_min-bin_vx/2))/bin_vx);	% number of vx bins
   m=floor((vy_max+bin_vy-(vy_min-bin_vy/2))/bin_vy);	% number of vy bins
   index=((v(:,x)>=(vx_min-bin_vx/2))&(v(:,x)<(vx_min-bin_vx/2+n*bin_vx))&(v(:,y)>=(vy_min-bin_vy/2))&...
      (v(:,y)<(vy_min-bin_vy/2+m*bin_vy))); % [) in both directions to agree with Phoenix 4.1
   v=v(index,:);
   int=int(index);
   err=err(index);
   pixels=[v(:,x)'; v(:,y)'; int'; err'];		% (4,Npixels)
   % === return if slice contains no data
   %disp(sprintf('Total number of pixels %10d',size(pixels,2)));
   if isempty(v),
      disp('Slice contains no data. Slice not performed.');
      slice_d=[];
      return;
   end
   
   if algorithm==2,
      % ===============================================================================================
      % alternative faster MATLAB binning routine 22-December-1998 
      % - matlab equivalent of the FORTRAN bin2d_df.for
      % typical execution time for the test slice (phoenix) 4.18s 
      % distribute all points in bins, cummulate intensities and errors^2, then do the averaging
      % ===============================================================================================
      if disp_names,
         disp(sprintf('matlab 2D binning routine'));
      end

      intensity=zeros(m,n);
      error_int=intensity;
      number=intensity;
      for k=1:size(pixels,2),
         i=floor((pixels(2,k)-(vy_min-bin_vy/2))/bin_vy)+1;
         j=floor((pixels(1,k)-(vx_min-bin_vx/2))/bin_vx)+1;
         number(i,j)=number(i,j)+1;
         intensity(i,j)=intensity(i,j)+pixels(3,k);
         error_int(i,j)=error_int(i,j)+pixels(4,k)^2;
      end
      index=(number==0);	% identify bins wiht no pixels  
      intensity(index)=NaN;
      intensity(~index)=intensity(~index)./number(~index); 
      error_int(index)=NaN;
      error_int(~index)=sqrt(error_int(~index))./number(~index);
   
   elseif algorithm==3, 
      % =======================================================================================================
      % alternative much slower MATLAB binning routine, 22-December-1998, uses multiplication of large logical matrices
      % typical times ~ 1min
      % typical execution time for the test slice (phoenix) 1 min
      % =======================================================================================================
      if disp_names,
         disp(sprintf('very slow matlab 2D binning routine'));
      end
      
      intensity=zeros(m,n);
      error_int=intensity;
      for i=1:m,
         for j=1:n,
            index=(pixels(1,:)>=(vx_min+(j-1-1/2)*bin_vx))&(pixels(1,:)<(vx_min+(j-1/2)*bin_vx))&...
               (pixels(2,:)>=(vy_min+(i-1-1/2)*bin_vy))&(pixels(2,:)<(vy_min+(i-1/2)*bin_vy));
            number=sum(index);
            if number>=1,
               intensity(i,j)=mean(pixels(3,index));
               error_int(i,j)=norm(pixels(4,index))/number;
            end      
         end
      end
   end
end

% add an extra column and row for the plotting routine pcolor
intensity(m+1,:)=NaN;
intensity(:,n+1)=NaN;
error_int(m+1,:)=NaN;
error_int(:,n+1)=NaN;

% ====================================================
% Prepare the slice data structure and plot parameters 
% ====================================================
% ==== construct slice title
form='%7.5g';	% number format 
titlestr1=[avoidtex(data.filename) ', ' data.title_label];
if data.emode==1,	% direct geometry Ei fixed
   titlestr1=[titlestr1 ', Ei=' num2str(data.efixed,form) ' meV'];
elseif data.emode==2,	% indirect geometry Ef fixed
   titlestr1=[titlestr1 ', Ef=' num2str(data.efixed,form) ' meV'];
else
   disp(sprintf('Could not identify spectrometer type (only direct/indirect geometry allowed), emode=%g',data.emode));
   titlestr1=[titlestr1 ', E=' num2str(data.efixed,form) ' meV'];   
end   
% if sample is single crystal and uv orientation and psi_samp are defined, include in title
if isfield(data,'uv')&~isempty(data.uv)&isnumeric(data.uv)&(size(data.uv)==[2 3])&...
      isfield(data,'psi_samp')&~isempty(data.psi_samp)&isnumeric(data.psi_samp)&...
      (prod(size(data.psi_samp)==1)),
   titlestr2=sprintf('{\\bfu}=[%g %g %g], {\\bfv}=[%g %g %g], Psi=({\\bfu},{\\bfki})=%g',...
      data.uv(1,:),data.uv(2,:),data.psi_samp*180/pi);
else
   titlestr2=[];
end
titlestr3=['slice '  num2str(vz_min,form) '<' deblank(data.axis_label(z,:)) '<' num2str(vz_max,form) ];
titlestr3=[titlestr3 ' , ' deblank(data.axis_label(x,:)) '=' num2str(vx_min,form) ':' num2str(bin_vx,form) ':'  num2str(vx_max,form)];
titlestr3=[titlestr3 ' , ' deblank(data.axis_label(y,:)) '=' num2str(vy_min,form) ':' num2str(bin_vy,form) ':'  num2str(vy_max,form) ];
if ~isempty(titlestr2),
   slice_d.title={titlestr1, titlestr2, titlestr3};
else
   slice_d.title={titlestr1, titlestr3};
end   

% ==== establish limits for plot along intensity axis
if ~exist('i_min','var')|isempty(i_min)|~isnumeric(i_min)|(length(i_min)~=1),
   i_min=min(intensity(:));
end
if ~exist('i_max','var')|isempty(i_max)|~isnumeric(i_max)|(length(i_max)~=1),
   i_max=max(intensity(:));
end

% ==== establish shading option for the colour plot
if ~exist('shad','var'),
   shad='flat';
end

% === prepare return slice data structure
slice_d.vx=vx_min-bin_vx/2+(0:n)*bin_vx;	% edges of bin boundaries (n+1) along vx
slice_d.vy=vy_min-bin_vy/2+(0:m)*bin_vy;	% (m+1) along vy
slice_d.intensity=intensity;
slice_d.error_int=error_int;

% === construct axis labels
labelx=[combil(deblank(data.axis_label(x,:)),data.u(x,:),data.u(z,:)*mean([vz_min vz_max])) ...
      ' ' deblank(data.axis_unitlabel(x,:))];
labely=[combil(deblank(data.axis_label(y,:)),data.u(y,:),data.u(z,:)*mean([vz_min vz_max])) ...
      ' ' deblank(data.axis_unitlabel(y,:))];
slice_d.axis_label=str2mat(labelx,labely,deblank(data.axis_unitlabel(4,:)));
slice_d.axis_unitlength=data.axis_unitlength([x;y]);

%=======================================================================
% Plot 2d slice with a vertical colorbar, unless 'noplot' is specified
%=======================================================================
if exist('noplot','var')&ischar(noplot)&strcmp(deblank(noplot),'noplot'),
   % do not plot slice_spe
   return
else
	plot_slice(slice_d,i_min,i_max,shad);   
end
