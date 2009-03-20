function cut=cut_spe(data,x,vx_min,vx_max,bin_vx,vy_min,vy_max,vz_min,vz_max,i_min,i_max,out_type,out_file,tomfit)

% function cut=cut_spe(data,x,vx_min,vx_max,bin_vx,vy_min,vy_max,vz_min,vz_max,i_min,i_max,out_type,out_file,tomfit)
%       for single crystal data with PSD detectors and 
% function cut=cut_spe(data,x,vx_min,vx_max,bin_vx,vy_min,vy_max,i_min,i_max,out_type,out_file,tomfit)
%       for single crystal data with conventional detectors, or powder data
% 
% data.v (ndet,ne,3) projections along the viewing axes u1,u2,u3
%     .S (ndet,ne)intensities
%     .ERR (ndet,ne) corresponding errors
%     .axis_label 4-row string vector with labels of the u1,u2,u3 axes and intensity axis
%      save results in .cut format for the fitting programmes tobyfit, smhfit, bobfit
%                  compare .cut files with phoenix_psd cut/1d/t , still ~5e-4 difference in the x positions 
%                  maybe due to how many decimal places were used for pi and transform En-> wavevector
%      save results in xye_file x,y,error table format
% tomfit=true if cut is to be sent directly to Mfit
% returns cut structure 
%     pixels: [5071x6  double]
%    npixels: [   1x41 double]
%          x: [   1x41 double]
%          y: [   1x41 double]
%          e: [   1x41 double]
%    x_label: '[ -0.3, 0, Q_l ]  in 1.160 �^{-1}'
%    y_label: 'Intensity(abs.units)'
%      title: {   1x2  cell  }
%     symbol: 'rs'
%       axis: [-0.6150 0.6300 0.1000 0.6000]
%      y_min: 0.1000
%      y_max: 0.6000
% Radu Coldea 22-Oct-2000

% === if .spe data file not read in the MSlice ControlWindow or if projections not calculated, return
if ~exist('data','var')|~isfield(data,'v'),
   disp('Load data and calculate projections on viewing axes, then do cut.')
   cut_d=[];
   return
end

% === check presence of required parameters in the calling syntax
if ~exist('x','var')|isempty(x)|~isnumeric(x)|(length(x)~=1)
   disp('Cut cannot be performed if axis is not specified.');
   cut_d=[];
   return;
end
flag=(~exist('vx_min','var'))|isempty(vx_min)|~isnumeric(vx_min)|(length(vx_min)~=1);
flag=flag&(~exist('vx_max','var'))|isempty(vx_max)|~isnumeric(vx_max)|(length(vx_max)~=1);
flag=flag&(~exist('bin_vx','var'))|isempty(bin_vx)|~isnumeric(bin_vx)|(length(bin_vx)~=1);
flag=flag&(~exist('vy_min','var'))|isempty(vy_min)|~isnumeric(vy_min)|(length(vy_min)~=1);
flag=flag&(~exist('vy_max','var'))|isempty(vy_max)|~isnumeric(vy_max)|(length(vy_max)~=1);
if flag,
   disp('One or more of the required range parameters for the cut is missing or has incorrect type.');
   disp('Cut not performed.');
   cut=[];
   return;
end   
   
% === rename variables to maintain compatbility between cut2d_df and cut3d_df (2 or 3 viewing axes)
if size(data.v,3)==2,	%=== 2d data set 
   if exist('out_type','var'),
      tomfit=out_type;
   end
   if exist('i_max','var'),
      out_file=i_max;
   end
   if exist('i_min','var'),
      out_type=i_min;
   end
   if exist('vz_max','var'),
      i_max=vz_max;
   end
   if exist('vz_min','var'),
      i_min=vz_min;
   end
end   
   
% === find out if cut is to be saved to a cut, smh or hkl format to an ASCII file  
to_cut_file=exist('out_file','var')&(~isempty(out_file))&(~isempty(findstr(lower(out_type),'cut')));
% TRUE if pixel information to be saved in .cut format
to_smh_file=exist('out_file','var')&(~isempty(out_file))&(~isempty(findstr(lower(out_type),'smh')))&...
   isfield(data,'hkl');
% TRUE if pixel information to be saved in .smh format
to_hkl_file=exist('out_file','var')&(~isempty(out_file))&(~isempty(findstr(lower(out_type),'hkl')))&...
   isfield(data,'hkl');
% TRUE if pixel information to be saved in .smh format
   
%==============================================================================
%=== CHOOSE BETWEEN SINGLE CRYSTAL AND PSD DETECTORS (3 viewing axes), 
%=== AND POWDER OR SINGLE CRYSTAL + CONVENTIONAL DETECTORS (2 viewing axes)
%==============================================================================
if size(data.v,3)==3,	%=== single crystal data and PSD detectors
   %=== check that cut limits are in the correct order
   if (vx_min>vx_max)|(vy_min>vy_max)|(vz_min>vz_max),
      disp('Warning: Range x,y or z with lower_limit > upper_limit.');
      disp('Cut not performed.');
   	cut=[];
   	return   
	end
	switch x,	% choose the other axes by permutation
   	case 1,	y=2; z=3;
      case 2,	y=3; z=1;
      case 3,  y=1; z=2;
      otherwise, disp(['Cannot perform cut along axis number ' num2str(x)]); cut=[];return
   end      
   grid=[vx_min vx_max bin_vx vy_min vy_max vz_min vz_max];   
   
   % === if y-axis not intensity, then use standard deviation of the mean per bin as error 
   if ~isfield(data,'ERR')|isempty(data.ERR),
      try
         [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy,vz]=cut3d_df(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),...
            data.S,data.S,grid);
      catch
         [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy,vz] =cut3d_m(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),...
            data.S,data.S,grid);
      end
      try
         [cut.y,cut.e]=avpix_df(data.S,cut.perm,cut.npixels);
      catch   
         [cut.y,cut.e] =avpix_m(data.S,cut.perm,cut.npixels);
      end
   else  % normal binning of intensity values
      % to increase speed xye cut only use basic algorithm (do not calculate pixel permutation matrix)
      if ~isfield(data,'altx')&~to_cut_file&~to_smh_file&~to_hkl_file, 
         %disp('Using xye cut routine.');
         try
         	[cut.x,cut.y,cut.e,vy,vz]=...
               cut3dxye_df(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),data.S,data.ERR,grid);
         catch
         	[cut.x,cut.y,cut.e,vy,vz]=...
               cut3dxye_m(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),data.S,data.ERR,grid);            
         end   
         cut.perm=[];
         cut.npixels=[];
		else  % need full pixel information to save data in special format(cut,smh,hkl) or plot on alternative x-axis       
         try
            [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy,vz]=...
               cut3d_df(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),data.S,data.ERR,grid);
      	catch
            [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy,vz] =...
               cut3d_m(data.v(:,:,x),data.v(:,:,y),data.v(:,:,z),data.S,data.ERR,grid);
         end  
      end
   end      
else	%=== single crystal and conventional detectors, or powder data
   if (vx_min>vx_max)|(vy_min>vy_max),
      disp('Warning: Range x or y with lower_limit > upper_limit.');
      disp('Cut not performed.');
   	cut_d=[];
   	return   
	end
	switch x,
   	case 1,	y=2;
      case 2,	y=1;
      otherwise, disp(['Cannot perform cut along axis number ' num2str(x)]); cut=[]; return
   end      
   grid=[vx_min vx_max bin_vx vy_min vy_max];   
   % === if y-axis not intensity, then use standard deviation of the mean per bin as error 
   if ~isfield(data,'ERR')|isempty(data.ERR),
      try 
         [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy]=cut2d_df(data.v(:,:,x),data.v(:,:,y),data.S,data.S,grid);     
      catch
         [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy] =cut2d_m(data.v(:,:,x),data.v(:,:,y),data.S,data.S,grid);     
      end   
      try 
         [cut.y,cut.e]=avpix_df(data.S,cut.perm,cut.npixels);
      catch
         [cut.y,cut.e]=avpix_m(data.S,cut.perm,cut.npixels);
      end
   else
      try
         [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy]=cut2d_df(data.v(:,:,x),data.v(:,:,y),data.S,data.ERR,grid);            
      catch
         [cut.x,cut.y,cut.e,cut.perm,cut.npixels,vy] =cut2d_m(data.v(:,:,x),data.v(:,:,y),data.S,data.ERR,grid);            
      end   
   end      
end

% === if cut contains no data, then return   
if isempty(cut.x),
   disp('Warning: Cut contains no data.');
   cut=[];
   return
end

% === if plotting axis not cut axis, then average alternative x-values 
if isfield(data,'altx')&~isempty(data.altx)&isnumeric(data.altx),
   try 
      [cut.x,temp]=avpix_df(data.altx,cut.perm,cut.npixels);   
   catch
      [cut.x,temp]=avpix_m(data.altx,cut.perm,cut.npixels);   
   end
end
   
if to_smh_file|to_cut_file,
   en=ones(size(data.det_group))*data.en;
   det_group=data.det_group*ones(size(data.en)); 
   ebin=data.en(2)-data.en(1);
end
if to_hkl_file,
   en=ones(size(data.det_group))*data.en;
end   
%=== if data is to be saved in .smh format extract hkl (if powder data, then |Q|(Angs^{-1}),2Theta(deg),Azimuth(deg));
if to_smh_file,
   % === put extra parameters if saving data in .smh format
   cut.efixed=data.efixed; 
   cut.ebin=ebin;	
	cut.emin=data.en(1)-ebin/2;	
   % === by default psi_samp=0 for powder data to be saved in .smh format
   if isfield(data,'psi_samp'),
     	cut.psi_samp=data.psi_samp*180/pi;
   else
     	cut.psi_samp=0;
     end	% if powder data, then data.hkl=(|Q|(Angs^{-1}),2Theta(deg),Azimuth(deg));
   cut.det_index=det_group(cut.perm);
   cut.en_index=floor((en(cut.perm)-cut.emin)/ebin)+1;	
end
if to_smh_file|to_hkl_file, % average h,k,l,energy values per bin
   try
      [cut.h,temp]=avpix_df(data.hkl(:,:,1),cut.perm,cut.npixels);
      [cut.k,temp]=avpix_df(data.hkl(:,:,2),cut.perm,cut.npixels);
      [cut.l,temp]=avpix_df(data.hkl(:,:,3),cut.perm,cut.npixels);
      [cut.energy,temp]=avpix_df(en,cut.perm,cut.npixels);
   catch
      [cut.h,temp]=avpix_m(data.hkl(:,:,1),cut.perm,cut.npixels);
      [cut.k,temp]=avpix_m(data.hkl(:,:,2),cut.perm,cut.npixels);
      [cut.l,temp]=avpix_m(data.hkl(:,:,3),cut.perm,cut.npixels);
      [cut.energy,temp]=avpix_m(en,cut.perm,cut.npixels);
   end
end   
if to_cut_file,
   % === store x values for the binned variable or for alternative x-axis
   if isfield(data,'altx'),
      v=data.altx(cut.perm);
   else
      v=data.v(:,:,x); % x-xalues to be binned 
      v=v(cut.perm);
   end
   % === if plotting other property than intensity individual pixels have zero error bar
   if ~isfield(data,'ERR')|isempty(data.ERR),
      err=zeros(length(cut.perm),1);
   else
      err=data.ERR(cut.perm);
   end
   if prod(size(data.det_group))==1, % only one detector, put data in columns 
      cut.pixels=[det_group(cut.perm)' en(cut.perm)' ones(length(cut.perm),1)*ebin ...
            v' data.S(cut.perm)' err(:)];
   else % if matrix det_group has more than one row then 
        % indexing puts data in columns automatically
      cut.pixels=[det_group(cut.perm) en(cut.perm) ones(length(cut.perm),1)*ebin ...
            v data.S(cut.perm) err];
   end   
end   
   
% === establish plot title
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

% === if sample is single crystal and uv orientation and psi_samp are defined, include in title
if isfield(data,'uv')&~isempty(data.uv)&isnumeric(data.uv)&(size(data.uv)==[2 3])&...
      isfield(data,'psi_samp')&~isempty(data.psi_samp)&isnumeric(data.psi_samp)&...
      (prod(ones(size(size(data.psi_samp)==1)))),
   titlestr2=sprintf('{\\bfu}=[%g %g %g], {\\bfv}=[%g %g %g], Psi=({\\bfu},{\\bfki})=%g',...
      data.uv(1,:),data.uv(2,:),data.psi_samp*180/pi);
else
   titlestr2=[];
end
titlestr3=['cut ' deblank(data.axis_label(x,:)) '='  num2str(vx_min,form) ':' num2str(bin_vx,form) ':' num2str(vx_max,form) ];
titlestr3=[titlestr3 ' , ' num2str(vy_min,form) '<' deblank(data.axis_label(y,:)) '<' num2str(vy_max,form)];
if exist('z','var'),	% single crystal data and PSD detectors
   titlestr3=[titlestr3 ' , ' num2str(vz_min,form) '<' deblank(data.axis_label(z,:)) '<' num2str(vz_max,form)];
end
if ~isempty(titlestr2), 	 
	cut.title={titlestr1,titlestr2,titlestr3};
else
	cut.title={titlestr1,titlestr3};
end   

% === establish axis labels for the plot
averages=[', <' deblank(data.axis_label(y,:)) '>=' num2str(vy,form)]; 
if exist('z','var'),	% single crystal sample, analysed as single crystal and with PSD detectors
  	temp=data.u(y,:)*mean([vy_min vy_max])+data.u(z,:)*mean([vz_min vz_max]);
   cut.x_label=[ combil(deblank(data.axis_label(x,:)),data.u(x,:),temp) ' ' deblank(data.axis_unitlabel(x,:))];
   averages=[averages ' <' deblank(data.axis_label(z,:)) '>=' num2str(vz,form)]; 
  	cut.y_label=deblank(data.axis_unitlabel(4,:));	% intensity label
else	
   sample=get(findobj('Tag','ms_sample'),'Value');
   if sample==1,
      analmode=get(findobj('Tag','ms_analysis_mode'),'Value');
   end   
   if (sample==1)&(analmode==1),	% single crystal sample, analysed as single crystal and with conventional (non-PSD) detectors
		temp=data.u(y,:)*mean([vy_min vy_max]);
  		cut.x_label=[ combil(deblank(data.axis_label(x,:)),data.u(x,:),temp) ' ' deblank(data.axis_unitlabel(x,:))];
		cut.y_label=deblank(data.axis_unitlabel(3,:));  	% intensity label 	   
   else   % powder axes
	  	cut.x_label=deblank(data.axis_unitlabel(x,:));
  		cut.y_label=deblank(data.axis_unitlabel(3,:));	% intensity label   
   end
end   

% === if using alternative x-axis change labels accordingly
if isfield(data,'altx'),
   cut.x_label=[data.altx_axislabel data.altx_unitname averages];
else % === otherwise use conventional x-values  
   cut.x_label=[cut.x_label averages];
end   
cut.datafile=data.filename;

% === establish plot symbol
if isfield(data,'symbol'),
	cut.symbol=data.symbol;
else % default symbol
   cut.symbol='wo';
end

% === establish plot axes
% === if intensity range is forced by the user, modify axes
ymin=min(cut.y-cut.e);
ymax=max(cut.y+cut.e);
% === if using alternative x-axis change x-axis range accordingly 
if isfield(data,'altx'),
   xmin=min(cut.x);
   xmax=max(cut.x);
   xrange=xmax-xmin;
   cut.axis=[xmin-0.01*xrange xmax+0.01*xrange ymin-0.2*(ymax-ymin) ymax+0.2*(ymax-ymin)];
else % === otherwise default range  
   cut.axis=[vx_min-bin_vx/2 vx_max+bin_vx ymin-0.2*(ymax-ymin) ymax+0.2*(ymax-ymin)]; 
end   

if exist('i_min','var')&~isempty(i_min)&isnumeric(i_min),
   if (i_min<ymax),
      cut.y_min=i_min;
      cut.axis(3)=i_min;
   else
      disp(sprintf('Given y-range min=%g not suitable for graph.',i_min)); 
      disp('Plotting with default y-range min.');
   end   
end 
if exist('i_max','var')&~isempty(i_max)&isnumeric(i_max),
	if (i_max>ymin),
	   cut.y_max=i_max;
      cut.axis(4)=i_max;
   else
      disp(sprintf('Given y-range max=%g not suitable for graph.',i_max)); 
      disp('Plotting with default y-range max.');
   end              
end

% === send cut to mfit or plot on a plot_cut window
if exist('tomfit','var')&~isempty(tomfit)&islogical(tomfit)&tomfit,   
	% === disp(['Sending cut data directly to mfit']);
   % === do not plot cut in plot_cut window, but send directly to Mfit    
   cut2mfit(cut);
else
   plot_cut(cut);
end

% === save cut to a file, if required
if ~isempty(out_file)&isempty(findstr(out_type,'none')),
	if to_cut_file&(~isempty(findstr(lower(out_type),'mfit'))),
      cut.efixed=data.efixed;	% numeric
   	cut.emode=data.emode;		% numeric
      cut.MspDir=data.MspDir;	% string
      cut.MspFile=data.MspFile;	% string
      cut.sample=data.sample;	% numeric
      if data.sample==1,	% single crystal, so save lattice parameters and crystal orientation as well
      	cut.abc=data.abc;	% [as bs cs; aa bb cc]
         cut.uv=data.uv;	% [ux uy uz; vx vy vz]
         cut.psi_samp=data.psi_samp;	% numeric
      end
   end   
	save_cut(cut,out_file,out_type);   
end
