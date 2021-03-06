function slice=get_slice(filename)
% Read a slice file with all pixel information
%
%   >> slice=get_slice(filename)
%
%   filename        Name of file from which to read slice
%
%   slice           Structure with the following information:
%
% - If succesfully read, then will contain
%      xbounds: [1x(nx+1) double]
%      ybounds: [1x(ny+1) double]
%            x: [1xn double], n=nx*ny
%            y: [1xn double]
%            c: [1xn double]
%            e: [1xn double]
%      npixels: [1xn double]
%       pixels: [mx7 double], m=sum(npixels(:))
%      x_label: '[ Q_h, 0, 3 ]  in 2.894 �^{-1}'
%      y_label: '[ +0.5   Q_vert, -Q_vert, 3 ]  in 2.506 �^{-1}'
%      z_label: 'Intensity (abs. units)'
%        title: {'map02114.spe, , Ei=447 meV'  [1x56 char]  [1x52 char]}
% x_unitlength: '2.894'
% y_unitlength: '2.5063'
%    SliceFile: 'aaa.slc'
%     SliceDir: 'c:\temp\'
%
% - Additionally, any other footer information in the file of the form <label> = <value> will be added as fields
%      e.g.  as = 2.507    results in the field:     cut.as = '2.507'
%   Multiple occurences of a label result in the created field being a cellarray of strings
%
% Format of slice file:
%   first line:
%     <nx = no. x bins>  <ny = no. y bins>  <x coord of centre of bin(1,1)>  <y coord of same>  <x bin width>  <y bin width>
%
%   then for each bin in the order (1,1)...(nx,1), (1,2)...(nx,2),  ... , (1,ny)...(nx,ny):
%      x(av)   y(av)   I(av)   err(I(av))   npix
%      det_no(1)      eps_centre(1)     d_eps(1)     x(1)     y(1)     I(1)     err(1)
%         .                 .              .           .        .        .        .
%      det_no(npix)   eps_centre(npix)  d_eps(npix)  x(npix)  y(npix)  I(npix)  err(npix)


try
%    [header,x,y,c,e,npixels,pixels,footer]=get_slice_fortran(filename);
    [header,x,y,c,e,npixels,pixels,footer]=libisisexc('IXTutility','getslice',filename);
    nx=header(1); ny=header(2); xorig=header(3); yorig=header(4); dx=header(5); dy=header(6);
    slice.xbounds=xorig+dx.*(linspace(0,nx,nx+1)-0.5);
    slice.ybounds=yorig+dy.*(linspace(0,ny,ny+1)-0.5);
%    slice.x=x'; slice.y=y'; slice.c=c'; slice.e=e'; slice.npixels=npixels'; slice.pixels=pixels';
    slice.x=x; slice.y=y; slice.c=c; slice.e=e; slice.npixels=npixels; slice.pixels=pixels';
    slice.x_label=[];
    slice.y_label=[];
    slice.z_label=[];
    slice.title=[];
    slice.x_unitlength=[];
    slice.y_unitlength=[];
    [slice,added]=read_labels(footer,slice);

catch
    % Open file for reading
    fid=fopen(filename,'rt');
    if fid==-1,
        error([ 'Error opening file ' filename ]);
    end
    % Read x,y,c,e and complete pixel information
    header=fscanf(fid,'%g',6);	% number of data points in the slice
    nx=header(1); ny=header(2); xorig=header(3); yorig=header(4); dx=header(5); dy=header(6);
    slice.xbounds=xorig+dx.*(linspace(0,nx,nx+1)-0.5);
    slice.ybounds=yorig+dy.*(linspace(0,ny,ny+1)-0.5);
    
    n=nx*ny;
    slice.x=zeros(1,n);
    slice.y=zeros(1,n);	% intensities
    slice.c=zeros(1,n);	% errors
    slice.e=zeros(1,n);	% errors
    slice.npixels=zeros(1,n);
    slice.pixels=[];      % pixel matrices
    for i=1:n,
        temp=fscanf(fid,'%g',5);
        slice.x(i)=temp(1);
        slice.y(i)=temp(2);
        slice.c(i)=temp(3);
        slice.e(i)=temp(4);
        slice.npixels(i)=temp(5);
        d=fscanf(fid,'%g',7*slice.npixels(i));
        slice.pixels=[slice.pixels;reshape(d,7,slice.npixels(i))'];
    end
    disp(['Loading slice ( ' num2str(n) ' data points and ' num2str(size(slice.pixels,1)) ' pixels) from file : ']);
    disp(filename);
    % Read footer information
    slice.x_label=[];
    slice.y_label=[];
    slice.z_label=[];
    slice.title=[];
    slice.x_unitlength=[];
    slice.y_unitlength=[];
    [slice,added]=read_labels(fid,slice);
    fclose(fid);
end
slice.c(slice.npixels==0)=NaN;  % bins with no pixels made NaN
slice.e(slice.npixels==0)=0;

if ~added
    disp('Have reached the end of file without finding any label information appended.');
    slice.x_label='x-variable';
    slice.y_label='y-variable';
    slice.z_label='Intensity';
    [pathname,file,ext]=fileparts(filename);
    slice.title=avoidtex([file,ext]);
    slice.x_unitlength='1';
    slice.y_unitlength='1';
    slice.SliceFile=avoidtex([file,ext]);
    slice.SliceDir=[pathname,filesep];
    return;
else
    [pathname,file,ext]=fileparts(filename);
    slice.SliceFile=avoidtex([file,ext]);
    slice.SliceDir=[pathname,filesep];
end
