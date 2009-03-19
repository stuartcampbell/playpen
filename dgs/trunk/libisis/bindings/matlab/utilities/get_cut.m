function cut=get_cut(filename)
% Read a cut file with all pixel information (.cut or Mfit .cut)
%
%   >> cut=get_cut(filename)
%
%   filename        Name of file from which to read cut
%
%   cut             Structure with the following information:
%
% - If succesfully read, then will contain
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
%   npixels: [1xn double]
%    pixels: [mx6 double], m=sum(npixels(:))
%     title: {'map02114.spe, , Ei=447 meV'  [1x56 char]  [1x54 char]}
%   x_label: '[ Q_h, 0, 3 ]  in 2.894 Å^{-1}, <Q_vert>=0.00084693 <Q_l>=3.0052'
%   y_label: 'Intensity (abs. units)'
%   CutFile: 'bog.cut'
%    CutDir: 'c:\temp\'
%
% - Additionally, footer information in the file of the form <label> = <value> will be added as fields
%      e.g.  as = 2.507    results in the field:     cut.as = '2.507'
%   Multiple occurences of a label result in the created field being a cellarray of strings


% Based on original m-file load_cut of R.Coldea. Output should be identical.
% Major addition: fortran reading routine
% Major error with original routine:
%   If value of signal is small, then the f format write can lead to significant rounding:
%   4 Jan 2006  TGP     Altered in .cut and Mfit .cut read section to read g format; see
%                       corresponding alteration in save_cut


try
%    [x,y,e,npixels,pixels,footer]=get_cut_fortran(filename);
    [x,y,e,npixels,pixels,footer]=libisisexc('IXTutility','getcut',filename);
%    cut.x=x'; cut.y=y'; cut.e=e'; cut.npixels=npixels'; cut.pixels=pixels';
    cut.x=x; cut.y=y; cut.e=e; cut.npixels=npixels; cut.pixels=pixels';    
    cut.x_label=[];
    cut.y_label=[];
    cut.title=[];
    [cut,added]=read_labels(footer,cut);

catch
    % Open file for reading
    fid=fopen(filename,'rt');
    if fid==-1,
        error(['Error opening file ' filename ]);
    end
    % Read x,y,e and complete pixel information
    n=fscanf(fid,'%d',1);	% number of data points in the cut
    cut.x=zeros(1,n);
    cut.y=zeros(1,n);	% intensities
    cut.e=zeros(1,n);	% errors
    cut.npixels=zeros(1,n);
    cut.pixels=[];      % pixel matrices
    for i=1:n,
        temp=fscanf(fid,'%g',4);
        cut.x(i)=temp(1);
        cut.y(i)=temp(2);
        cut.e(i)=temp(3);
        cut.npixels(i)=temp(4);
        d=fscanf(fid,'%g',6*cut.npixels(i));
        cut.pixels=[cut.pixels;reshape(d,6,cut.npixels(i))'];
    end
    disp(['Loading .cut ( ' num2str(n) ' data points and ' num2str(size(cut.pixels,1)) ' pixels) from file : ']);
    disp(filename);
    % Read footer information
    cut.x_label=[];
    cut.y_label=[];
    cut.title=[];
    [cut,added]=read_labels(fid,cut);
    fclose(fid);
end


if ~added
    disp('Have reached the end of file without finding any label information appended.');
    cut.x_label='x-variable';
    cut.y_label='y-variable';
    [pathname,file,ext]=fileparts(filename);
    cut.title=avoidtex([file,ext]);
    cut.CutFile=avoidtex([file,ext]);       % If no labels, then Radu *does not* avoidtex the CutFile
    cut.CutDir=[pathname,filesep];          % If no labels, then Radu does not return the CutDir
    return;
else
    [pathname,file,ext]=fileparts(filename);
    cut.CutFile=avoidtex([file,ext]);       % If labels, then Radu *does* avoidtex the CutFile
    cut.CutDir=[pathname,filesep];          % If labels, then Radu returns the CutDir
end
