function cut=load_cut(filename)

% function cut=load_cut(filename)
% function to read a .cut file with all pixel information (.cut format) 
% and return structure cut with fields (n=number of data points)
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
%   npixels: [1xn double]
%    pixels: [mx6 double], m=sum(npixels(:))
% 	   title: 'cut\spe750_12s_apr98_all_nesw_cut_i_a.cut'

% === open <filename> for reading
fid=fopen(filename,'rt');
if fid==-1,
   disp([ 'Error opening file ' filename ]);
   cut=[];
   return
end

% === read x,y,e and complete pixel information
n=fscanf(fid,'%d',1);	% number of data points in the cut
drawnow;
cut.x=zeros(1,n);
cut.y=cut.x;	% intensities
cut.e=cut.x;	% errors
cut.npixels=cut.x;
cut.pixels=[];	% pixel matrices
cut.title=avoidtex(stripath(filename));
for i=1:n,
   temp=fscanf(fid,'%f',4);
   cut.x(i)=temp(1);
   cut.y(i)=temp(2);
   cut.e(i)=temp(3);
   cut.npixels(i)=temp(4);
   d=fscanf(fid,'%f',6*cut.npixels(i));
   cut.pixels=[cut.pixels;reshape(d,6,cut.npixels(i))'];
end
disp(['Loading .cut ( ' num2str(n) ' data points and ' num2str(size(cut.pixels,1)) ' pixels) from file : ']);
disp(filename);
cut.x_label=[];
cut.y_label=[];
cut.title=[];

% === check to see if label information is appended at the end of the .cut file
% === typical output looks like this 
%x_label                         = [ 0, k, 0 ]  in 0.840 Å^{-1}, <Energy>=0.0875
%y_label                         = Normalised Intensity (arb. units)
%title                           = irs19151sum.spe 6032.5\muAhrs, 38.95h, \alpha=+0.5
%title                           = cut k=0:0:1.1 , 0.075<Energy<0.1
%MspDir                          = M:\matlab\iris\cs2cucl4\msp\
%MspFile                         = irs19151_cut75_100uev.msp
%efixed                          = 1.8463 
%emode                           = 2      
%sample                          = 1      
%as                              = 9.65   
%bs                              = 7.48   
%cs                              = 12.35  
%aa                              = 90     
%bb                              = 90     
%cc                              = 90     
%ux                              = 0      
%uy                              = 1      
%uz                              = 0      
%vx                              = 0      
%vy                              = 0      
%vz                              = 0      
%psi_samp                        = -90 
%CutFile: 'irs19162ac1\_cut\_det46\_51.cut'
%CutDir: 'm:\matlab\iris\cs2cucl4\cutac1\'

% === read line by line until end of file 
t=fgetl(fid);
% === if not end of file move on to the next line until a line with characters is found
while ~feof(fid)&isempty(t),
	t=fgetl(fid);		% read next complete line
end
if isempty(t),
   disp('Have reached the end of file without finding any label information appended.');
   fclose(fid);
   cut.x_label='x-variable';
   cut.y_label='y-variable';
   [file,pathname]=stripath(filename);
   cut.CutFile=file;
   cut.title=avoidtex(file);   
   return;
end

while (ischar(t))&(~isempty(t(~isspace(t)))),
   pos=findstr(t,'=');
   field=t(1:pos-1);
   field=field(~isspace(field));	% extract field name
   value=t(pos+1:length(t));
	value=deblank(value);	% remove trailing blanks from both the beginning and end of string
 	value=fliplr(deblank(fliplr(value)));	% extract string value
   if ~isfield(cut,field),	% new field 
      cut=setfield(cut,field,value);
   else	% field already exists
      temp=getfield(cut,field);
      if isempty(temp),
         cut=setfield(cut,field,value);         
      elseif ischar(temp),		% make into a cell of strings
         cut=setfield(cut,field,{temp value});
      elseif iscell(temp),
         temp{length(temp)+1}=value;
         cut=setfield(cut,field,temp);
      end      
   end
   t=fgetl(fid);
end
% === cut.title is either a string or a cell of strings
fclose(fid);
[cut.CutFile,cut.CutDir]=stripath(filename);
cut.CutFile=avoidtex(cut.CutFile);
