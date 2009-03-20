function [x,y,err,xlab,ylab,monitor]=genie(file)
%
% MFIT function [x,y,err,xlab,ylab,monitor]=genie(file)
%	R.C. 4-August-1998 Genie2.3 ascii output file
%
% This is a basic load routine for MFIT, illustrating the required
% syntax. The routine takes the name of a data file (including path) as a
% parameter and returns the column vectors x, y, err.
% On error the routine must exit with datafile=0.

%------------- Open data file---------------------
x=[]; y= []; err=[]; ylab=''; xlab='';

fid=fopen(file,'r');				
if (fid<0) 
	datafile=0;
	return
end

%-------------- Initialize arrays -----------------	
header_end='Bin no.        x             y             e';
fpos=ffind(file,header_end);	% find start of data
header=['Loading Genie2.3 file ' file];
while ftell(fid)<fpos,
   header=str2mat(header,fgetl(fid));
end
disp(header);

%------ Read data line by line --------------------------
fseek(fid,fpos,'bof');
temp=fgetl(fid);
data=fscanf(fid,'%f');
fclose(fid); % close input file
nrows=length(data)/4;
data=reshape(data,4,nrows)';

x=(data(1:nrows-1,2)+data(2:nrows,2))/2;
y=data(1:(nrows-1),3);
err=data(1:(nrows-1),4);
[x,perm]=sort(x);
y=y(perm);
err=err(perm);

%===== Make x and y column labels ===================================
xlab='x';
ylab='y';
monitor = ones(size(y));

% ==== Calculate centre of mass and standard deviation from the mean
ytotal=sum(y(:));
if abs(ytotal)>eps,
   cm=dot(x(:),y(:))/ytotal;
   stdcm=sqrt(dot((x(:)-cm).^2,y(:)/ytotal));
	disp(sprintf('Y-MAX      : %10.5g       CENTER OF MASS : %10.5g',max(y(:)),cm));
	disp(sprintf('GAUSS FWHM : %10.5g       TOTAL Y        : %10.5g',2.355*stdcm,ytotal));   
else   
	disp(sprintf('Y-MAX      : %10.5g       CENTER OF MASS : Not defined',max(y(:))));
	disp(sprintf('GAUSS FWHM : Not defined  TOTAL Y        : %10.5g',ytotal));   
end
disp(sprintf('MIDP       : %10.5g',mean([x(1) x(end)])));
   
