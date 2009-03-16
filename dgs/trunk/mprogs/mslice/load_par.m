function data=load_par(par_filename)

% function data=load_par(filename)
% to read .par file
% data=rows of [l2(m) theta(deg) psi(deg) width(m) height(m)]


% === return if error opening par_filename
if ~exist('par_filename','var')|isempty(par_filename),
   disp(['Incorrect filename ' par_filename]);
   data=[];
   return;
end
fid=fopen(par_filename,'rt');
if fid==-1,
   disp(['Error opening file ' par_filename]);
   data=[];
   return
end

% === load detector information in .par format
fid=fopen(par_filename,'rt');
n=fscanf(fid,'%5d \n',1);
disp(['Loading .par file with ' num2str(n) ' detectors: ' par_filename]);
temp=fgetl(fid);
data=sscanf(temp,'%f');
cols=length(data); % number of columns 5 or 6
data=[data;fscanf(fid,'%f')];
fclose(fid);
data=reshape(data,cols,n)';
