function bkg=load_fit(filename)

% function bkg=load_fit(filename)

% === open <filename> for reading
fid=fopen(filename,'rt');
if fid==-1,
   disp([ 'Error opening file ' filename ]);
   cut=[];
   return
end

data=[];
% === if not end of file move on to the next line until a line with numbers is found
while ~feof(fid)&isempty(data),
   temp=fgetl(fid);		% read next complete line
   data=sscanf(temp,'%f');
end
if isempty(data),
   disp('Have reached the end of file without finding any data blocks.');
   bkg=[];
   fclose(fid);
   return;
end

% === read now the rest of numbers until the end
data=[data(:); fscanf(fid,'%f')];
fclose(fid);
if rem(length(data),2)~=0,
   disp(sprintf('File does not appear to have the standard 2-column format. File not read.'));
   bkg=[];
   return;
end
data=reshape(data,2,length(data)/2)';
bkg.x=data(:,1);
bkg.y=data(:,2);
bkg.e=0.01*bkg.y;
disp(sprintf('Have read %d data points from file \n%s',length(bkg.x),filename));
bkg.x_label='x_label';
bkg.y_label='y_label';
bkg.title=filename;