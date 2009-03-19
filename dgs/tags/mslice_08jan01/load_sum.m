function [d,e1,e2,header]=load_sum(sum_filename,index)

% function to read .sum files d=[spec no, intensity, error]
% [e1,e2] energy integration range

fid=fopen(sum_filename,'rt');
if fid==-1,
   disp([ 'Error opening file ' sum_filename ]);
   drawnow;
   d=[];
   e1=[];
   e2=[];
   header=[];
   return
end

% FIND BEGINNING OF DATA TABLE
header=[fgets(fid) fgets(fid) fgets(fid) fgets(fid)];
fpos=ffind(sum_filename,'Per no');
fseek(fid,fpos,'bof');
fgets(fid);
fgetl(fid);

% READ IN DATA AND RESHAPE AS A TABLE
data=fscanf(fid,'%f');
fclose(fid);
n=length(data);
data=reshape(data,7,n/7)';
e1=data(1,3);
e2=data(1,4);
d=data(:,[2 6 7]);

if exist('index','var')&~isempty(index)&ischar(index),
   if ~isempty(findstr(lower(index),'pix_981'))|~isempty(findstr(lower(index),'pix_ew')),
    	% extract spectra in PSD E+W banks only
      s1=401;
      s2=2064;
   elseif ~isempty(findstr(lower(index),'pix_991')),
      % extract all PSD spectra E+W+N+S
      s1=401;
      s2=2584;
   elseif ~isempty(findstr(lower(index),'pix_ns')),
      s1=2065;
      s2=2584;
   else
      disp(sprintf('Unknown option %s',index));
      return;
   end
	i=((d(:,1)>=s1)&(d(:,1)<=s2));
   d=d(i,:);
end
disp(sprintf('Loading .sum file (%d spectra from %d to %d ): \n%s',size(d,1),min(d(:,1)),max(d(:,1)),sum_filename));
drawnow;

