function msk=load_msk(filename,index)

% function msk=load_msk(filename)
% index = pix_981, pix_991 restrict to these spectra, no restriction by default
% === function to read a .msk file

% === open file to read as a text file
fid=fopen(filename,'rt');
if fid==-1,
   disp(['Error opening file ' filename]);
   msk=[];
   return
end
msk=fscanf(fid,'%d');
fclose(fid);
disp(['Loaded .msk file (' num2str(length(msk)) ' spectra) ' filename]);
drawnow;

% === restrict spectra if required
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
	i=((msk>=s1)&(msk<=s2));
   msk=msk(i);
end
