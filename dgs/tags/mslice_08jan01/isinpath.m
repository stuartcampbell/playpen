function flag=isinpath(directory)

% function flag=isinpath
if isempty(directory)|~ischar(directory),
   disp(['Input parameter for function isinpath has to be a directory name']);
   flag=[];
   help isinpath;
   return;
end

n=length(directory);
if strcmp(directory(n),'\')
   directory=directory(1:(n-1));
end
flag=~isempty(findstr(path,[directory '\;'))
