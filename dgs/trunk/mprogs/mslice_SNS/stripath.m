function [name,pathname]=stripath(filename)

% removes path from full filename
% ex. [name,pathname]=stripath('c:\radu\matlab\anal_spe\stripath.m') --> ['stripath.m','c:\radu\matlab\anal_spe\'] 
% R.C. 23-Aug-1998

if ~exist('filename','var')|isempty(filename)|~ischar(filename),
   disp('[Empty filename]');
   name=[];
   pathname=[];
   return
end

a=version;
if (a(1)<=5)&isvms,
    	index=findstr(filename,']');   
else   
   index=findstr(filename,filesep);
end
if ~isempty(index),
	name=filename(max(index)+1:length(filename));
	pathname=filename(1:max(index));   
else
   pathname=[pwd filesep];
   name=filename;
end