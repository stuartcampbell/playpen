function type=filetype(filename)
% type = filetype('filename')
% function returns either 1 or 2 depending on whether ascii or binary file
if ischar(filename)
    type=libisisexc('IXTutility','filetype',filename);
else
    disp('bad input to filetype')
end
