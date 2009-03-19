function mask_out = fileread(mask_in,filename)
% mask=fileread(mask,'filename')
% this function will read an ascii type mask file into the IXTmask object
mask_out = libisisexc('IXTmask','fileread',IXTmask,mask_in,filename);

