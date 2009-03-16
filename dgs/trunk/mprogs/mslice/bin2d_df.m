function [intensity,error_int]=bin2d_df(pixels,grid)

% DIGITAL FORTRAN mex function [intensity,error_int]=bin2d_df(pixels,grid)
% used by MSlice slice_spe.m routine to distribute pixelated
% intensity data over a 2d planar grid 
% pixels = (4,Npixels) with vx, vy, int, error for each of the 'Npixels' data points  
% grid=[vx_min vx_max bin_vx vy_min vy_max bin_vy]
% the 2d rectangular region is split into m(v) x n(h)  bins of  size bin_vy(v) x bin_vx(h)
% where n=int(((vx_max+bin_vx)-(vx_min-bin_vx/2))/bin_vx) is number of bins along the vx axis (h)
% the intensity in each bin is the average intensity of all pixels in that bin 
% with boundary convention : [) left inclusive right non-inclusive
% intensity and error_int are matrices (m,n)