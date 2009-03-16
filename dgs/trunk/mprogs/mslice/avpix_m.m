function [xx,std_dev]=avpix_m(x,perm,number_pix)

% function [xx,std_dev]=avpix_df(x,perm,number_pix)
% compute averages for pixels into bins given the recipe in 
% perm = global pixel index in matrix (ndet,ne) in order of going into bins
% number_pix(nbins) = number of pixels per bins in successive order
% sum(number_pix) must equal length(perm) = total number of contributing pixels
% xx(nbins), std_dev(nbins) will be the average and standard deviations
% per bin, x-values are from matrix x(ndet,ne)
% xx,std_dev will have the same vector dimensions as number_pix, (1,nbins) or (nbins,1)
% matlab version in avpix_m.m to emulate the fortran code avpix_df.f
% Radu Coldea 22-Oct-2000

disp('Execute matlab version of avpix_m.m');
% === read total number of pixels and determine total number of bins 
npixels=length(perm); % total number of contributing pixels
nbins=length(number_pix);	% total number of x bins

% === initialize variables and check consistency of binning recipe
xx=zeros(size(number_pix));
std_dev=zeros(size(number_pix));
cumm_pixels=0;
for i=1:nbins,
   %xx(i)=0;       
   %std_dev(i)=0;
   cumm_pixels=cumm_pixels+number_pix(i);
end
if npixels~=cumm_pixels,
   disp('Error, binning recipe and given number of pixels not compatible');
   xx=[];
   std_dev=[];
   return;
end

%=== run through all pixels, put them in bins and 
%=== calculate average and standard deviation
cumm_pixels=0;
j=1;
for i=1:npixels,
   cumm_pixels=cumm_pixels+1;	% add current pixel to the current bin
   xx(j)=xx(j)+x(perm(i));	% partial sum of x-values
   std_dev(j)=std_dev(j)+x(perm(i))^2;	% partial sum of x^2-values
   if cumm_pixels==number_pix(j),	% have reached maximum number of pixels in current bin
      xx(j)=xx(j)/number_pix(j);	% <x> 
      if number_pix(j)==1,
         std_dev(j)=0;
      else
         std_dev(j)=sqrt(std_dev(j)/number_pix(j)-xx(j)^2)*...
            sqrt(number_pix(j)/(number_pix(j)-1));
      end
      j=j+1;	% move on to the next bin 
      cumm_pixels=0; % start with no pixels in next bin
   end
end