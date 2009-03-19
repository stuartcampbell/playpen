function pixel_perm=order_m(number_pix,bin_perm)

% number_pix(nbins), bin_perm(nbins)
% === check compatibility between total number of pixels and total number of bins across the input data
npixels=sum(number_pix); % total number of pixels
nbins=length(number_pix);% total number of bins
if length(bin_perm)~=nbins,
   disp(['Incompatibility between the number of bins in the number_pix vector' num2str(nbins)]);
   disp(['and in the number of bins in the permutation matrix' num2str(length(bin_perm))]);
   pixel_perm=[];
   return;
end
%disp(sprintf('Reordering %d pixels',npixels));
pixel_index=zeros(npixels,2);	% (npixels,2)
cumm_npixels=zeros(size(number_pix));
pixel_perm=zeros(npixels,1);

% === run through all pixels and assign bin number and the j'th index in each bin  (global index)
bin=1;	% will index current bin
j=0;		% j'th pixel in current bin
for i=1:npixels,
   j=j+1;	% put one more pixel in current bin
   pixel_index(i,:)=[bin j]; % unsorted pixel index
   if j==number_pix(bin),	% have reached maximum number of pixels in current bin 
      bin=bin+1;	% move on to the next bin
      j=0;
   end
end

for i=1:nbins,
	bin_index(bin_perm(i))=i;
end
% === calculate cummulative number of pixels up to each bin in the new order
cumm_npixels(1)=0;
new_number_pix=number_pix(bin_perm);
for i=2:nbins,
	cumm_npixels(i)=cumm_npixels(i-1)+new_number_pix(i-1);	   
end

% === determine global permutation matrix for the pixel index matrix 
for i=1:npixels,
   pixel_perm(cumm_npixels(bin_index(pixel_index(i,1)))+pixel_index(i,2))=i;
end
