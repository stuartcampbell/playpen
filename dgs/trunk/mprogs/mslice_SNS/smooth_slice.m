function slice_out=smooth_slice(slice_in,nsmooth)

%
% function slice_out=smooth_slice(slice_in,nsmooth)
% Smoothing by repeated normalised convolution with a 3x3 matrix
%

slice_out=slice_in;
if nsmooth==0,
    return
end

% === define convolution matrix A 
%A=[1 1 1; 1 1 1; 1 1 1];
A=[0.1 0.2 0.1; 0.2 0.8 0.2; 0.1 0.2 0.1];
disp(['Smoothing by repeated normalised convolution (' num2str(nsmooth) ' time(s))']);
disp(['with the 3x3 matrix :']);
for i=1:3,
	disp(sprintf('%g  %g  %g',A(i,:)));   
end
%A=[0.001 0.001 0.001; 0.001 1 0.001; 0.001 0.001 0.001];
%A=A/sum(A(:));	% normalise convolution matrix

% === convolve data with smoothing matrix A
S=slice_in.intensity;
ERR=slice_in.error_int;

index=isnan(S);	% store positions of bins with no data
weight=conv2(double(~index),A,'same');	% calculate weight matrix for each bin 
weight(index)=NaN;	% put NaN weight in bins with no data

for i=1:nsmooth,
   S(index)=0;		% for convolution put zero where there is no data, these bins will not contribute to the convolution
   ERR(index)=0;
   S=conv2(S,A,'same');
   ERR=sqrt(conv2(ERR.^2,A.*A,'same'));  
   S(~index)=S(~index)./weight(~index);	% divide convolution at bins with data by weight to maintain normalization
   ERR(~index)=ERR(~index)./weight(~index);
end 

S(index)=NaN;	% restore bins with no initial data
ERR(index)=NaN;
slice_out.intensity=S;
slice_out.error_int=ERR;
slice_out.title=slice_in.title;
slice_out.title{1}=[slice_in.title{1} ', s=' num2str(nsmooth)];
