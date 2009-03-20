function data_out=smooth_spe(data_in,nsmooth)

%
% function data_out=smooth_spe(data_in,nsmooth)
% Smoothing by repeated normalised convolution with a 3x3 matrix
%

data_out=data_in;
if nsmooth==0,
    return
end

% === define convolution matrix A 
%A=[1 1 1; 1 1 1; 1 1 1];
A=[0.1 0.2 0.1; 0.2 0.8 0.2; 0.1 0.2 0.1];
%A=[0.01 0.2 0.01; 0.01 1 0.01; 0.01 0.2 0.01];
%A=[0 0.1 0; 0 0.4 0; 0 0.1 0]';
disp(['Smoothing by repeated normalised convolution (' num2str(nsmooth) ' time(s))']);
disp(['with the 3x3 matrix :']);
for i=1:3,
	disp(sprintf('%g  %g  %g',A(i,:)));   
end
%A=A/sum(A(:));	% normalise convolution matrix

% === convolve data with smoothing matrix A
S=data_in.S;
ERR=data_in.ERR;

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
data_out.S=S;
data_out.ERR=ERR;
data_out.title_label=[data_in.title_label ', s=' num2str(nsmooth)];
