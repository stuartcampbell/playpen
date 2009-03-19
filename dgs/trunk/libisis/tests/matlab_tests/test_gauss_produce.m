function w = test_gauss_produce(x,scale,offset,sigma)
% Produces a roughly gaussian IXTdataset_1d object
%   w = test_gauss_produce(x,scale,offset,offset_scale)
if nargin==1
    scale = 2; 
    offset = 3; 
    sigma = 2;
end
y_temp = gauss_bkgd(x,[scale,offset,sigma,0,0]) + 0.2*rand(1,length(x));

w = IXTdataset_1d(x, y_temp, 0.1*ones(1,length(x)));
