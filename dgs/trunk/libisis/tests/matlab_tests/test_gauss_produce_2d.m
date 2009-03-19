function w = test_gauss_produce_2d(x,y,scale,offset,offset2,sigma,corr,sigma2)
% Produces a roughly gaussian IXTdataset_2d object
%   w = test_gauss_produce_2d(x,scale,offset,offset2,sigma)
[x2,y2] = meshgrid(x,y);

if nargin==2
    scale=2;
    offset=0;
    offset2=0;
    sigma=2;
    corr = 1.5;
    sigma2 = 2;   
end

z_temp = gauss2d_bkgd(x2,y2,[scale, offset, offset2, sigma, corr, sigma2, 0, 0, 0]); 
z_temp = z_temp + 0.05*rand(length(y),length(x));

w = IXTdataset_2d(x, y, z_temp', 0.05*ones(size(z_temp')));
