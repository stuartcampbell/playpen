function sim_cs2cucl4

cd c:\mprogs\mslice
clear mex; mex ms_iris.f

mslice M:\matlab\iris\cs2cucl4\mspac1\irs19151ac1_cut_det46_51.msp
ms_load_data;
ms_calc_proj;
ms_disp;
%return
tic;
% === test simulation 1 mag vs. analytic for the isotropic triangular lattice
data_mc=ms_iris_spe(0,1.5,0.01,[0.15 0.60 0.01 -0.0782 0 0.01 0 0.1 0 0 0.18 0 0 0.261 1],...
   [500 17],[2.3*pi/180 2.3*pi/180 3*pi/180 6.4*pi/180 212e-6]);
data_an=ms_iris_spe(0,1.5,0.01,[0.15 0.60 0.01 1 0 0.01 0 0.1 0 0 0.18 0 0 0 1],...
   [500  3],[2.3*pi/180 2.3*pi/180 3*pi/180 6.4*pi/180 212e-6]);
data_an1=ms_iris_spe(0,1.5,0.01,[0.15 0.60 0.01 1 0 0.01 0 0.1 0 0 0.18 0 0 0 1],...
   [500 19],[2.3*pi/180 2.3*pi/180 3*pi/180 6.4*pi/180 212e-6]);
%data=ms_iris_spe(seed,-0.2,1.4,0.01,...
%   [10 0.64 0.01 0.175 100 0.01 0 0.1 0.1 0 0.1 1 1],[200 4],...
%   [2.3*pi/180 2.3*pi/180 6.4*pi/180 6.4*pi/180 212e-6]);
toc;

% === test simulation 1 mag vs. analytic for the an-isotropic triangular lattice
data=ms_iris_spe(0,1.5,0.01,[0 0.60 0.01 0.175 0 0.01 0 0.1 0 0 0.18 0.15 0.15 0 1],...
   [500 3],[2.3*pi/180 2.3*pi/180 6.4*pi/180 6.4*pi/180 212e-6]);
