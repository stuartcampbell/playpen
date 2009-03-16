function data=add_spe_example

% function data=add_spe_example
% example how to add sep files using function add_spe
% Radu Coldea 02-Oct-1999

% define relative or absolute weights of different spe files (these numbers can be uAhrs)
weights=[222.6 267.7 250.1 233.8 250.1];

% give path and filenames of spe files to be added
spedir='m:\matlab\iris\ornl\';
files={'irs17866.spe','irs17867.spe','irs17868.spe',...
       'irs17869.spe','irs17870.spe'}; 

% give filename to save results 
fileout=[spedir 'irs17866sum.spe']; 
data=add_spe(weights,spedir,files,fileout);