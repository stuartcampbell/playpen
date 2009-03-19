function dataset_4d = IXTdataset_4d( varargin )
%  IXTdataset_2d(IXTbase, 'title', [signal], [error], IXTunits,
%  [x], 'x_label', IXTunits, [x_distribution],
%  [y], 'y_label', IXTunits, [y_distribution])
% 
%  Creates an IXTdataset_2d object with the following elements:
%
%   IXTbase             IXTbase
% 	title				char			Title of dataset for plotting purposes
% 	signal				real    		Signal
% 	error				real    		Standard error
% 	s_axis				IXTaxis			S axis object containing caption and units codes
% 	x					real        	values of bin boundaries along x-axis(if histogram data)
% 						real            values of data point positions along x-axis(if point data)
% 	x_axis				IXTaxis			x axis object containing caption and units codes
% 	x_distribution      logical         x-data distribution data flag
% 	y					real        	values of bin boundaries along y-axis(if histogram data)
% 						real            values of data point positions along y-axis(if point data)
% 	y_axis				IXTaxis			y axis object containing caption and units codes
% 	y_distribution      logical         y-data distribution data flag

dataset_4d.base = IXTbase;
dataset_4d.title=' ';
dataset_4d.signal=zeros(2,2,2,2);
dataset_4d.error=zeros(2,2,2,2);
dataset_4d.s_axis=IXTaxis;
dataset_4d.x1=[0 0];
dataset_4d.x1_axi=IXTaxis;
dataset_4d.x1_distribution=logical(0);
dataset_4d.x2=[0 0];
dataset_4d.x2_axis=IXTaxis;
dataset_4d.x2_distribution=logical(0);
dataset_4d.x3=[0 0];
dataset_4d.x3_axis=IXTaxis;
dataset_4d.x3_distribution=logical(0);
dataset_4d.x4=[0 0];
dataset_4d.x4_axis=IXTaxis;
dataset_4d.x4_distribution=logical(0);
dataset_4d = class(dataset_4d,'IXTdataset_4d');    
if (nargin > 0)           
    dataset_4d = libisisexc('IXTdataset_4d','create',dataset_4d,varargin);
end