function dataset_3d = IXTdataset_3d( varargin )
%  IXTdataset_3d(IXTbase, 'title', [signal], [error], IXTaxis,
%  [x], IXTaxis, [x_distribution],
%  [y], IXTaxis, [y_distribution],
%  [z], IXTaxis, [z_distribution])
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
% 	y_distribution      logical         z-data distribution data flag
% 	z					real        	values of bin boundaries along z-axis(if histogram data)
% 						real            values of data point positions along z-axis(if point data)
% 	z_axis				IXTaxis			z axis object containing caption and units codes
% 	z_distribution      logical         z-data distribution data flag

dataset_3d.base = IXTbase;
dataset_3d.title=[' '];
dataset_3d.signal=zeros(2,2,2);
dataset_3d.error=zeros(2,2,2);
dataset_3d.s_axis=IXTaxis;
dataset_3d.x=[0 0];
dataset_3d.x_axis=IXTaxis;
dataset_3d.x_distribution=logical(0);
dataset_3d.y=[0 0];
dataset_3d.y_axis=IXTaxis;
dataset_3d.y_distribution=logical(0);
dataset_3d.z=[0 0];
dataset_3d.z_axis=IXTaxis;
dataset_3d.z_distribution=logical(0);
dataset_3d = class(dataset_3d,'IXTdataset_3d');    
if (nargin > 0)           
    dataset_3d = libisisexc('IXTdataset_3d','create',dataset_3d,varargin);
end