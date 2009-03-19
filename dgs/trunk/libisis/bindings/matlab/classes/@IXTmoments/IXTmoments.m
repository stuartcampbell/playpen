function moments = IXTmoments( varargin )
%moments Create an IXTmoments object 

% !-----------------------------------------------------------------------------------------------------------------------------------
% ! IXTmoments
% ! -------
% !	Contains various information about moments of a peak
% !
% ! base                IXTbase
% !	area				IXTdatum_array	area of peak after background subtraction
% !	bkgd_xmean			real	background level at the value of the first moment of x
% !	bkgd_error			real	error of constant background at that position
% !	xmax				real	position of the peak maximum
% !	c_fwhh				real	the half-way position between the peak half-heights
% !	fwhh				real	peak fwhh
% !	xmean				IXTdatum_array	first moment, <x>
% !	sigma				IXTdatum_array	second moment, <(x-<x>)^2>
% !	g1					IXTdatum_array	skewness = <(x-<x>)^3>/(<(x-<x>)^2>)^(3/2)
% !	g2					IXTdatum_array	kurtosis = <(x-<x>)^4>/(<(x-<x>)^2>)^2  -
% !
% ! (For definitions of skewness and kurtosis, see Kendall and Stewart Vo1.1 pp 87-89)
% ! (Note that Kurtosis = 0 for a Gaussian)
    % if no input arguments, create a default object

    moments.base = IXTbase;
    moments.area=IXTdatum_array;
    moments.bkgd_xmean=[0.0];
    moments.bkgd_error=[0.0];
    moments.xmax=[0.0];
    moments.c_fwhh=[0.0];
    moments.fwhh=[0.0];
    moments.xmean=IXTdatum_array;
    moments.sigma=IXTdatum_array;
    moments.g1=IXTdatum_array;
    moments.g2=IXTdatum_array;
    
    moments = class(moments,'IXTmoments');
if (nargin > 0)
    moments = libisisexc('IXTmoments','create',moments,varargin);
end  