function peaks = IXTpeaks( varargin )
% ! Create an IXTpeaks object
% ! REQUIRED INPUT PARAMETERS
% ! peaks  = IXTpeaks( [monitor_no], [integral], [error], 
%                              [range], [integral_units], [moments], 
%                              [moments_units], [det]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! IXTpeaks
% ! ==============
% ! NeXus class: NXpeaks
% !
% !	entry_name			char		        name of entry in NeXus file
% ! *name				char		        name of the peaks: whitebeam_1, whitebeam_2,... monochromatic_1, monochromatic_2 ...
% !	integral			real	          	peaks integral
% !	error				real	        	error on peaks integral
% !	range(2)			real	        	integration range
% ! *integral_units		char	         	units in which the integration was performed
% ! *moments			moments	         	various derived quantities for peaks (see separate derived data type)
% !	momentus_units		char	        	units in which the moments are expressed
% ! *det				ISISEXCdetarray	    detector information for the peaks
% !
% !
% ! NOTES:
% ! ------
% !	We omit raw data, ie time_of_flight and data. Also, efficiency. Other information, such as distance, height
% !	etc. that are currently in the NeXus standard can in fact be contained in a detector. So lets use this definition.
% !	We store derived information about peaks
% !

    % if no input arguments, create a default object

    peaks.base = IXTbase;    
    peaks.monitor_no = [int32(1)];
    peaks.integral=IXTdatum_array;
    peaks.irange_low=[0.0];    
    peaks.irange_high=[0.0];     
    peaks.integral_units = 'none';
    peaks.moments = IXTmoments;
    peaks.moments_units = 'none';
   
    peaks = class(peaks,'IXTpeaks');
if (nargin > 0)
    peaks = libisisexc('IXTpeaks','create',peaks,varargin);
end  


