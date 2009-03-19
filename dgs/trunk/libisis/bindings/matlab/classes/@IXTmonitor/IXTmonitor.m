function monitor = ISISEXCmonitor( entry_name, varargin )
% ! Create an ISISEXCmonitor object
% ! REQUIRED INPUT PARAMETERS
% ! monitor  = ISISEXCmonitor( [entry_name], [name], [integral], [error], 
%                              [range], [integral_units], [moments], 
%                              [moments_units], [det]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCmonitor
% ! ==============
% ! NeXus class: NXmonitor
% !
% !	entry_name			char		        name of entry in NeXus file
% ! *name				char		        name of the monitor: whitebeam_1, whitebeam_2,... monochromatic_1, monochromatic_2 ...
% !	integral			real	          	monitor integral
% !	error				real	        	error on monitor integral
% !	range(2)			real	        	integration range
% ! *integral_units		char	         	units in which the integration was performed
% ! *moments			moments	         	various derived quantities for peaks (see separate derived data type)
% !	momentus_units		char	        	units in which the moments are expressed
% ! *det				ISISEXCdetarray	    detector information for the monitor
% !
% !
% ! NOTES:
% ! ------
% !	We omit raw data, ie time_of_flight and data. Also, efficiency. Other information, such as distance, height
% !	etc. that are currently in the NeXus standard can in fact be contained in a detector. So lets use this definition.
% !	We store derived information about peaks
% !



switch nargin
case 0
    % if no input arguments, create a default object

    monitor.entry_name = 'none';    
    monitor.name = 'none';
    monitor.integral=NaN;
    monitor.error=NaN;    
    monitor.range=NaN;    
    monitor.integral_units = 'none';
    monitor.moments = moments;
    monitor.moments_units = 'none';
    monitor.det = ISISEXCdetarray;
    
    
    monitor = class(monitor,'ISISEXCmonitor');
    
case 1
    % if single argument of class ISISEXCmonitor, return it
    if isa(entry_name,'ISISEXCmonitor')
        monitor = entry_name;
    else
        
        error('!! It is not a valid ISISEXCmonitor constructor');
        return
    end
    

    % create object using specified arguments
    
case 9    
    
    check=1;
    
    monitor.entry_name = entry_name;
    
    if(isa(varargin{1},'char')), monitor.name = varargin{1};  else error('1'), check =0; end
    if(isa(varargin{2},'double')&&(length(varargin{2})==1)), monitor.integral = varargin{2};  else  error('2'), check =0; end
    if(isa(varargin{3},'double')&&(length(varargin{3})==1)), monitor.error = varargin{3};  else  error('3'), check =0; end
    if(isa(varargin{4},'double')&&(length(varargin{4})==2)), monitor.range = varargin{4};  else  error('4'), check =0; end
    if(isa(varargin{5},'char')), monitor.integral_units = varargin{5};  else error('5'), check =0; end
    if(isa(varargin{6},'moments')), monitor.moments = varargin{6};  else error('6'), check =0; end
    if(isa(varargin{7},'char')), monitor.moments_units = varargin{7};  else error('7'), check =0; end
    if(isa(varargin{8},'ISISEXCdetarray')), monitor.det = varargin{8};  else error('8'), check =0; end    
    
    if (check==1), 
        monitor = class(monitor,'ISISEXCmonitor'); 
    else 
        error('!! It is not a valid ISISEXCmonitor constructor');
        monitor =[];
    end

    
    
otherwise
    error('!! It is not a valid ISISEXCmonitor constructor');
end



