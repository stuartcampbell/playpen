function detarray = ISISEXCdetarray( entry_name, varargin )
% ! Create an ISISEXCdetarray object
% ! REQUIRED INPUT PARAMETERS
% !  detarray  = ISISEXCdetarray( [entry_name], [name], [ndet_tot], [det_no], 
%                                 [delta], [distance], [code], [theta], [phi], 
%                                 [w], [f], [alpha], [det]);
%                                         
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCdetarray
% ! ===============
% ! NeXus class: NXdetector
% !
% !	entry_name			char	name of entry in NeXus file
% ! name				char	name of the detector array
% !	ndet_tot			int		total number of detectors
% ! *	det_no(:)			int		detector index number (use in preference to a name, as e.g. on maps can have O(10^5) detectors
% ! *	delta(:)			real	electronic delay time (microseconds)
% ! 	distance(:)			real	distance from sample (m)
% ! *	code(:)				int		detector type code			(equivalent to TYPE, but integer, not character)
% ! 	theta(:)			real	scattering angle (rad)		([phi] - normally called phi or two_theta)
% ! *	phi(:)				real	azimuthal angle (rad)
% ! *	w(3,:)				real	true dimensions in natural frame (m)
% ! *	f(3,:)				real	false dimensions in natural frame (m)
% ! *	alpha(3,:)			real	vector describing orientation of detector (degrees). See notes elsewhere
% ! *	det(4,:)			real	detector parameters; interpretation depends on code (see notes elsewhere)
% !
% ! Not used:
% !	TYPE is replaced by CODE
% !	GAS_PRESSUE, EFFICIENCY, HEIGHT, RADIUS are implicit in DET
% !	TIME_OF_FLIGHT omitted
% !
% ! Notes:
% ! ------
% !	The order of indices is important - the first index in e.g. alpha is the one that gives the components for a single detector
% !	Rather different to NeXus standard - stores much more information about a detector.
% ! 


switch nargin
case 0
    % if no input arguments, create a default object
    detarray.entry_name = 'none';
    detarray.name = 'none';
    detarray.ndet_tot = int32(0);
    detarray.det_no = int32(0);
    detarray.delta = NaN;
    detarray.distance = NaN;
    detarray.code = int32(0);
    detarray.theta = NaN;
    detarray.phi = NaN;
    detarray.w = NaN;
    detarray.f = NaN;
    detarray.alpha = NaN;
    detarray.det = NaN;
    
    detarray = class(detarray,'ISISEXCdetarray');
    
case 1
    % if single argument of class ISISEXCdetarray, return it
    if isa(entry_name,'ISISEXCdetarray')
        detarray = entry_name;
    else
        
        error('!! It is not a valid ISISEXCdetarray constructor');
        return
    end
    

    % create object using specified arguments
    
case 13    
    
    check=1;
    
    detarray.entry_name = entry_name;
    if(isa(varargin{1},'char')), detarray.name = varargin{1}; else error('!! problems with name'), check=0; end
    if(isa(varargin{2},'int32')&&(length(varargin{2})==1)), detarray.ndet_tot = varargin{2}; else error('!! problems with ndet_tot'), check=0; end
    if(isa(varargin{3},'int32')&&(length(varargin{3})==detarray.ndet_tot)), detarray.det_no = varargin{3}; else error('!! problems with det_no'), check=0; end
    if(isa(varargin{4},'double')&&(length(varargin{4})==detarray.ndet_tot)), detarray.delta = varargin{4}; else error('!! problems with delta'), check=0; end
    if(isa(varargin{5},'double')&&(length(varargin{5})==detarray.ndet_tot)), detarray.distance = varargin{5}; else error('!! problems with distance'), check=0; end
    if(isa(varargin{6},'int32')&&(length(varargin{6})==detarray.ndet_tot)), detarray.code = varargin{6}; else error('!! problems with code'), check=0; end
    if(isa(varargin{7},'double')&&(length(varargin{7})==detarray.ndet_tot)), detarray.theta = varargin{7}; else error('!! problems with theta'), check=0; end
    if(isa(varargin{8},'double')&&(length(varargin{8})==detarray.ndet_tot)), detarray.phi = varargin{8}; else error('!! problems with phi'), check=0; end
    if(isa(varargin{9},'double')&([3,detarray.ndet_tot]==size(varargin{9}))), detarray.w = varargin{9}; else error('!! problems with w'), check=0; end
    if(isa(varargin{10},'double')&([3,detarray.ndet_tot]==size(varargin{10}))), detarray.f = varargin{10}; else error('!! problems with f'), check=0; end
    if(isa(varargin{11},'double')&([3,detarray.ndet_tot]==size(varargin{11}))), detarray.alpha = varargin{11}; else error('!! problems with alpha'), check=0; end
    if(isa(varargin{12},'double')&([4,detarray.ndet_tot]==size(varargin{12}))), detarray.det = varargin{12}; else error('!! problems with det'), check=0; end
    
    
    
    if (check==1), 
        detarray = class(detarray,'ISISEXCdetarray'); 
    else 
        error('!! It is not a valid ISISEXCdetarray constructor');
        detarray=[];
    end
    
    
otherwise
    error('!! It is not a valid ISISEXCdetarray constructor');
end

