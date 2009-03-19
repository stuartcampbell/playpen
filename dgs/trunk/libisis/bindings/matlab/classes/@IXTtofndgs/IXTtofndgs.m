function tofndgs = ISISEXCtofndgs( entry_name, varargin )
% ! Create an ISISEXCtofndgs object
% ! REQUIRED INPUT PARAMETERS
% ! tofndgs  = ISISEXCtofndgs( [entry_name], [title], [start_time],
%                              [end_time], [run_number], [total_charge], 
%                              [total_raw_frames], [total_good_frames], 
%                              [program_name], [command_line], [user], 
%                              [sample], [instrument], [monitor], [data]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCtofndgs   (from: time-of-flight neutron direct geometry spectrometer)
% ! ==============
% ! NeXus class: NXentry
% !	Can hold SPE files, or elastic scattering data.
% !
% !	entry_name          char    Name of entry in NeXus file
% !	title               char    Main title for the whole entry
% !	start_time          char    Start time of entire measurement in iso8601 form [yyyy-mm-dd hh:mm:ss+zzzz  zzz=times zone e.g. 0600 for chicago]
% !	end_time            char    End time of entire measurement
% !	run_number          int
% !	total_charge        real    Number of microamp.hours
% !	total_raw_frames    real    Number of raw frames (i.e. before vetoing)
% !	total_good_frames   real    Number of good frames
% !	program_name        char    Program name that last performed analysis e.g. homer, diag
% !	command_line        char    Associated command line that produced data 
% !
% !	user(:)             ISISEXCuser                 User details (may have more than one user)
% !	sample              ISISEXCsample               Sample
% !	instrument          ISISEXCchopper_instrument   Instrument
% !	monitor             ISISEXCmonitor              Monitors (named whitebeam_1, whitebeam_2, ... monochromatic_1, monochromatic_2, ....)
% !                                                 (so our M1, M2, M3 are whitebeam_1, monochromatic_1, monochromatic_2)
% !	data(:)             ISISEXCdata                 Data arrays. A vector to allow for multiple detector banks (NOT multiple periods)
% !
% !	The data x-axis is energy transfer or time-of-flight, the y-axis is the workspace numbers. For speed of access, we insist that
% !	the workspace numbers for DATA(1),DATA(2) ...are in the same order as stored in the array INSTRUMENT%WORKSPACES%WORK_NO.
% !
% ! Notes:
% ! ------
% !	The x-axis for each of the ISISEXCdataset_2d objects in the allocated array DATA(:) is a list of workspace numbers.
% !	Each of these workspace numbers must appear in the object of type ISISEXCworkarray in the object of type
% !   ISISEXCchopper_instrument i.e. 
% !	The reason why DATA(:) is an array is that we may have workspaces with e.g. different energy bins. There are two
% !	limiting cases:
% !	(1) All workspaces have the same energy bins.
% !			This is the case with .SPE files produced by the VMS HOMER program, where the data is rebinned onto the
% !			energy transfer axis with equal sized bins. In this case, size(data) = 1 i.e.there
% !			only needs to be one DATA entry
% !	(2) Each workspace has different energy bins.
% !			This will be the case if time-of-flight bin boundaries are converted to energy transfer, but without
% !			rebinning. As each detector is at a different distance from the sample in general, the energy bins
% !			will be different for each workspace. In this case, size(data) = no. workspaces.
% !
% !	The data structure can hold other data e.g. we demand that if MONOCHROMATOR%ENERGY=0, then we had a
% !	white beam. Then this structure could have data(i)%x(:) holding the d-spacing. The two limiting examples above
% !	apply equally in this instance.
% !
% !	When an allocatable array of type ISISEXCtofndgs appears in ISISEXCmultiple_tofndgs (which hold multiple SPE files),
% !	the values of INSTRUMENT%WORKSPACES%DET_EFF%NDET_TOT and INSTRUMENT%WORKSPACES%DET%NDET_TOT are zero, and the
% !	detector information arrays are not allocated. Instead, the detctor information in held in two other arrays. See
% !	the definition of ISISEXCmultiple_tofndgs for more details. The same applies for projected dataset
% !	(ISISproj_tofndgs) and for 1D, 2D ... cuts (****).
% !
% !	When an allocatable array of type ISISEXCtofndgs appears in ISISproj_tofndgs (which hold projected datasets), then
% !	the data arrays DATA(:) are not allocated. This is because the data is held on a
% !	pixel-by-pixel basis in an instance of the derived type ISISproj_data, which includes indexing back to 
% !	the particular SPE file and workspace from which that pixel came. The same applieso to 1D, 2D ... cuts (****)
% !
% !
% !	*** add a checksum so that source file for data can be uniquely identified ? (we might end up combining
% !	    different SPE files from the same run number e.g. HET when have scattering plane horizontal and vertical.
% !		A checksum would make it easy to check the files were unique rather than looking through the detector lists ?
% !


switch nargin
case 0
    % if no input arguments, create a default object
    tofndgs.entry_name = 'none';
    tofndgs.title = 'none';
    tofndgs.start_time = 'none';
    tofndgs.end_time = 'none';
    tofndgs.run_number = int32(0);    
    tofndgs.total_charge = NaN;
    tofndgs.total_raw_frames = NaN;
    tofndgs.total_good_frames = NaN;
    tofndgs.program_name = 'none';
    tofndgs.command_line = 'none';    
    tofndgs.user = ISISEXCuser;
    tofndgs.sample = ISISEXCsample;
    tofndgs.instrument = ISISEXCchopper_instrument;
    tofndgs.monitor = ISISEXCmonitor;
    tofndgs.data = ISISEXCdataset_2d;
  
    tofndgs = class(tofndgs,'ISISEXCtofndgs');
    
    
    
case 1
    % if single argument of class ISISEXCtofndgs, return it
    if isa(entry_name,'ISISEXCtofndgs')
        tofndgs = entry_name;
    else
        error('!! It is not a valid ISISEXCtofndgs constructor ');
        return
    end
    

    % create object using specified arguments
    
case 15    
    
    check=1;
    
    tofndgs.entry_name = entry_name;
    if(isa(varargin{1},'char')), tofndgs.title = varargin{1}; else check =0; end
    if(isa(varargin{2},'char')), tofndgs.start_time = varargin{2};  else check =0; end
    if(isa(varargin{3},'char')), tofndgs.end_time = varargin{3};  else check =0; end
    if(isa(varargin{4},'int32')&&(length(varargin{4})==1)), tofndgs.run_number = varargin{4}; else check =0; end
    if(isa(varargin{5},'double')&&(length(varargin{5})==1)), tofndgs.total_charge = varargin{5}; else check =0; end
    if(isa(varargin{6},'double')&&(length(varargin{6})==1)), tofndgs.total_raw_frames = varargin{6}; else check =0; end
    if(isa(varargin{7},'double')&&(length(varargin{7})==1)), tofndgs.total_good_frames = varargin{7}; else check =0; end
    if(isa(varargin{8},'char')), tofndgs.program_name = varargin{8};  else check =0; end
    if(isa(varargin{9},'char')), tofndgs.command_line = varargin{9};  else check =0; end    
    if(isa(varargin{10},'ISISEXCuser')), tofndgs.user = varargin{10};  else check =0; end     
    if(isa(varargin{11},'ISISEXCsample')), tofndgs.sample = varargin{11};  else check =0; end     
    if(isa(varargin{12},'ISISEXCchopper_instrument')), tofndgs.instrument = varargin{12};  else check =0; end      
    if(isa(varargin{13},'ISISEXCmonitor')), tofndgs.monitor = varargin{13};  else check =0; end  
    if(isa(varargin{14},'ISISEXCdataset_2d')), tofndgs.data = varargin{14};  else check =0; end      

    
    if (check==1), 
        tofndgs = class(tofndgs,'ISISEXCtofndgs'); 
    else 
        error('!! It is not a valid ISISEXCtofndgs constructor ');
        tofndgs=[];
    end
    
otherwise
    error('!! It is not a valid ISISEXCtofndgs constructor ');
end
