function workarray = ISISEXCworkarray( entry_name, varargin )
% ! Create an ISISEXCworkarray object
% ! REQUIRED INPUT PARAMETERS
% ! workarray  = ISISEXCworkarray( [entry_name], [name], [nwork_tot],
%                                  [work_no], [nspec], [spec_ind], 
%                                  [det_eff_ind], [nspec_tot], [spec_no],
%                                  [masked], [ndet], [det_ind], [det_eff], 
%                                  [det]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCworkarray
% ! ================
% ! NeXus class: None
% !	To hold the defining information for all the Homer workspaces for a single run, and which spectra have been masked.
% !
% !	The detector information will be held only if the data structure originates from a single .SPE file.
% !	When several SPE files are combined, or when cuts are made, the detector
% !	information and the effective detector parameters for the workspaces will be held in two global arrays
% !	(ee ISISEXCmultiple_tofndgs for more information). In this case, we insist that ISISEXCworkarray%det_eff%ndet_tot=0,
% !	ISISEXCworkarray%det%ndet_tot=0, and that the allocatable arrays in ISISEXCworkarray%det_eff & ISISEXCworkarray%det
% !	are not allocated.
% !
% !
% !	entry_name				char	Name of entry in NeXus file
% ! 	name				char	Name of the workspace array
% !
% ! workspaces:
% ! -----------
% !	nwork_tot				int		Number of workspaces
% !	work_no(1:nwork_tot)	int		Workspace numbers
% !	nspec(1:nwork_tot)		int		Number of contributing spectra to each workspace (before any masking)
% !	spec_ind(1:nwork_tot)	int		Index of first spectrum number in the array spec_no defined below.
% !	det_eff_ind(1:nwork_tot)int		Index of effective detector in the derived type ISISEXCdetarray with name DET_EFF
% !								   in derived type ISISEXCtofndgs ('eff' stands for 'effective')
% !
% ! spectra:
% ! ---------
% !	nspec_tot				int		Number of spectra in all the workspaces [must equal sum(nspec)]
% !	spec_no(1:nspec_tot)	int		Spectrum numbers for the workspaces. The spectra corresponding to workspace work_no(i) are
% !								   spec_no(spec_ind(i)) ... spec_no(spec_ind(i)) + nspec(i) - 1
% !	masked(1:nspec_tot)		log		Mask array for spectra. If masked, then =.TRUE., if unmasked then = .FALSE.
% !	ndet(1:nspec_tot)		int		Number of detectors contributing to each spectrum
% !	det_ind(1:nspec_tot)	int		Index of first detector number in the derived type ISISEXCdetarray with name DET
% !								   in derived type ISISEXCtofndgs. The detectors corresponding to spectrum spec_no(i) are
% !								   det_ind(spec_no(i)) ... det_ind(spec_no(i)) + ndet(spec_no(i)) - 1
% !
% ! detectors:
% ! ----------
% !
% !	det_eff		ISISEXCdetarray		Effective detector information for the summed detectors that contribute to the
% !								   workspace.
% !	det			ISISEXCdetarray		Detector information of the individual detectors that contribute to the workspaces
% !
% !
% !	For speed of access, we insist that the workspace numbers for ISISEXCtofndgs%DATA(1), ISISEXCtofndgs%DATA(2) ...are in the same
% !	order as stored in the array WORK_NO.
% !
% !	This structure is limited to the case where each detctor appears in at most one spectrum, and each spectrum appears in at most
% !	one workspace.
% !
% !	The arrays in DET_EFF and DET will not be allocated in the case when the data file is a multiple  SPE file or a 1D, 2D ... cut. 
% !
% !	The reason why the detector information is kept in a separate derived type is because in the general case when .SPE files or
% !	cuts are created from many .SPE files, then the detector information from one instrument does not need to be repeated.
% !	It can be seen by all workspaces independent of the .SPE file from which they come.
% !
% ! Notes:
% ! ------
% ! It is not necessary to hold the total number of workspaces and spectra, as they can be found using F90 intrinsic functions.
% ! However, it is convenient to do so, as (i) we should check that the arrays are allocated before using SIZE, and also
% ! we can use NWORK_TOT = 0 as a short-hand to determine if the arrays allocated (so long as we keep it up-to-date)
% !
% ! *** we might want to split up the workspace and spectrum information ??




switch nargin
case 0
    % if no input arguments, create a default object
    workarray.entry_name = 'none';
    workarray.name = 'none';
    % ! workspaces:
    workarray.nwork_tot = int32(0);
    workarray.work_no = int32(0);
    workarray.nspec = int32(0);
    workarray.spec_ind = int32(0);
    workarray.det_eff_ind = int32(0);
    % ! spectra:    
    workarray.nspec_tot = int32(0);
    workarray.spec_no = int32(0);
    workarray.masked = logical(0);
    workarray.ndet = int32(0);
    workarray.det_ind = int32(0);
    % ! detectors:
    workarray.det_eff = ISISEXCdetarray;
    workarray.det = ISISEXCdetarray;
        
    
    
    workarray = class(workarray,'ISISEXCworkarray');
    
case 1
    % if single argument of class ISISEXCworkarray, return it
    if isa(entry_name,'ISISEXCworkarray')
        workarray = entry_name;
    else
        
        error('!! It is not a valid ISISEXCworkarray constructor');
        return
    end
    

    % create object using specified arguments
    
case 14    
    
    check=1;
    
    workarray.entry_name = entry_name;
    if(isa(varargin{1},'char')), workarray.name = varargin{1};  else check =0; end
    if(isa(varargin{2},'int32')&&(length(varargin{2})==1)),workarray.nwork_tot = varargin{2};  else check =0; end
    if(isa(varargin{3},'int32')&&(length(varargin{3})==varargin{2})),workarray.work_no = varargin{3};  else check =0; end
    
    if(isa(varargin{4},'int32')&&(length(varargin{4})==varargin{2})),workarray.nspec = varargin{4};  else check =0; end
    if(isa(varargin{5},'int32')&&(length(varargin{5})==varargin{2})),workarray.spec_ind = varargin{5};  else check =0; end
    if(isa(varargin{6},'int32')&&(length(varargin{6})==varargin{2})),workarray.det_eff_ind = varargin{6};  else check =0; end
    
    if(isa(varargin{7},'int32')&&(length(varargin{7})==1)),workarray.nspec_tot = varargin{7};  else check =0; end
    if(isa(varargin{8},'int32')&&(length(varargin{8})==varargin{7})),workarray.spec_no = varargin{8};  else check =0; end
    if(isa(varargin{9},'logical')&&(length(varargin{9})==varargin{7})),workarray.masked = varargin{9};  else check =0; end    
    if(isa(varargin{10},'int32')&&(length(varargin{10})==varargin{7})),workarray.ndet = varargin{10};  else check =0; end    
    if(isa(varargin{11},'int32')&&(length(varargin{11})==varargin{7})),workarray.det_ind = varargin{11};  else check =0; end   

    if(isa(varargin{12},'ISISEXCdetarray')), workarray.det_eff = varargin{12};  else check =0; end
    if(isa(varargin{13},'ISISEXCdetarray')), workarray.det = varargin{13};  else check =0; end    
    
    
    if (check==1), 
        workarray = class(workarray,'ISISEXCworkarray'); 
    else 
        error('!! It is not a valid ISISEXCworkarray constructor');
        workarray =[];
    end

    
    
    
    
otherwise
    error('!! It is not a valid ISISEXCworkarray constructor');
end


