function multiple_tofndgs = ISISEXCmultiple_tofndgs( entry_name, varargin )
% ! Create an ISISEXCmultiple_tofndgs object
% ! REQUIRED INPUT PARAMETERS
% ! multiple_tofndgs  = ISISEXCmultiple_tofndgs( [entry_name], [title],
%                                                [dataset], [det_eff], [det]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCmultiple_tofndgs
% ! =======================
% ! NeXus class: NXentry
% !	For holding multiple SPE files. The chopper instruments need not be the same, of have the same workspace=> spectra
% !	mapping.
% !
% !	entry_name      char                Name of entry in NeXus file
% !	title           char                Main title for the whole entry
% !	dataset(:)      ISISEXCtofndgs      Data (less detector information)
% !	det_eff         ISISEXCdetarray     Effective detector information for the workspaces in all the datasets
% !	det             ISISEXCdetarray     Detector information for all the datasets
% !	
% !	Notes:
% !	------
% !	In any instance of ISISEXCmultiple_tofndgs, the detector information will be absent from all
% !	DATASET(I)%INSTRUMENT%WORKSPACES. Instead, it will have been collected into DET_EFF and DET. This is so that
% !	when copmbining many tens of data from e.g. MAPS, it is not necessary to hold multiple copies of the same
% !	data. The index arrays in all of DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND and 
% !	DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND will need to be altered accordingly.
% !
% !	We insist that the detector information is collected as follows:
% !		* if DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND  /= DATASET(J=1,I-1)%INSTRUMENT%WORKSPACES%DET_EFF_IND, append
% !		DATASET(I)%INSTRUMENT%WORKSPACES%DET_EFF_IND to DET_EFF
% !		* Similarly, if DATASET(I+1)%INSTRUMENT%WORKSPACES%DET_IND  /= DATASET(J=1,I-1)%INSTRUMENT%WORKSPACES%DET_IND, append
% !		DATASET(I)%INSTRUMENT%WORKSPACES%DET_IND to DET
% !	i.e. the entire detector array is appended, even if there are only minor differences compared to previous detector
% !	information, but consider the cases of the effective detector information for the workspaces and the physical detectors
% !	as separate cases.
% !	This is most efficient in the cases where there are SPE files being combined which have the same mapping and spectra.dat
% !	files, or different mapping files but the same spectra.dat.
% !
% !	We can always recover an instance of ISISEXCtofndgs from ISISEXCmultiple_tofndgs without loss of information.
% !



switch nargin
case 0
    % if no input arguments, create a default object
    multiple_tofndgs.entry_name = 'none';
    multiple_tofndgs.title = 'none';
    multiple_tofndgs.dataset = ISISEXCtofndgs;
    multiple_tofndgs.det_eff = ISISEXCdetarray;
    multiple_tofndgs.det = ISISEXCdetarray;
    
    multiple_tofndgs = class(multiple_tofndgs,'ISISEXCmultiple_tofndgs');
    
    
    
case 1
    % if single argument of class ISISEXCmultiple_tofndgs, return it
    if isa(entry_name,'ISISEXCmultiple_tofndgs')
        multiple_tofndgs = entry_name;
    else
        error('!! It is not a valid ISISEXCmultiple_tofndgs constructor ');
        return
    end
    

    % create object using specified arguments
    
case 5    
    
    check=1;
    
    multiple_tofndgs.entry_name = entry_name;
    if(isa(varargin{1},'char')), multiple_tofndgs.title = varargin{1}; else check =0; end
    if(isa(varargin{2},'ISISEXCtofndgs')), multiple_tofndgs.dataset = varargin{2};  else check =0; end  
    if(isa(varargin{3},'ISISEXCdetarray')), multiple_tofndgs.det_eff = varargin{3};  else check =0; end      
    if(isa(varargin{4},'ISISEXCdetarray')), multiple_tofndgs.det = varargin{4};  else check =0; end  
    
    if (check==1), 
        multiple_tofndgs = class(multiple_tofndgs,'ISISEXCmultiple_tofndgs'); 
    else 
        error('!! It is not a valid ISISEXCmultiple_tofndgs constructor ');
        multiple_tofndgs=[];
    end
    
otherwise
    error('!! It is not a valid ISISEXCmultiple_tofndgs constructor ');
end

