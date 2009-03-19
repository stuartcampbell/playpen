function proj_tofndgs = ISISEXCproj_tofndgs( entry_name, varargin )
% ! Create an ISISEXCproj_tofndgs object
% ! REQUIRED INPUT PARAMETERS
% ! proj_data  = ISISEXCproj_data( [entry_name], [title], [dataset],
%                                  [proj_info], [proj_data], [det_eff], [det]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCproj_tofndgs
% ! ===================
% ! NeXus class: None
% !	For holding one or multiple SPE file data in terms of an arbitrary number of projection axes.
% !	For comments on the allocation of detector information and data arrays in TYPE(ISISEXCtofndgs) DATASET(:), see ISISEXCtofndgs.
% !	For comments on how detector information is stored in DET_EFF and DET, see ISISEXCmultiple_tofndgs.
% !
% !	entry_name		char				Name of entry in NeXus file
% !	title			char				Main title for the whole entry
% !	dataset(:)		ISISEXCtofndgs		Data (less detector information and data arrays))
% !	proj_info		ISISEXCproj_info	Information about the projection axes
% !	proj_data		ISISEXCproj_data	Intensity and coordinates in terms of projection axes for each pixel
% !	det_eff			ISISEXCdetarray		Effective detector information for the workspaces in all the datasets
% !	det				ISISEXCdetarray		Detector information for all the datasets
% !



switch nargin
case 0
    % if no input arguments, create a default object
    proj_tofndgs.entry_name = 'none';
    proj_tofndgs.title = 'none';
    proj_tofndgs.dataset = ISISEXCtofndgs;
    proj_tofndgs.proj_info = ISISEXCproj_info;
    proj_tofndgs.proj_data = ISISEXCproj_data;
    proj_tofndgs.det_eff = ISISEXCdetarray;
    proj_tofndgs.det = ISISEXCdetarray;    
    
    proj_tofndgs = class(proj_tofndgs,'ISISEXCproj_tofndgs');

    
case 1
    % if single argument of class ISISEXCproj_tofndgs, return it
    if isa(entry_name,'ISISEXCproj_tofndgs')
        proj_tofndgs = entry_name;
    else
        
        error('!! It is not a valid ISISEXCproj_tofndgs constructor');
        return
    end
    

    % create object using specified arguments
    
case 7    
    
    check=1;
    if(get_n_axes( varargin{3} )== get_n_axes(varargin{4})),
    
        proj_tofndgs.entry_name = entry_name;
        if(isa(varargin{1},'char')), proj_tofndgs.title = varargin{1}; else error('!! problems with title'), check=0; end
        if(isa(varargin{2},'ISISEXCtofndgs')), proj_tofndgs.dataset = varargin{2}; else error('!! problems with dataset'), check=0; end
        if(isa(varargin{3},'ISISEXCproj_info')), proj_tofndgs.proj_info = varargin{3}; else error('!! problems with proj_info'), check=0; end
        if(isa(varargin{4},'ISISEXCproj_data')), proj_tofndgs.proj_data = varargin{4}; else error('!! problems with proj_data'), check=0; end
        if(isa(varargin{5},'ISISEXCdetarray')), proj_tofndgs.det_eff = varargin{5}; else error('!! problems with det_eff'), check=0; end
        if(isa(varargin{6},'ISISEXCdetarray')), proj_tofndgs.det = varargin{6}; else error('!! problems with det'), check=0; end
        
    else
        error('!! problems with the number of pixels'), check=0;
    end    
        
    if (check==1), 
        proj_tofndgs = class(proj_tofndgs,'ISISEXCproj_tofndgs'); 
    else 
        error('!! It is not a valid ISISEXCproj_tofndgs constructor');
        proj_tofndgs=[];
    end
    
    
otherwise
    error('!! It is not a valid ISISEXCproj_tofndgs constructor');
end

