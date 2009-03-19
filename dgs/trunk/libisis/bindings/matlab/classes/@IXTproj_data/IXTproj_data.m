function proj_data = ISISEXCproj_data( entry_name, varargin )
% ! Create an ISISEXCproj_data object 
% ! REQUIRED INPUT PARAMETERS
% ! proj_data  = ISISEXCproj_data( [entry_name], [name], [s], [e], [u],
%                                  [id], [iw], [xlo], [xhi]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCproj_data
% ! ================
% ! NeXus class: None
% !	For holding the coordinates of the centres of the pixels along the projection axes, with an index to the
% !	unprojected dataset and the workspace from which it came.
% !
% !
% !	entry_name		char		Name of entry in NeXus file
% !	name			char		Name
% !	s(:)			real		Intensity
% !	e(:)			real		Standard error
% !	u(:,:)			real		Coordinates of centre of pixel along projection axes [u(n_axes, n_pixels)]
% !	id(:)			int			Index of dataset from which pixel came
% !	iw(:)			int			Index of workspace for that dataset
% !	xlo(:)			real		Lower bin boundary
% !	xhi(:)			real		Upper bin boundary
% !
% !	IW is the workspace index, NOT the workspace number. For example, in ISISEXCproj_tofndgs, for the nth pixel
% !	dataset(id(n))%instrument%workspaces%work_no(iw(n)) gives the actual workspace number.
% !
% ! Notes:
% ! ------
% !	The more rapidly varying dimension (first dimension) of U gives the coordinates in the projection axes for a single pixel.
% !
% !
% ! *** No constraint is imposed on the order of the pixels. However, we can imagine that order may be helpful
% !	e.g. in order of idataset, then in order of iw for each idataset, then in order of xlo for that workspace
% !




switch nargin
case 0
    % if no input arguments, create a default object
    proj_data.entry_name = 'none';
    proj_data.name = 'none';
    proj_data.s = NaN;
    proj_data.e = NaN;
    proj_data.u = NaN;
    proj_data.id = int8(0);
    proj_data.iw = int8(0);
    proj_data.xlo = NaN;
    proj_data.xhi = NaN;
    
    
    proj_data = class(proj_data,'ISISEXCproj_data');

    
case 1
    % if single argument of class ISISEXCproj_data, return it
    if isa(entry_name,'ISISEXCproj_data')
        proj_data = entry_name;
    else
        
        error('!! It is not a valid ISISEXCproj_data constructor ');
        return
    end
    

    % create object using specified arguments
    
case 9    
    
    check=1;
    
    proj_data.entry_name = entry_name;
    if(isa(varargin{1},'char')), proj_data.name = varargin{1}; else error('!! problems with name'), check=0; end
    if(isa(varargin{2},'double')), proj_data.s = varargin{2}; else error('!! problems with s'), check=0; end
    if(isa(varargin{3},'double')&&(length(varargin{2})==length(varargin{3}))), proj_data.e = varargin{3}; else error('!! problems with e'), check=0; end
    if(isa(varargin{4},'double')&&(length(varargin{2})==size(varargin{4},2))), proj_data.u = varargin{4}; else error('!! problems with u'), check=0; end    
    if(isa(varargin{5},'int8')&&(length(varargin{2})==length(varargin{5}))), proj_data.id = varargin{5}; else error('!! problems with id'), check=0; end    
    if(isa(varargin{6},'int8')&&(length(varargin{2})==length(varargin{6}))), proj_data.iw = varargin{6}; else error('!! problems with iw'), check=0; end    
    if(isa(varargin{7},'double')&&(length(varargin{2})==length(varargin{7}))), proj_data.xlo = varargin{7}; else error('!! problems with xlo'), check=0; end    
    if(isa(varargin{8},'double')&&(length(varargin{2})==length(varargin{8}))), proj_data.xhi = varargin{8}; else error('!! problems with xhi'), check=0; end    

    
    if (check==1), 
        proj_data = class(proj_data,'ISISEXCproj_data'); 
    else 
        error('!! It is not a valid ISISEXCproj_data constructor ');
        proj_data=[];
    end
    
    
otherwise
    error('!! It is not a valid ISISEXCproj_data constructor ');
end



