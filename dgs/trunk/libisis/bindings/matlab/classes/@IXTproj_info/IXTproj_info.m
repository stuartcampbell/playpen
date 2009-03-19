function proj_info = ISISEXCproj_info( entry_name, varargin )
% ! Create an ISISEXCproj_info object 
% ! REQUIRED INPUT PARAMETERS
% ! proj_info  = ISISEXCproj_info( [entry_name], [name],
%                                  [caption], [uindex], [u_string], [u], 
%                                  [ulength], [u2ortho]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCproj_info
% ! ================
% ! NeXus class: None
% !	Information about the projection axes (called viewing axes in present MSLICE) in which the data are expressed.
% !
% !
% !	entry_name			char		Name of entry in NeXus file
% !	name				char		Name
% !	caption(1:n_axes)	char		Captions for viewing axes
% !	uindex(:)			int			Indices of any projection axes that were given in terms of reciprocal lattice units (rlu).
% !								    Up to three such 'rlu projection axes' can occur e.g. if the axes were (1,1,0), (1,-1,0) and
% !								    (1,1,1). Entries must be set to zero if not used e.g. if only two projection axes were in rlu,
% !								    then uindex(3)=0. [It gives the 'positions' of the axes projected on to the Q space]
% ! u_string            cell        Cell array contains the characters that
%                                   represent the axes to be projected (here we do not include the axes projected on to Q space)
% !	u(3,:)				real		Rlu projection axes expressed in term of rlu: e.g.in the above (u(:,1)=(1,1,0), u(:,2)=(1,-1,0),
% !								    and u(:,3) = (1,1,1) [in terms of Q space]
% !!! ulength(3)		real		Length of rlu projection axes in inverse Angstroms (unused elements set = 0.0)
% !!! u2ortho(3,:)		real		Matrix to convert those coordinates of a point expressed in rlu projection axes to orthonormal
% !								    coordinates (needed to actually plot the data). (Elements in unused rows set to zero). The
% !								    orthonormal is defined as follows:
% !										if		u1(i), i=1->3 : first rlu projection axis, u2(i), i=1->3 second rlu projection axis ...
% !										then	x1(i), i=1->3 : first axis in orthonormal frame; is parallel to u1
% !												x2(i), i=1->3 : 2nd axis (if exists); in plane of u1 and u2
% !												x3(i), i=1->3 : 3rd axis (if exists); parallel to vector product u1 x u2
% !
% !									 The matrix u2ortho is defined as follows: if a vector r = a(i)*xi  = b(i)*ui
% !												a(i) = u2ortho(i,j)*b(j)
% ! Notes:
% ! ------
% !	The reason why ulength and u2ortho are needed is that they will represent some average over all the SPE files if the lattice
% !	parameters are not all equal (for example, if they changed slightly with changing temperature).
% !
% !	In fact, ulength is implicit in u2ortho, but it is convenient to have it held explicitly.
% !
% !	The number of projection axes does not need to be stored, but is convenient.
% !



switch nargin
    
    
    case 0
    % if no input arguments, create a default object
    proj_info.entry_name = 'none';
    proj_info.name = 'none';
    proj_info.n_axes = int32(0);
    proj_info.caption = 'none';
    proj_info.uindex = int32([0,0,0]);
    proj_info.u_string = cell(0);
    proj_info.u = [NaN];
    proj_info.ulength = [NaN];
    proj_info.u2ortho = [NaN];
    
    proj_info = class(proj_info,'ISISEXCproj_info');

    
case 1
    % if single argument of class ISISEXCproj_info, return it
    if isa(entry_name,'ISISEXCproj_info')
        proj_info = entry_name;
    else
        
        error('!! It is not a valid ISISEXCproj_info constructor ');
        return
    end
    

    % create object using specified arguments
    
case 9    
    
    check=1;
    
    proj_info.entry_name = entry_name;
    if(isa(varargin{1},'char')), proj_info.name = varargin{1}; else error('!! problems with name'), check=0; end
    if(isa(varargin{2},'int32')&&(length(varargin{2})==1)), proj_info.n_axes = varargin{2}; else error('!! problems with n_axes'), check=0; end
    if(isa(varargin{3},'char')&&(length(varargin{3})==varargin{2})), proj_info.caption = arargin{3}; else error('!! problems with caption'), check=0; end
    if(isa(varargin{4},'int32')), proj_info.uindex = varargin{4}; else error('!! problems with uindex'), check=0; end
    if(isa(varargin{5},'cell')), proj_info.u_string = varargin{5}; else error('!! problems with u_string'), check=0; end
    if(isa(varargin{6},'double')&([3,length(varargin{4})]==size(varargin{6}))), proj_info.u = varargin{6}; else error('!! problems with u'), check=0; end
    if(isa(varargin{7},'double')&&(length(varargin{7})==3)), proj_info.ulength = varargin{7}; else error('!! problems with ulength'), check=0; end
    if(isa(varargin{8},'double')&([3,length(varargin{4})]==size(varargin{8}))), proj_info.u2ortho = varargin{8}; else error('!! problems with u2ortho'), check=0; end
    
    if (check==1), 
        proj_info = class(proj_info,'ISISEXCproj_info'); 
    else 
        error('!! It is not a valid ISISEXCproj_info constructor ');
        proj_info=[];
    end
    
    
otherwise
    error('!! It is not a valid ISISEXCproj_info constructor ');
end


