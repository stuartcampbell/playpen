function dataset_nd = ISISEXCdataset_nd( entry_name, varargin )
%ISISEXCdataset_nd Create a ISISEXCdataset_nd object 

% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCdataset_nd
% ! ==============
% ! NeXus class: NXdata
% !
% !	Contains 1  dimensional datasets that should be sufficient for present purposes, but can be extended
% ! to higher dimensions trivially.
% !
% !
% !---------------------------------------------------------
% 	type ISISEXCdataset_nd
% ! title:
% 		character(len=long_len) title
% ! signal:
% 		real(i4b), allocatable :: nx(:)			! size(nx) = no. dimensions
% 		real(dp), allocatable :: s(:), e(:)		! size(s)=size(e) = nx(1)*nx(2)*nx(3) ... nx(size(nx)), and can reshape in any graphics plotting package
% 		character(len=long_len) s_label
% 
% ! x-axes: the length of the arrays will be be size(nx) i.e. no. of dimensions
% 		type (alloc_array), allocatable :: x(:)
% 		character(len=long_len), allocatable ::  x_label(:)
% 		character(len=short_len), allocatable :: x_units(:)
% 		logical, allocatable :: x_distribution(:), x_histogram(:)
% 





switch nargin
case 0
    % if no input arguments, create a default object
    
    dataset_nd.base = IXTbase;
    dataset_nd.title = 'none';
    dataset_nd.nx = NaN;
    dataset_nd.s = NaN;
    dataset_nd.e = NaN;
    dataset_nd.s_label = 'none';
    dataset_nd.x=NaN;
    dataset_nd.x_label = 'none';
    dataset_nd.x_units = 'none';
    dataset_nd.x_distribution = logical(0);
    dataset_nd.x_histogram = logical(0);

    dataset_nd = class(dataset_nd,'ISISEXCdataset_nd');
    
case 1
    % if single argument of class ISISEXCdataset_nd, return it
    if isa(entry_name,'ISISEXCdataset_nd')
        dataset_nd = entry_name;
    else
        disp([entry_name, '  is not an ISISEXCdataset_nd object.']);
        return
    end
    

    % create object using specified arguments
    
case 12    
    
    check=1;
    
    dataset_nd.entry_name = entry_name;
    if(isa(varargin{1},'char')), dataset_nd.name = varargin{1}; else check =0; end    
    if(isa(varargin{2},'char')), dataset_nd.title = varargin{2}; else check =0; end
    if(isa(varargin{3},'double')), dataset_nd.nx = varargin{3}; else check =0; end
   
    if(isa(varargin{4},'double')), dataset_nd.s = varargin{4}; else check =0; end
    if(isa(varargin{5},'double')), dataset_nd.e = varargin{5}; else check =0; end
    if(isa(varargin{6},'char')), dataset_nd.s_label = varargin{6}; else check =0; end  
    
    if(isa(varargin{7},'double')), dataset_nd.x = varargin{7}; else check =0; end
    if(isa(varargin{8},'char')), dataset_nd.x_label = varargin{8}; else check =0; end    
    if(isa(varargin{9},'char')), dataset_nd.x_units = varargin{9}; else check =0; end    
    if(isa(varargin{10},'logical')), dataset_nd.x_distribution = varargin{10}; else check =0; end    
    if(isa(varargin{11},'logical')), dataset_nd.x_histogram = varargin{11}; else check =0; end  
 
  
    
    
    if (check==1), 
        dataset_nd = class(dataset_nd,'ISISEXCdataset_nd'); 
    else 
        disp('!! It is not a valid ISISEXCdataset_nd constructor');
        dataset_nd=[];
    end    
    
otherwise
    disp('!! It is not a valid ISISEXCdataset_nd constructor');
end


