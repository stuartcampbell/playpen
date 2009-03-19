function cut_tofndgs = ISISEXCcut_tofndgs( entry_name, varargin )
% ! Create a ISISEXCcut_tofndgs object 
% ! REQUIRED INPUT PARAMETERS
% ! proj_data  = ISISEXCproj_data( [entry_name], [title], [plot],
%                                  [viewing_axes], [viewing_axes_label], 
%                                  [cut_spec_min], [cut_spec_max], [cut_spec_xyz], 
%                                  [cut_spec_delta], [npix], [pix_ind], [pixels]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCcut_tofndgs
% ! ==============
%       entry_name              char
%       title                   char
%       plot                    ISISEXCdataset_nd
%       viewing_axes            double
%       viewing_axes_label      char
%       cut_spec_min            double
%       cut_spec_max            double
%       cut_spec_xyz            char
%       cut_spec_delta          double
%       npix                    int
%       pix_ind                 int
%       pixels                  ISISEXCproj_tofndgs
%   

switch nargin
case 0
    % if no input arguments, create a default object
    
    cut_tofndgs.entry_name = 'none';
    cut_tofndgs.title = 'none';
    cut_tofndgs.plot = ISISEXCdataset_nd;
    cut_tofndgs.viewing_axes = NaN;
    cut_tofndgs.viewing_axes_label = 'none';
    cut_tofndgs.cut_spec_min = NaN;
    cut_tofndgs.cut_spec_max = NaN;
    cut_tofndgs.cut_spec_xyz = 'none';
    cut_tofndgs.cut_spec_delta = NaN;
    cut_tofndgs.npix = int8(0); 
    cut_tofndgs.pix_ind = int8(0);
    cut_tofndgs.pixels = ISISEXCproj_tofndgs;
    cut_tofndgs = class(cut_tofndgs,'ISISEXCcut_tofndgs');
    
case 1
    % if single argument of class ISISEXCcut_tofndgs, return it
    if isa(entry_name,'ISISEXCcut_tofndgs')
        cut_tofndgs = entry_name;
    else
        disp([entry_name, '  is not an ISISEXCcut_tofndgs object.']);
        return
    end
    

    % create object using specified arguments
    
case 12    
    
    check=1;
    
    if((get_n_axes(varargin{11})) ~= (length(varargin{3}))), check=0;
    else    
        
        if(size(varargin{3},2) ~= size(varargin{3},1)), check=0;
        else
            cut_tofndgs.entry_name = entry_name;
            if(isa(varargin{1},'char')), cut_tofndgs.title = varargin{1}; else check =0; end    
            if(isa(varargin{2},'ISISEXCdataset_nd')), cut_tofndgs.plot = varargin{2}; else check =0; end
            if(isa(varargin{3},'double')), cut_tofndgs.viewing_axes = varargin{3}; else check =0; end
            if(isa(varargin{4},'char')&&(size(varargin{4},1)==size(varargin{3},2))), cut_tofndgs.viewing_axes_label = varargin{4}; else check =0; end
            if(isa(varargin{5},'double')&&(length(varargin{5})==size(varargin{3},2))), cut_tofndgs.cut_spec_min = varargin{5}; else check =0; end
            if(isa(varargin{6},'double')&&(length(varargin{6})==size(varargin{3},2))), cut_tofndgs.cut_spec_max = varargin{6}; else check =0; end    
            if(isa(varargin{7},'char')&&(size(varargin{7},1)==size(varargin{3},2))), cut_tofndgs.cut_spec_xyz = varargin{7}; else check =0; end   
            if(isa(varargin{8},'double')&&(length(varargin{8})==size(varargin{3},2))), cut_tofndgs.cut_spec_delta = varargin{8}; else check =0; end
            if(isa(varargin{9},'int8')), cut_tofndgs.npix = varargin{9}; else check =0; end
            if(isa(varargin{10},'int8')), cut_tofndgs.pix_ind = varargin{10}; else check =0; end
            if(isa(varargin{11},'ISISEXCproj_tofndgs')), cut_tofndgs.pixels = varargin{11}; else check =0; end    
    
  
    
        end
    end
    
    
    if (check==1), 
        cut_tofndgs = class(cut_tofndgs,'ISISEXCcut_tofndgs'); 
    else 
        disp('!! It is not a valid ISISEXCcut_tofndgs constructor');
        cut_tofndgs=[];
    end    
    
otherwise
    disp('!! It is not a valid ISISEXCcut_tofndgs constructor');
end



