function [varargout]=ixf_combine_data(w, format_oid, plot_format, length_x, length_y)
%-----------help for GTK combine_data function-----------------------------
% takes a dataset1d or dataset2d array and combines all the elements into
% single x,y,z arrays, with length_x number of x values and length_y number
% of y values.
%
% inputs: an array of dataset objects, type oid, type, length of x, length
% of y
%
% outputs: x,y,z single column vectors with all data for x,y,z combined
%
% call syntax: [x y z] =
% ixf_combine_data(w,type_oid,plot_type,length_x,length_y)
%
% Example: [x y z] = ixf_combine_data(Dataset2d,'type','surface',500,500)
%--------------updated 22/08/2006, Dean Whittaker--------------------------

validate_inputs(w, format_oid, plot_format)

if isa(w,'IXTdataset_1d') || isa(w,'IXTdataset_2d')
    
    x=[];
    y=[];
    z=[];
    
    if ~exist('length_x','var')
        length_x=length(w(1).x);
    end

    if ~exist('length_y','var')
        try
            length_y=length(w(1).y);    % may not be y data (i.e. dataset1d)
        end
    end
    
end

if isa(w,'IXTdataset_2d')
    for i=1:length(w)       
        [x2 y2 z2]=ixf_compute_values_twod(w(i).x, w(i).y, w(i).signal, format_oid, plot_format);

        %--------------- Data rearrangement---------------

        clear x3; clear y3; clear z3;
        x3(1,:)=x2(:);  % linearise
        x=[x, x3];      % add to array
        y3(1,:)=y2(:);
        y=[y, y3];
        z3(1,:)=z2(:);
        z=[z, z3];        
        
       %--------------------------------------------------
    
    end
    
    %-------------------memory management--------------------
    
    clear x3; clear y3; clear z3; clear x2; clear y2; clear z2; clear w; clear i; 
    clear type_oid;
    
    %--------------------------------------------------------
    
    [x i] = sort(x);
    y=y(i);
    z=z(i);                 % sort the data, and interpolate it in case it's not uniform
    [varargout{1} varargout{2}]=meshgrid(linspace(min(x),max(x),length_x),linspace(min(y),max(y),length_y));
    warning off 
    varargout{3}=griddata(x,y,z,varargout{1},varargout{2});
    warning on 
    
elseif isa(w,'IXTdataset_1d')
   
    for i=1:length(w)       % loop goes from 1 to varargin
        [x2 y2]=ixf_compute_values(w.x, w.signal, '', plot_type);
        x(1,:)=[x, x2(:)];
        y(1,:)=[y, y2(:)];
    end
    [varargout{1} i] = sort(x);
    varargout{2}=y(i);

elseif isa(w,'IXTpolygons')
    
    % Assumes the object is properly made
    
    vertices = [];
    faces = [];
    signal = [];
    for i = 1:numel(w)
        if i > 1 % after first set of data, match dimensions for second set.
            facesize = size(faces,2);
            if facesize < size(w(i).faces,2)
                faces = [faces, NaN*ones(size(w(i).faces,2),size(faces,1))];
            elseif facesize > size(w.faces,2)
                w(i).faces = [w(i).faces,  NaN*ones(size(faces,2),size(w(i).faces,1))];
            end
        end
        vert_length = length(vertices,1);
        vertices = [vertices; w(i).vertices];
        
        w(i).faces = w(i).faces + vert_length;  %offset the indexes in faces so they match the new arrangement. 
        
        faces = [faces; w(i).faces];
        signal = [signal; w(i).signal];
            
    end
    
    varargout{1} = vertices;
    varargout{2} = faces;
    varargout{3} = signal;

end



%--------------------------------------------------------------------------
function validate_inputs(varargin)
% validate_inputs - checks the input data is of the correct type, length,
% etc.
% inputs: input arguments       outputs: none
%
% syntax flag=validate_inputes(varargin)
%
% updated: 22/08/2006, Dean Whittaker
%--------------------------------------------------------------------------
IXG_ST_ERROR = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR');

check_dataset = isa(varargin{1}, 'IXTdataset_1d') || ...
    isa(varargin{1}, 'IXTdataset_2d') || isa(varargin{1},'IXTpolygons'); 
                    % check that this is a dataset 1d or 2d
if ~check_dataset
    ixf_display_error(IXG_ST_ERROR.no_object);
end
%