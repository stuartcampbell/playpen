function dataset_2d = d2d_binary_op(a,b,operation,direction)

%--- Help for IXTdataset_2d/d2d_binary_op.m---
% call syntax: d2d_binary_op(a,b,operation,direction)
%
% Performs binary operations on IXTdataset_2d objects by each other 
% element-by-element or scalars. See documentation for further information
%
% inputs: 
%           a =         IXTdataset_2d object or scalar
%           b =         IXTdataset_2d, IXTdataset_1d object or scalar
%           operation = The operation to be performed (string)
%           direction = The direction for operation to be performed in (x
%                       or y, only required if second input is IXTdataset_1d or 
%                       vector array) (string)
%
% output: 
%           1XTdataset_2d object
% 
% example:
%           mydataset = d2d_binary_op(d2a, d1b, 'divide','x')
%           This example will divide d2a by d2b in the x direction
%           (direction does not matter if given an IXTdataset_2d or matrix
%           input) 


len_a = length(a);
len_b = length(b);

if ~exist('direction','var') 
    direction = '';
end

if iscell(b) && len_a == len_b           % need to use cell syntax if either is a cell.
        dataset_2d = a;
        for i = 1:length(a)                  % they must be equal length though
            dataset_2d(i) = operate_dataset(a(i),b{i}, operation, direction);
        end
        
    elseif iscell(a) && len_a == len_b
        dataset_2d = b;
        for i = 1:length(a)
            dataset_2d(i) = operate_dataset(a{i},b(i), operation, direction);
        end
        
    else 
        dataset_2d = operate_dataset(a,b, operation, direction);
end
    



%--------function deals with numbers and datasets only------------

    function dataset_2d = operate_dataset(a, b, operation, direction)
    
len_a = length(a);
len_b = length(b);

% if single object and array of datasets, then expand the single object
% into an array of identical objects.
if len_a == 1 && isa(b,'IXTdataset_2d')
    a(1:len_b) = a;
    len_a = len_b;
elseif len_b == 1 && isa(a,'IXTdataset_2d')
    b(1:len_a) = b;
    len_b = len_a;
end

if len_a == len_b 
        
   if isa(b,'IXTdataset_1d')
       if isempty(direction) % need to specify a default direction for this op.
           direction = 'x';
       end
            dataset_2d = libisisexc('IXTdataset_2d',['dataset_1d_' direction '_' operation],IXTdataset_2d,a,b);
   else
       dataset_2d = libisisexc('IXTdataset_2d',operation,IXTdataset_2d,a,b);
   end
 
elseif  ((len_a == 1 && isnumeric(b)) || (len_b == 1 && isnumeric(a))) && ~isempty(direction)    % single dataset, matrix of numbers 

    dataset_2d = libisisexc('IXTdataset_2d',['array_' direction '_' operation],IXTdataset_2d,a,b);

elseif  ((len_a == 1 && isnumeric(b)) || (len_b == 1 && isnumeric(a))) && isempty(direction) 
            
    dataset_2d = libisisexc('IXTdataset_2d',['array_' operation],IXTdataset_2d,a,b);
            
elseif isa(b,'IXTdataset_1d')        % looking at array of d1d added to the d2d or single d1d added to array d2d

   len_d2d = length(a);
   len_d1d = length(b);
   
    for i = 1:len_d2d
        
        len_y = size(a(i).signal,2);
        
        if len_d1d>=len_y
            dataset_2d(i) = libisisexc('IXTdataset_2d',['arraydataset_1d_' operation],IXTdataset_2d,a(i),b(1:len_y));
            len_d1d = len_d1d - len_y;
            
            if len_d1d ~= 0
                b=b((len_y+1):end);
            end
            
        else 
            error('dataset_1d dimensions inconsistent with dataset_2d dimensions')
        end
        
    end    

else

    error('a and b have incompatable column dimensions')
    
end