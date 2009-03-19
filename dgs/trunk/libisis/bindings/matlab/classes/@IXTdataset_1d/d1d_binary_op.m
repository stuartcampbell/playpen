function dataset_1d = d1d_binary_op(a,b,binary_op)
%--- Help for IXTdataset_1d/d1dbinary_op.m---
% call syntax: c = d1d_binary_op(a,b,operation)
%
% Performs binary operations on IXTdataset_1d objects by each other 
% element-by-element or scalars
%
% inputs: 
%           a =         IXTdataset_1d object or scalar,
%           b =         IXtdataset_1d object or scalar
%           operation = Operation to be performed with a and b (string)
%
% output: 
%           IXTdataset_1d object.. dataset_1d = a/b
%
% example:
%           mydataset = d1d_binary_op(d1a, d1b, 'divide')
%
%           In this case mydataset will be d1a / d1b.

len_a = length(a);
len_b = length(b);


if iscell(b) && len_a == len_b           % need to use cell syntax if either is a cell.
        dataset_1d = a;
        for i = 1:length(a)                  % they must be equal length though
            dataset_1d(i) = operation_dataset(a(i),b{i},binary_op);
        end
        
    elseif iscell(a) && len_a == len_b
        dataset_1d = b;
        for i = 1:length(a)
            dataset_1d(i) = operation_dataset(a{i},b(i),binary_op);
        end
        
    else 
        dataset_1d = operation_dataset(a,b,binary_op);
end
    


%------------------Function deals with numbers and datasets----------------
function dataset_1d = operation_dataset(a,b,binary_op)

col_a = length(a);
col_b = length(b);

% if a single object but array of datasets, then copy the object to match
% the dataset array. 
if col_a == 1 && isa(b,'IXTdataset_1d')
    a(1:col_b) = a;
    col_a = col_b;
elseif col_b == 1 && isa(a,'IXTdataset_1d')
    b(1:col_a)=b;
    col_b = col_a;
end

if  col_a == col_b       % both are same length 
    
  dataset_1d = libisisexc('IXTdataset_1d',binary_op,IXTdataset_1d,a,b);

elseif col_b == 1 && isnumeric(a) || ...
        col_a == 1 && isnumeric(b)      % array of a, only 1 d1d 

    dataset_1d = libisisexc('IXTdataset_1d',['array_' binary_op],IXTdataset_1d,a,b);

else

    error('a and b have incompatable column dimensions')

end
