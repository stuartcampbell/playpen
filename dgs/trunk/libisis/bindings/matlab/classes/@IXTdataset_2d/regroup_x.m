function dataset_2d = regroup_x(a,varargin)
% regroup dataset 2d objects in libisis
%
% dataset_new = regroup_x(dataset_1d,params), or 
% dataset_new = regroup_x(dataset_1d,xlo,dx,xhi)
%
% regroups an IXTdataset_2d object according to parameters given
% where params=[xlo,dx,xhi] describe the binning paramaters to ensure that bins have minimum width 
% determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
% bin boundaries.
%
% input: either params = [xlo, dx, xhi] or as separate arguments xlo to xhi = range
% dx = minimum bin width, dataset_2d to regroup
%
% output: dataset_2d that has been regrouped
% 
% if given an array of dataset_2d, each will be regrouped using the params
% given.
%
% If params are given as arrays of the same size as dataset_1d, then 
% dataset_1d(i) will be regrouped with the parameters xlo(i), dx(i), xhi(i).
% 
% When given in the params array, xlo, dx and xhi should be column vectors,
% otherwise they can be either row or column vectors, as long as they are
% the same size as the dataset array.
%
dataset_2d = a;
b = [];
len_arg = length(varargin);

%-------------------sort data into correct format (for arrays)----------
if nargin == 1
    dataset_2d = a;
    return
end

if len_arg == 1 

    b = varargin{1};        % else needs to be row (or is already column)
       
else
    for i = 1:len_arg           % for many objects, arrange in columns
        
        try                     % if this fails, b will be incorrect size.
        
            if size(varargin{i},2) > 1
                b = [b, varargin{i}'];
            else
                b = [b, varargin{i}];   % is already column
            end

        catch
            
            warning('array dimension missmatch in xdesc arguments - rebin not performed')
            return

        end

    end

end

%----------------perform actions-----------------------

len_a = length(a);
len_b = size(b,1);

if  len_b == len_a          % both arrays or both single

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','regroup_x',IXTdataset_2d,a(i),b(i,:));
    end

elseif len_a > 1 && len_b == 1     % a is array, b isn't

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','regroup_x',IXTdataset_2d,a(i),b);
    end

      % errors
elseif  len_b ~= len_a      % if arrays are difference sizes

    warning('rebin_x not performed - array dimensions do not agree');
    return

else

    warning('rebin_x not performed - not enough data provided'); % something must be 0
    return

end