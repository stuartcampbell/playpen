function dataset_1d = regroup(a,varargin)
% regroup dataset 1d objects
%
% dataset_new = regroup(dataset_1d,params), or 
% dataset_new = regroup(dataset_1d,xlo,dx,xhi)
%
% regroups an IXTdataset_1d object according to parameters given
% where params=[xlo,dx,xhi] describe the binning parameters to ensure that bins have minimum width 
% determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
% bin boundaries.
%
% input: either params = [xlo, dx, xhi] or as separate arguments xlo to xhi = range
% dx = minimum bin width, dataset_1d to regroup
%
% output: dataset_1d that has been regrouped
% 
% if given an array of dataset_1d, each will be regrouped using the params
% given, if params are an array within a single argument params, then
% xlo, dx and xhi should be column vectors, if given as separate arguments
% then either row or column vectors can be given. 
%
% If params are given as arrays of the same size as dataset_1d, then 
% dataset_1d(i) will be regrouped with the parameters xlo(i), dx(i), xhi(i).
% 
% When given in the params array, xlo, dx and xhi should be column vectors,
% otherwise they can be either row or column vectors, as long as they are
% the same size as the dataset array.

dataset_1d = IXTdataset_1d;
b = [];
len_arg = length(varargin);

if nargin == 1
    dataset_1d = a;
    return;
end

if len_arg == 1 
    b = varargin{1};
else
    for i = 1:len_arg
        try
            if size(varargin{i},2) > 1
                b = [b, varargin{i}'];
            else
                b = [b, varargin{i}];
            end
        catch
            warning('regroup not performed - array dimension mismatch in input arguments')
            return
        end            
    end
end
    
len_a = length(a);
len_b = size(b,1);

if len_b == len_a       % either both array or both single
    
    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','regroup',IXTdataset_1d,a(i),b(i,:));
    end
    
elseif len_a > 1 && len_b == 1  % only a is array
    
    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','regroup',IXTdataset_1d,a(i),b);
    end
    % errors
elseif len_b ~= len_a
    warning('regroup not performed - array dimensions do not agree');
    return
else
    warning('regroup not performed - not enough data provided');
    return
end



