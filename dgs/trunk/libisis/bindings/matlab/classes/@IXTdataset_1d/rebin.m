function dataset_1d = rebin(a,varargin)
%  rebin dataset 1d object for libisisexec, keywords: libisis, gtk 
%
%  new_dataset = rebin(dataset_1d,option) or 
%  new_dataset = rebin(dataset_1d,xlo,dx,xhi) or
%  new_dataset = rebin(dataset_1d, xlo, xhi),
%
%  inputs: option= xref or xdesc, xlo to xhi = range to rebin, dx =
%  intervals. inputs may be arrays (see below)
%
%  rebins an IXTdataset_1d object according to the specific arguments
%--------------------------------------------------------------------------
%   rebin(w1,xref)      rebin w1 with the bin boundaries of xref (*** Note: reverse of Genie-2)
%
%   rebin(w1,xdesc)  xdesc is an array of boundaries and intervals. Linear or logarithmic
%  ------------------------ rebinning can be accommodated by conventionally specifying the rebin
%                           interval as positive or negative respectively:
%   e.g. rebin(w1,[2000,10,3000])  and
%        rebin(w1,2000,10,3000)
%
%   rebins from 2000 to 3000 in bins of 10
%
%   e.g. rebin(w1,[5,-0.01,3000])  and
%        rebin(w1,5,-0.01,3000)
%
%   rebins from 5 to 3000 with logarithmically spaced bins with
%                                 width equal to 0.01 the lower bin boundary 
% 
%  The conventions can be mixed on one line:
%   e.g. rebin(w1,[5,-0.01,1000,20,4000,50,20000])
%
%  Rebinning between two limits maintaining the existing bin boundaries between those limits
%  is achieved with
%
%   rebin(w1,[xlo,xhi])  retain only the data between XLO and XHI, otherwise maintaining the
%  ------------------------ existing bin boundaries. 
%
%  general form:
%   rebin(w1,[x_1,dx_1,x_2,dx_2,...,x_n,dx_n,x_n+1])  
%
%------------Arrays of dataset_1d and option--------------------
% if dataset_1d is an array, every member of the array will be rebinned
% according to option. 
%
% If option is an array the same size as the dataset,
% dataset(i) will be rebinned with option(i), this should be a column vector if given as xdesc 
% (i.e. [xlo1, dx1, xhi1; xlo2, dx2, xhi2]) 
% 
% if using separate arguments (i.e. new_dataset = rebin(old_dataset, xlo, dx, xhi) 
% then xlo, dx and xhi may be column or row vectors, 
% an array of xref may also be given as column or row vectors.
%
% e.g. : new_dataset = rebin(dataset_1d(1:2),[200,50,10000;100,30,5000]) or 
%        new_dataset = rebin(dataset_1d(1:2),[200,100],[50;30],[10000,5000])
%
% both rebin dataset_1d(1) between 200 and 10,000 with bins of 50 and
% dataset_1d(2) between 100 and 5000 with bins of 30
% 
%---------------------------------------------------------------------------------------------

dataset_1d = IXTdataset_1d;
b = [];
len_arg = length(varargin);

%-------------------sort data into correct format (for arrays)----------
if nargin == 1
    dataset_1d = a;
    return
end

if len_arg == 1 
    if size(varargin{1},1) > 1 && ...
             isa(varargin{1},'IXTdataset_1d')
        b = varargin{1}';       % if array of objects, make sure it's column
    else
        b = varargin{1};        % else needs to be row (or is already column)
    end
else
    for i = 1:len_arg           % for many objects, arrange in columns
        try         % if this fails, b will be incorrect size.
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
        dataset_1d(i) = libisisexc('IXTdataset_1d','rebin',IXTdataset_1d,a(i),b(i,:));
    end

elseif len_a > 1 && len_b == 1     % a is array, b isn't

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','rebin',IXTdataset_1d,a(i),b);
    end

      % errors
elseif  len_b ~= len_a      % if arrays are difference sizes
    warning('rebin not performed - array dimensions do not agree');
    return
else
    warning('rebin not performed - not enough data provided'); % something must be 0
    return
end
