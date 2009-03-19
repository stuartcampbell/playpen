function dataset_2d = rebin_y(a,varargin)
%  rebin_y rebin y axis of a dataset 2d object for libisisexec 
%
%  new_dataset = rebin_y(dataset_2d,option) or 
%  new_dataset = rebin_y(dataset_2d,ylo,dy,yhi) or
%  new_dataset = rebin_y(dataset_2d, ylo, yhi),
%
%  inputs: option = xref or xdesc, ylo to yhi = range to rebin, dy =
%  intervals. inputs may be arrays (see below)
%
%  rebins an IXTdataset_2d object along the y axis according to the specific arguments
%--------------------------------------------------------------------------
%   rebin_y(w1,yref)      rebin w1 with the bin boundaries of yref (*** Note: reverse of Genie-2)
%
%   rebin_y(w1,ydesc)  xdesc is an array of boundaries and intervals. Linear or logarithmic
%  ------------------------ rebinning can be accommodated by conventionally specifying the rebin
%                           interval as positive or negative respectively:
%   e.g. rebin_y(w1,[2000,10,3000])  and
%        rebin_y(w1,2000,10,3000)
%
%   rebins from 2000 to 3000 in bins of 10
%
%   e.g. rebin_y(w1,[5,-0.01,3000])  and
%        rebin_y(w1,5,-0.01,3000)
%
%   rebins along y axis from 5 to 3000 with logarithmically spaced bins with
%                                 width equal to 0.01 the lower bin boundary 
% 
%  The conventions can be mixed on one line:
%   e.g. rebin_y(w1,[5,-0.01,1000,20,4000,50,20000])
%
%  Rebinning between two limits along x maintaining the existing bin boundaries between those limits
%  is achieved with
%
%   rebin_y(w1,[ylo,yhi])  retain only the data between YLO and YHI, otherwise maintaining the
%  ------------------------ existing bin boundaries. 
%
%  general form:
%   rebin_y(w1,[y_1,dy_1,y_2,dy_2,...,y_n,dy_n,y_n+1])  
%
%------------Arrays of dataset_2d and option--------------------
% if dataset_2d is an array, every member of the array will be rebinned
% according to option. 
%
% If option is an array the same size as the dataset,
% dataset(i) will be rebinned along y with option(i), this should be a column vector if given as xdesc 
% (i.e. [ylo1, dy1, yhi1; ylo2, dy2, yhi2]) 
% 
% if using separate arguments (i.e. new_dataset = rebin_y(old_dataset, ylo, dy, yhi) 
% then ylo, dy and yhi may be column or row vectors, 
% an array of yref may also be given as column or row vectors.
%
% e.g. : new_dataset = rebin_y(dataset_2d(1:2),[200,50,10000;100,30,5000]) or 
%        new_dataset = rebin_y(dataset_2d(1:2),[200,100],[50;30],[10000,5000])
%
% both rebin dataset_2d(1) along y between 200 and 10,000 with bins of 50 and
% dataset_2d(2) between 100 and 5000 with bins of 30
% 
%--------------------------------------------------------------------------
%-------------------

dataset_2d = a;
b = [];
len_arg = length(varargin);

%-------------------sort data into correct format (for arrays)----------
if nargin == 1
    dataset_2d = a;
    return
end

if len_arg == 1 
    if size(varargin{1},1) > 1 && ...
             isa(varargin{1},'IXTdataset_2d')
        b = varargin{1}';       % if array of objects, make sure it's column
    else
        b = varargin{1};        % else needs to be row (or is already column)
    end
else
    for i = 1:len_arg           % for many objects, arrange in columns
        try                     % if this fails, b will be incorrect size.
            if size(varargin{i},2) > 1
                b = [b, varargin{i}'];
            else
                b = [b, varargin{i}];   % is already column
            end
        catch
            warning('array dimension missmatch in xdesc arguments - rebin_y not performed')
            return
        end
    end
end

%----------------perform actions-----------------------

len_a = length(a);
len_b = size(b,1);

if  len_b == len_a          % both arrays or both single

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebin_y',IXTdataset_2d,a(i),b(i,:));
    end

elseif len_a > 1 && len_b == 1     % a is array, b isn't

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebin_y',IXTdataset_2d,a(i),b);
    end

      % errors
elseif  len_b ~= len_a      % if arrays are difference sizes
    warning('rebin_y not performed - array dimensions do not agree');
    return
else
    warning('rebin_y not performed - not enough data provided'); % something must be 0
    return
end