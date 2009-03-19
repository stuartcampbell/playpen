function size_arr=parse_array_size(varargin)
% Gets the array that gives the size of an array from the input argument(s)
% following the syntax of built-in Matlab functions e.g. rand
%
%   >> size_arr = array_size(n)             % size_arr=[n,n]
%   >> size_arr = array_size(n,m)           % size_arr=[n,m]
%   >> size_arr = array_size(n,m,p...)      % size_arr=[n,m,p]
%   >> size_arr = array_size([n,m,p...])    % size_arr=[n,m,p]
%

% T.G.Perring 21 Dec 2006

size_arr=[];
if all(cellfun('isclass',varargin,'double'))    % every element is a real
    if length(varargin)==1
        n=varargin{1};
        if length(n)==1
            size_arr=[n,n];
        elseif isvector(n) && size(n,1)==1
            size_arr=n;
        end
    elseif all(cellfun('prodofsize',varargin)==1)   % every element is a scalar
        size_arr=cell2mat(varargin);
    end
end
