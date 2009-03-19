function tf = iscellnum(s)
%ISCELLNUM True for cell array of numerics.
%   ISCELLNUM(S) returns 1 if S is a cell array of numerics and 0
%   otherwise.  A cell array of numerics is a cell array where 
%   every element is a numeric array.
%

if isa(s,'cell'),
  res = cellfun('isclass',s,'double');
  tf = all(res(:));
else
  tf = false;
end