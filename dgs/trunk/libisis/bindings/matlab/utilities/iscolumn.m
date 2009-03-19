function yes = iscolumn(v)
%  ISCOLUMN True if array is a column vector.
%     ISCOLUMN(V) returns logical true (1) if V is a n x 1 vector,
%     where n >= 0
yes = length(size(v))==2 & size(v,2)==1;