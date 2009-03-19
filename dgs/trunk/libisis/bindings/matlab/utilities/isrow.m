function yes = isrow(v)
%  ISROW True if array is a row vector.
%     ISROW(V) returns logical true (1) if V is a 1 x n vector,
%     where n >= 0
yes = length(size(v))==2 & size(v,1)==1;