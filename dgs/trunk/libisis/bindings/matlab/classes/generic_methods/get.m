%    val = get(var, 'field')  % returns field
%    val = get(var)           % returns structure of object contents 
%
% This is a generic method for IXT classes ($Revision: 254 $)
% Only edit the master version in "matlab/classes/generic_methods"
%
function r = get(self,field)
if (nargin == 1)
    r = struct(self);
    return
end
if (~ischar(field))
    error('GET: field name must be a character string')
end
r = self.(field);