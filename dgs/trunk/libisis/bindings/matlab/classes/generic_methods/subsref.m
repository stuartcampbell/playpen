% Implements
%                  val = var.field
% by calling
%                  val = get(var, field)
%
% This is a generic method for IXT classes ($Revision: 605 $)
% Only edit the master version in "matlab/classes/generic_methods"
%
function r = subsref(r, s)
for i = 1:length(s) 
  switch s(i).type
    case '.'  % self.field syntax
       r = get(r, s(i).subs);
    case '()'  % self() syntax
       r = r(s(i).subs{:});
    case '{}' % cell addressing
       r = r(s(i).subs{:});
   end
end