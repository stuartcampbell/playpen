%    var = read(var, IXTfileio, path)
%
% This is a generic method for IXT classes ($Revision: 395 $)
% Only edit the master version in "matlab/classes/generic_methods"
%
function r = read(self,fio,path)
r = libisisexc(class(self), 'read', self, fio, path);
