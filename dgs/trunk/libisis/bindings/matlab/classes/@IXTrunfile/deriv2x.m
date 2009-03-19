function runfile = deriv2x(a)
%--- Help for IXTrunfile/deriv2x.m---
% call syntax: runfile = deriv2x(a)
%
% takes the second derivative of the data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = deriv2x(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','deriv2x',res,a);