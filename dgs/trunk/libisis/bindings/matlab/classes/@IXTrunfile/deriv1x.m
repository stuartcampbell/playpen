function runfile = deriv1x(a)
%--- Help for IXTrunfile/deriv1x.m---
% call syntax: runfile = deriv1x(a)
%
% takes the first derivative of the data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = deriv1x(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','deriv1x',res,a);