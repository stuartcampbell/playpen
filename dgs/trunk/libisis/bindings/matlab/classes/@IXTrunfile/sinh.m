function runfile = sinh(a)
%--- Help for IXTrunfile/cos.m---
% call syntax: runfile = sinh(a)
%
% takes the hyperbolic sine of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = sinh(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','sinh',res,a);