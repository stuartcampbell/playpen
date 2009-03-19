function runfile = cosh(a)
%--- Help for IXTrunfile/cosh.m---
% call syntax: runfile = cosh(a)
%
% takes the hyperbolic cosine of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = cosh(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','cosh',res,a);