function runfile = exp(a)
%--- Help for IXTrunfile/exp.m---
% call syntax: runfile = exp(a)
%
% takes the exponent of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = exp(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','exp',res,a);