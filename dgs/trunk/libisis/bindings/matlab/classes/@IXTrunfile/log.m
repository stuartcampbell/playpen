function runfile = log(a)
%--- Help for IXTrunfile/log.m---
% call syntax: runfile = log(a)
%
% takes the logarithm of a IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = log(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','log',res,a);