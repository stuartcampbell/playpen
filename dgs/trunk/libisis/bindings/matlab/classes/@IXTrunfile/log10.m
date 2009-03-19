function runfile = log10(a)
%--- Help for IXTrunfile/log.m---
% call syntax: runfile = log10(a)
%
% takes the logarithm base(10) of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = log(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','log10',res,a);