function runfile = cos(a)
%--- Help for IXTdataset_1d/cos.m---
% call syntax: runfile = cos(a)
%
% takes the cosine of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = cos(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','cos',res,a);