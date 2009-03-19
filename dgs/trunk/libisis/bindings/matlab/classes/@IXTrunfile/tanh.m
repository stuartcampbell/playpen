function runfile = tanh(a)
%--- Help for IXTrunfile/tanh.m---
% call syntax: runfile = tanh(a)
%
% takes the hyperbolic tangent of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = tanh(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','tanh',res,a);