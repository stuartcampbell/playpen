function runfile = sin(a)
%--- Help for IXTrunfile/sin.m---
% call syntax: runfile = sin(a)
%
% Takes the sine of all data in an IXTrunfile object
%
% inputs: a= 1XTrunfile object 
%
% output: 1XTrunfile object... sin(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','sin',res,a);