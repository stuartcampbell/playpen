function runfile = tan(a)
%--- Help for IXTrunfile/cos.m---
% call syntax: runfile = tan(a)
%
% takes the tangent of all data in an IXTrunfile object
%
% inputs: a = IXTrunfile object 
%
% output: IXTrunfile object.. runfile = tan(a)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','tan',res,a);