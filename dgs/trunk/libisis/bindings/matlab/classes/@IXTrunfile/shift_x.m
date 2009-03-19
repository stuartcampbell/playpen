function runfile = shift_x(a,x_shift)
%--- Help for IXTrunfile/shift_x.m---
% call syntax: runfile = shift_x(a,x_shift)
%
% Shifts the xarray of the data in an IXTrunfile object to the right by the amount
% 'x_shift'
%
% inputs: a = IXTrunfile object, x_shift = amount to shift a data by 
%
% output: IXTrunfile object.. runfile = shift_x(a,x_shift)
res = IXTrunfile;
res(length(a)) = res(1);
runfile = libisisexc('IXTrunfile','shift_x',res,a,x_shift);