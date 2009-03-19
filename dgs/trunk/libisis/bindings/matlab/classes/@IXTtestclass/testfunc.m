% Method of IXTtestclass to allow testing of a FORTRAN function 
%
% Usage:      
%
%      [out1,out2,...] = testfunc(IXTtestclass, arg1, arg2, ...)
%
% Will invoke the subroutine  "IXBtestfunc_testclass" 
% defined in the file  "bindings/matlab/IXMtestclass_m.f90"
%
% See the FORTRAN source file for instructions on modifying this function
% so that it calls your own FORTRAN modules with the correct arguments
%
function varargout = testfunc(r, varargin)
varargout = libisisexc('IXTtestclass', 'testfunc_varargin', varargin);
