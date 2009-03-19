% LIBISISEXC: call an external ISISEXC library method on matlab objects
%  
% Usage: result = libisisexc("class", "operation", object1, object2, ... )
%
% For example two objects of class "ixtestclass" might be added 
% with the call:
%
%    sum = libisisexc("ixtestclass", "plus", ixtestclass, obj1, obj2)
%
% Note that as it is only possible to create a structure and not a class in an
% external program the "plus" operation here requires an empty result object to
% be passed out as one of the argument - here created via the
% ixtestclass class default constructor. Operations that return structures will 
% not need to do this - refer to the documentation for each call.
%
% The external computation is carried out by a compiled function whose 
% name is "class_operation" (so "ixtestclass_plus" above). This function
% needs to have been included in the LIBISISEXC library or an error will be
% reported.
%
%%% special varargin usage %%%
%
% Appending "_varargin" to the operation name has special meaning. Writing
%
%    sum = libisisexc("ixtestclass", "plus_varargin", ixtestclass, varargin)
%
% is intercepted by the library and interpreted as if you had instead written
%
%    sum = libisisexc("ixtestclass", "plus", ixtestclass, 
%                                          varargin{1}, varargin{2}, ... )
%
% (i.e. varargin is unpacked into a standard looking argument list)
% 
% $Id: libisisexc.m 332 2004-09-19 10:30:20Z faa59 $