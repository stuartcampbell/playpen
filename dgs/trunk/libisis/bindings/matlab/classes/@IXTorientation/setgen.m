function r = setgen(varargin)
%%function r = setgen(self,v)

%Add in "easy" varargin support from matlab. If you write
%    result = libisisexc("myclass","myfunc_varargin",result_variable, varargin)
%It is equivalent to you writing
%    result = libisisexc("myclass","myfunc",result_variable, varargin(1), varargin(2), ... , varargin(nargin) )

r = libisisexc('IXTorientation','setgen_varargin',IXTorientation,varargin);

%%r = libisisexc('IXTorientation','setgen',IXTorientation,self,v);