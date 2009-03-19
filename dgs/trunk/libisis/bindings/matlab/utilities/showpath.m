function  showpath(varargin)
% showpath('name') or showpath()
% given no arguments this will display an exhaustive list of searchpaths
% which have been set, or given a name will display only that searchpath if
% it has been set.
% this global path can also be used in the matlab environment using the
% ixf_global_props function
if nargin == 1
libisisexc('IXTpath','showpath',varargin{1});
else
libisisexc('IXTpath','showpath');
end