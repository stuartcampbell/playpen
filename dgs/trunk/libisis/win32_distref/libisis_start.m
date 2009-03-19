function libisis_start(root_dir)
% libisis_start(root_dir) , whereroot_dir is the root directory of libisis
%
% Deprecated function. Place the following in your startup.m or type at the command prompt:
%   >> addpath(root_dir)
%   >> libisis_init

try
    addpath(root_dir)
    libisis_init
catch
    error('Unable to initialise Libisis')
end
