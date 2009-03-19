function gtk_init()
%---------------------------------
%General Toolkit functions paths
%---------------------------------

%pwd 
IXG_MATLAB_ROOT = matlabroot;
if (isempty(IXG_MATLAB_ROOT))
    IXG_MATLAB_ROOT = [];
    error('Cannot find matlab root, Exiting');
end

%init file
init_file = 'gtk_init';
find_init = feval('which',init_file);
if (isempty(find_init))
    IXG_MATLAB_ROOT=[];
    error('Graphical toolkit init file not found, Exiting');    
else
    ind = strfind(find_init,init_file);
    path_string = find_init(1:ind-1);
end

% path_string = [IXG_MATLAB_ROOT '\work\'];
ind =isdir(path_string);
%error
if (isempty(ind))
    error('Cannot find work directory');
end

% make a global variable for GTK path
% evalin('base','global IXG_GTK_INSTALLATION_PATH');

IXG_GTK_INSTALLATION_PATH = path_string;

%add lower level functions

IXG_LLF_STRING = [path_string 'lowlevelfunctions'];
gtk_low_level_functions = genpath(IXG_LLF_STRING);
addpath (gtk_low_level_functions);

%add user functions

IXG_UF_STRING = [path_string 'userfunctions'];
gtk_user_functions = genpath(IXG_UF_STRING);
addpath (gtk_user_functions);

%add interface

IXG_GI_STRING = [path_string 'generalinterface'];
gtk_general_interface = genpath(IXG_GI_STRING);
addpath (gtk_general_interface);

%add std user function

IXG_SUF_STRING = [path_string 'stduserfunctions'];
gtk_std_user_functions = genpath(IXG_SUF_STRING);
addpath (gtk_std_user_functions);

IXG_SOM_STRING = [path_string 'sliceomatic_DJWmod'];
gtk_sliceomatic = genpath(IXG_SOM_STRING);
addpath(gtk_sliceomatic);

%add images

IXG_IMG_STRING = [path_string 'images'];
gtk_images = genpath(IXG_IMG_STRING);
addpath (gtk_images);

ixf_global_var('libisis_graphics','set','IXG_MATLAB_ROOT',IXG_MATLAB_ROOT);
ixf_global_var('libisis_graphics','set','IXG_IMG_STRING',IXG_IMG_STRING);
ixf_global_var('libisis_graphics','set','IXG_SUF_STRING',IXG_SUF_STRING);
ixf_global_var('libisis_graphics','set','IXG_GI_STRING',IXG_GI_STRING);
ixf_global_var('libisis_graphics','set','IXG_UF_STRING',IXG_UF_STRING);
ixf_global_var('libisis_graphics','set','IXG_GTK_INSTALLATION_PATH',IXG_GTK_INSTALLATION_PATH);
ixf_global_var('libisis_graphics','set','IXG_LLF_STRING',IXG_LLF_STRING);
ixf_global_var('libisis_graphics','set','IXG_SOM_STRING',IXG_SOM_STRING);
%run global script
ixf_globaldefault;

%display message
% disp('%-------------------------------------------------------------------------')
% disp('Graphical Toolkit Started Successfully')
% disp('For graphical toolkit function index use gtkhelp command');
% disp('-   Example: gtkhelp');
% disp('Any comments or requirements should be mailed to
% D.Whittaker@rl.ac.uk');
% disp('%--------------------------------------------------------------------------')

