function gtk_off()
%---------------------------------
% Turns off the graphics package
%---------------------------------

ixf_global_var('libisis_graphics','clear')

%init file
init_file = 'gtk_init';
find_init = feval('which',init_file);
ind = strfind(find_init,init_file);
path_string = find_init(1:ind-1);
ind =isdir(path_string);
%error

% make a global variable for GTK path
% evalin('base','global IXG_GTK_INSTALLATION_PATH');

IXG_GTK_INSTALLATION_PATH = path_string;

%add lower level functions

IXG_LLF_STRING = [path_string 'lowlevelfunctions'];
gtk_low_level_functions = genpath(IXG_LLF_STRING);
rmpath (gtk_low_level_functions);

%add user functions

IXG_UF_STRING = [path_string 'userfunctions'];
gtk_user_functions = genpath(IXG_UF_STRING);
rmpath (gtk_user_functions);

%add interface

IXG_GI_STRING = [path_string 'generalinterface'];
gtk_general_interface = genpath(IXG_GI_STRING);
rmpath (gtk_general_interface);

%add std user function

IXG_SUF_STRING = [path_string 'stduserfunctions'];
gtk_std_user_functions = genpath(IXG_SUF_STRING);
rmpath (gtk_std_user_functions);

%add images

IXG_IMG_STRING = [path_string 'images'];
gtk_images = genpath(IXG_IMG_STRING);
rmpath (gtk_images);
