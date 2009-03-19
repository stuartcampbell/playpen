function set_ext(input1)
% mgenie style set_inst command for libisis. Sets the extension of the file that is
% being analysed. This is for easier file access. By using set_dir,
% set_inst and ass, there is no need to have objects on the workspace. 
% 
% set_ext(input1)
%
% inputs
%------------
%
% input1:       Char     The extension of the file 
%
% outputs
%------------
%
% none
%
% example: set_ext('RAWs')

if ~ischar(input1)
    error('extension must be a string')
else
    ixf_global_var('data_source','set','ext',input1);
end