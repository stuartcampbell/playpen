function set_inst(input1)
% mgenie style set_inst command for libisis. Sets the instrument that is
% being analysed. This is for easier file access. By using set_path,
% set_inst and ass, there is no need to have objects on the workspace,
% rawfile number and spectrum numbers can be changed quickly and easily.
% 
% set_dir(input1)
%
% inputs
%------------
%
% input1:       Char     The three letter abbrev. of the instrument name,
%                       i.e. MAR, MER, MAP, HET
%
% outputs
%------------
%
% none
%
% example: set_inst('map')

if ~ischar(input1)
    error('instrument must be a string')
else
    ixf_global_var('data_source','set','inst',input1);
end