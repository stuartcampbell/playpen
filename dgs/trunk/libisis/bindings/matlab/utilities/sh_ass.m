function out = sh_ass()
% mgenie style sh_ass show assignment function for libisis
%
% >> output = sh_ass()
%
% shows the currently assigned rawfile path, instrument and number, also
% returns an IXTraw_file object of the currently assigned rawfile if
% possible.
%
% Inputs:
%-------------
% none
% 
% Outputs:
% ------------
%
% output:       struct          .path -         currently assigned path
%                               .inst -         currently assigned instrument 
%                               .ext -          currently assigned file extension
%                               .rawfile_no -   currently assigned raw file
%                                               number
%                               .rawfile -      IXTraw_file object
%                                               containing the assigned
%                                               rawfile if possible.

[output.path, output.inst, output.ext, output.rawfile_no, output.rawfile] = ixf_global_var('data_source','get','path','inst','ext','rawfile_no','rawfile');
display(' ')
display('Currently assigned rawfile information')
display('--------------------------------------')
display(' ')
display(['path:   ' output.path])
display(['instrument:  ' output.inst])
display(['extension:  ' output.ext])
display(['rawfile number:  ' output.rawfile_no])

if nargout > 0
    out = output;
end