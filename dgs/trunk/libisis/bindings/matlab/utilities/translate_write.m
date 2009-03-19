function elaborated_path=translate_write(path_in)
% elaborated_path=translate_read('path_in')
% function takes a global path prefix eg inst_path:::
% and turns it into a real path for writing to eg c:\data
% provided that path prefix has been defined
if ischar(path_in)
    elaborated_path=libisisexc('IXTutility','translate_write',path_in);
else
    disp('bad input to translate_read')
end
