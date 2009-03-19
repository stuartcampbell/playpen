function obj_out=read_nxs(filename,object,entry)
%IXTobject = read_nxs('file.nxs',IXTobject,'entry')
%IXTobject_out = read_nxs('file.nxs',IXTobject,'entry')
% IXTobject_out is the object being populated from a file 'file.nxs'. it will be of type IXTobject
% 'entry' optionally reads an object with top level directory 'entry' in
% the nexus file, the default entry name to read will be 'object' if it is of type IXTobject
% ie. if it is of type IXTrunfile, the entry name will be 'runfile'
if ischar(filename) && isobject(object)
    disp(['Reading file: ',filename]);
    fio=open(IXTfileio,translate_read(filename),1);
    if nargin > 2 && ischar(entry)
        obj_out=read(object,fio,entry);
        fio=close(fio);
    else
        obj_out=read(object,fio,'');
        fio=close(fio);
    end
else
    disp('invalid filename or object input');
    return
end

