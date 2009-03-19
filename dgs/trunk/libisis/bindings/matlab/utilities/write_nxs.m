function write_nxs(filename,object,entry)
% Write libisis object to nNeXus file
%
%   >> write_nxs (filename, object_in, entryname)
%
%   filename    File name
%   object_in   Object to be written to the file
%   entryname   Optionally writes entry name to top level directory
%              of object in nexus file.
%               The default entry name will be 'object' if object_in
%              is of type IXTobject
%                e.g. if type is IXTrunfile, the entry will be 'runfile'

if ischar(filename) && isobject(object)
    disp(['Creating file: ',filename]);
    fio=open(IXTfileio,translate_write(filename),4);
    if nargin > 2 && ischar(entry)
        write(object,fio,entry);
        fio=close(fio);
    else
        write(object,fio,'');
        fio=close(fio);
    end
else
    disp('invalid filename or object input');
    return
end
