function [dso,mask_made]=mask_prep(dso,mask_type,mask_file,arg)
mask_made=0;
[path,found]=findpath(dso,mask_type);
if(isa(arg,'IXTmask'))
    %    this will implicitly create a nexus file on the fly in the working directory
    fio=open(IXTfileio,mask_file,4);
    write(arg,fio,'mask');
    fio=close(fio);
    if(found)
        disp('INFO: mask file entry in IXTdata_source object being over-ridden by supplied parameters')
        dso=replace_item(dso,mask_file,mask_type);
    else
        dso=add_item(dso,mask_file,mask_type);
    end
    mask_made=1;
else
    if(ischar(arg))
        if(found)
            disp('INFO: mask file entry in IXTdata_source object being over-ridden by supplied parameters')
            dso=replace_item(dso,arg,mask_type);
        else
            dso=add_item(dso,arg,mask_type);
        end
    else
        error('ERROR: detector mask path must be a character string')
    end
end

end

