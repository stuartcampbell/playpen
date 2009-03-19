function [dso,map_made]=map_prep(dso,map_type,map_file,arg)
map_made=0;
[path,found]=findpath(dso,map_type);
if(isa(arg,'IXTmap'))
    %    this will implicitly create a nexus file on the fly in the working
    %    directory
    fio=open(IXTfileio,map_file,4);
    write(arg,fio,'map');
    fio=close(fio);
    if(found)
        disp('INFO: map file entry in IXTdata_source object being over-ridden by supplied parameters')
        dso=replace_item(dso,map_file,map_type);
    else
        dso=add_item(dso,map_file,map_type);
    end
    map_made=1;
else
    if(ischar(arg))
        if(found)
            disp('INFO: map file entry in IXTdata_source object being over-ridden by supplied parameters')
            dso=replace_item(dso,arg,map_type);
        else
            dso=add_item(dso,arg,map_type);
        end
    else
        error('ERROR:  map path must be a character string')
    end
end


