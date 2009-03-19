function dso=create_dso_from_par(dso,par)

% if there are run_numbers defined in the par cell array this function will 
% create entries in the dso with a proper path.
%we first check to see if parameters have been supplied by argument calls
% if there are two then we remove rawfile entry from the dso if it is there
% and add in the parameter information

% dso inspection for rawfiles
parlen=size(par,2);
%%%%%
% par values are now ONLY run numbers
if(parlen >=1)
    %    remove all rawfile references if they exist
    [respath,found]=findpath(dso,'rawfile');
    if(found)
        dso=del_item(dso,'rawfile');
        disp('INFO: rawfile entry in IXTdata_source object will be over-ridden by supplied parameters')
    end
    % call findpaths command
    [respath,found]=findpaths(dso,'rawfile_mult');
    if(found)
        for j=size(respath,1)
            dso=del_item(dso,'rawfile_mult');
        end
        disp('INFO: multiple rawfile entry in IXTdata_source object will be over-ridden by supplied parameters')
    end
    % create new path from parameter input
    [rpath,pathlen]=make_path_from_par(par);
%    add new path info to dso
    if(pathlen > 1)
        for i=1:pathlen
            dso=add_item(dso,rpath{i},'rawfile_mult');
        end
    else
        dso=add_item(dso,rpath{1},'rawfile');
    end
end

%  put new path info into dso 


% get rawpath info from dso so can send to independent functions like homer_getei
[rawpath,found]=findpath(dso,'rawfile');
if(~found)
    [rawpath,found]=findpaths(dso,'rawfile_mult');
end
%

if(~found)
    error('ERROR: no data file input present');    
end