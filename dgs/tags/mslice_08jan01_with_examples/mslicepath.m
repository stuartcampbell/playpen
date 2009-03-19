function mslicepath(p)

% === if no path given select current directory
if ~exist('p','var'),
    p=[pwd filesep];
    disp(sprintf('Use currect directory to update default mslice paths :\n %s',p));
end

pp=deblank(p); % remove trailing blancs from end 
pp=fliplr(deblank(fliplr(pp))); % ... and beginning

% === if not a valid directory name given return
if ~exist(pp,'dir'),
    disp(sprintf('Invalid directiory:\n %s. Command not performed.',pp));
    return;
end

% === if a valid directory name ensure it ends in '\'
if exist('pp','dir')|~(strcmp(pp(end),'\')|strcmp(pp(end),'/'))
    pp=[pp filesep];
end

disp(sprintf('Updating path in default .msp files to: \n %s',pp));
updatemsp(pwd,'xye','c:\mprogs\mslice\',pp);
updatemsp([pwd filesep 'HET'],'xye','c:\mprogs\mslice\',pp);
updatemsp([pwd filesep 'IRIS'],'xye','c:\mprogs\mslice\',pp);
updatemsp([pwd filesep 'MARI'],'xye','c:\mprogs\mslice\',pp);
updatemsp([pwd filesep 'MAPS'],'xye','c:\mprogs\mslice\',pp);