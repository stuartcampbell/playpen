function rf=create_population_command(rf,arg1,progname_full,progname,par,argout,present)
% arg1 will always be a dso for a population command
rf.program_name=add_item(rf.program_name,progname_full);
command=progname;
val=struct2cell(argout);
pres=struct2cell(present);
names=fieldnames(present);
command=strcat(command, ' (',arg1,',');
for i=1:length(par)
    %%% need to parse par to string from num..
    if iscellnum(par{i})
        for j=1:size(par{i},2)
            command=strcat(command,'[',num2str(int32(par{i}{j})),'],');
        end
    end
    if iscellstr(par{i})
        for j=1:size(par{i},2)
            command=strcat(command,'''',int32(par{i}{j}),''',');
        end
    end
    if isnumeric(par{i}) %array[1 or N]
        for j=1:size(par{i},2)
            command=strcat(command,'[',num2str(int32(par{i}(j))),'],');
        end
    end
    if ischar(par{i})
        command=strcat(command,'''',par{i},'''',',');
    end
end
first=false;
for i=1:length(pres)
    if(pres{i})
        if(first)
            command=strcat(command,',','''',names{i},'''');
        else
            command=strcat(command,'''',names{i},'''');
        end
        if(isnumeric(val{i}))
            Nval=num2str(val{i});
            command=strcat(command,',[',Nval,']');
        elseif(isa(val{i},'IXTrunfile'))
            command=strcat(command,', IXTrunfile');
        elseif(isa(val{i},'IXTmask'))
            command=strcat(command,', IXTmask');
        elseif(isa(val{i},'IXTmap'))
            command=strcat(command,', IXTmap');
        elseif(islogical(val{i}))
            if(val{i})
                Lval='true';
            else
                Lval='false';
            end
            command=strcat(command,',',Lval);
        else
            command=strcat(command,',','''',val{i},'''');
        end
        first=true;
    end
end
command=strcat(command,')');
rf.command_line=add_item(rf.command_line,command);

  