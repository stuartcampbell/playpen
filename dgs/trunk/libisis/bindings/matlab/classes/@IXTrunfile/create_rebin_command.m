function rf=create_rebin_command(rf,arg1,arg2,rebin_parameter,progname_full,progname)
rf.program_name=add_item(rf.program_name,progname_full);
command=progname;
command=strcat(command, ' (',arg1,',');
if(isnumeric(rebin_parameter))
    Nval=num2str(rebin_parameter);
    command=strcat(command,'[',Nval,'])');
else
    %can only be a reference object
    command=strcat(command,arg2,')');
end
rf.command_line=add_item(rf.command_line,command);

