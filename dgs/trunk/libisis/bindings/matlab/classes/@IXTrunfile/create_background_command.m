function rf=create_background_command(rf,arg1,bmin,bmax,progname_full,progname)
rf.program_name=add_item(rf.program_name,progname_full);
command=progname;
command=strcat(command, ' (',arg1,',');

Nval1=num2str(bmin);
Nval2=num2str(bmax);
command=strcat(command,Nval1,',',Nval2,')');

rf.command_line=add_item(rf.command_line,command);

