function rf=create_remap_command(rf,arg1,arg2,progname_full,progname)
rf.program_name=add_item(rf.program_name,progname_full);
command=progname;
command=strcat(command, ' (',arg1,',',arg2,')');
rf.command_line=add_item(rf.command_line,command);

