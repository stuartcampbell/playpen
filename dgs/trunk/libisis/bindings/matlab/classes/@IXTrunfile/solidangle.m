function runfile_out = solidangle(runfile_in,dso,wbrf)
    runfile_out = libisisexc('IXTrunfile','solid',IXTrunfile,runfile_in,dso,wbrf);
