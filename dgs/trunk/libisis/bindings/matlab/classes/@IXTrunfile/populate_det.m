function runfile_out = populate_det(runfile_in,dso,period,ei,nchunk,d_axis,d_rebin,bgrd,i_lim,opt)
runfile_out = libisisexc('IXTrunfile','populate_det',IXTrunfile,runfile_in,dso,period,ei,d_axis,d_rebin,bgrd,i_lim,opt,nchunk);

