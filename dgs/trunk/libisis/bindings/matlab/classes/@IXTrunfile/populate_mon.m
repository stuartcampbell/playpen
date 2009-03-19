function runfile_out = populate_mon(runfile_in,dso,period,ei,m_axis,m_rebin,opt)

runfile_out = libisisexc('IXTrunfile','populate_mon',IXTrunfile,runfile_in,dso,period,ei,m_axis,m_rebin,opt);

