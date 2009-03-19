function data2d = get_monitor_datasets(runfile)
data2d = libisisexc('IXTrunfile','getmondata',IXTdataset_2d,runfile);