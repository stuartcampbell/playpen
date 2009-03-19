function data2d = get_detector_datasets(runfile)
data2d = libisisexc('IXTrunfile','getdetdata',IXTdataset_2d,runfile);