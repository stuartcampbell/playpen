function runfile_out = rebunch_x_mon(runfile_in,nbins)
% new_runfile = rebunch_x_det(dataset_1d,nbins)
%
% rebunches the monitor data of an IXTrunfile object with nbins elements grouped together
%
% input: IXTrunfile - IXTrunfile object or array, nbins - number of bins
% to group together
%
% output: IXTrunfile object
runfile_out= libisisexc('IXTrunfile','rebunch_x_mon',IXTrunfile,runfile_in,nbins);

