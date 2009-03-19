function runfile_out = regroup_x(runfile_in,regroup_parameter)
%
% new_runfile = regroup_x(runfile,params)
%
% regroups an IXTrunfile object according to parameters given
% where params=[xlo,dx,xhi] describe the binning parameters to ensure that bins have minimum width 
% determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
% bin boundaries.
%
% input: either params = [xlo, dx, xhi]
% xlo to xhi = range, dx = minimum bin width, 
%
% output: IXTrunfile that has been regrouped
runfile_out= libisisexc('IXTrunfile','regroup_x',IXTrunfile,runfile_in,regroup_parameter);

