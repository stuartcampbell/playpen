function runfile_out = rebin_x_mon(runfile_in,rebin_parameter)
%  rebin_x_mon(runfile,option)
%  option=xref or xdesc
%
%  rebins the x-dimension of the monitor data contained in an IXTrunfile object according to the specific arguments
%--------------------------------------------------------------------------
%   rebin_x_mon(w1,xref)      rebin w1 with the bin boundaries of xref (*** Note: reverse of Genie-2)
%
%   rebin_x_mon(w1,xdesc)  xdesc is an array of boundaries and intervals. Linear or logarithmic
%  ------------------------ rebinning can be accommodated by conventionally specifying the rebin
%                           interval as positive or negative respectively:
%   e.g. rebin_x_mon(w1,[2000,10,3000])  rebins from 2000 to 3000 in bins of 10
%
%   e.g. rebin_x_mon(w1,[5,-0.01,3000])  rebins from 5 to 3000 with logarithmically spaced bins with
%                                 width equal to 0.01 the lower bin boundary 
%  The conventions can be mixed on one line:
%   e.g. rebin_x_mon(w1,[5,-0.01,1000,20,4000,50,20000])
%
%  Rebinning between two limits maintaining the existing bin boundaries between those limits
%  is achieved with
%
%   rebin_x_mon(w1,[xlo,xhi])  retain only the data between XLO and XHI, otherwise maintaining the
%  ------------------------ existing bin boundaries. 
%
%  general form:
%   rebin_x_mon(w1,[x_1,dx_1,x_2,dx_2,...,x_n,dx_n,x_n+1])  
%---------------------------------------------------------------------------------------------
runfile_in=create_rebin_command(runfile_in,inputname(1),inputname(2),rebin_parameter,mfilename('fullpath'),mfilename);
runfile_out= libisisexc('IXTrunfile','rebin_x_mon',IXTrunfile,runfile_in,rebin_parameter);

