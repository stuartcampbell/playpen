function r = sumspec(self,spec_nums,xmin,xmax,period)
% dataset_1d=sumpspec(rawfile,spectrum_array,xmin,xmax,period)
%
% function to read in the integrals of raw counts of an array of defined spectra into an IXTdataset_1d object.
% spectra to be read in are defined by the spectrum array variable which can either
% be a consecutive list created implicitly by matlab -> [spec_1:spec_n], or 
% a list of specific spectra defined by the user -> [spec_1 spec_2 spec_3 ... spec_n].
% xmin and xmax define the integration limits in microseconds
% The period argument is single-valued, and defines the period to extract
% spectra from, the default is 1. If it is specified to be zero then spectra spanning periods 
% can be defined.
d1 = IXTdataset_1d;
if nargin < 5
  period=1;
end
r = libisisexc('IXTraw_file', 'sumspec', self, int32(spec_nums),xmin,xmax,int32(period), d1);