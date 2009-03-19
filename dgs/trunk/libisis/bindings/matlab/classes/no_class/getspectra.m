function r = getspectra(spec_nums,period)
% dataset_2d=getspectra(rawfile,spectrum_array,period)
%
% function to read in raw file spectra to an IXTdataset_2d object. spectra
% to be read in are defined by the spectrum array variable which can either
% be a consecutive list created implicitly by matlab -> [spec_1:spec_n], or 
% a list of specific spectra defined by the user -> [spec_1 spec_2 spec_3 ... spec_n].
% The period argument can be single or array-valued, and defines the period(s) to extract
% spectra from, the default is 1. If it is specified to be zero then spectra spanning periods 
% can be defined.
d2 = IXTdataset_2d;

self = ixf_global_var('data_source','get','rawfile');

if isempty(self)
    error('No default RAW file currently assigned and no IXTraw_file object given')
end

if nargin < 3
  period=[1];
end
r = libisisexc('IXTraw_file', 'getspectra', self, int32(spec_nums),int32(period), d2);