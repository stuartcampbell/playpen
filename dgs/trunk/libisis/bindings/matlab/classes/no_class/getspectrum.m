function r = getspectrum(spec_num,period)
% dataset_1d=getspectrum(rawfile,spectrum_number,period)
% 
% function to read in raw file spectra to an IXTdataset_1d object. spectra
% to be read in are defined by the spectrum_number which can be a single
% number or array-valued which can either be a consecutive list created  
% implicitly by matlab -> [spec_1:spec_n], or a list of specific spectra 
% defined by the user -> [spec_1 spec_2 spec_3... spec_n]. 
% If it is array valued the data will populate an array of IXTdataset_1d objects.
% The period argument defines the period to extract the spectrum from, the default is 0 and allows
% for spectra spanning periods. If the period defined is non-zero, then the spectrum
% number MUST not span a period, it cannot be array-valued, the first array
% number will be used
d1 = IXTdataset_1d;
if nargin < 2
  period=0;
end

self = ixf_global_var('data_source','get','rawfile');

if isempty(self)
    error('No default RAW file currently assigned and no IXTraw_file object given')
end
% period cannot be array-valued, so takes first element however big it is
period=period(1);
r = libisisexc('IXTraw_file', 'getspectrum', self, int32(spec_num),int32(period), d1);