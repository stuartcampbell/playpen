function spectra = IXTspectra( varargin )
%  IXTspectra(IXTbase, 'name', 'title', [signal], [error], IXTunits,
spectra.base = IXTbase;
spectra.spec_lookup=[int32(1)];
spectra.spec_no=[int32(1)];
spectra.ndet=[int32(1)];
spectra.det_index=[int32(1)];
spectra.det_no=[int32(1)];
spectra= class(spectra,'IXTspectra');    
if (nargin > 0)           
    spectra = libisisexc('IXTspectra','create',spectra,varargin);
end