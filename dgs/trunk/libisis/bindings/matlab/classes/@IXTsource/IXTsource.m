function source = IXTsource( varargin )
% ! IXTsource    ( IXTbase,'facility_name',[frequency] )
% ! Create an IXTsource object
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! IXTsource
% ! =============
% ! NeXus class: NXsource
% !
% !	base				IXTbase		Name of entry in NeXus file
% !	facility_name			char		Source name e.g. ISIS
% !	frequency				real		Source frequency (Hz)
% !
% !-----------------------------------------------------------------------------------------------------------------------------------

    source.base =IXTbase;
    source.facility_name = 'none';
    source.frequency = 0.0;
    source = class(source,'IXTsource');
if (nargin > 0)
    dataset_1d = libisisexc('IXTsource','create',source,varargin);
end