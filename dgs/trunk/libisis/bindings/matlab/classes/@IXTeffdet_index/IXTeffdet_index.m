function effdet_index = IXTeffdet_index( varargin )
% ! Create an IXTeffdet_index object
% ! REQUIRED INPUT PARAMETERS
% ! effdet_index  = IXTeffdet_index(base, [good_index], [total_index]); 
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTeffdet_index
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
%   good_index     int
%   total_index    int
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type effdet_index to store info about the detectors that make
% ! up a single spectrum.
% !	
    % if no input arguments, create a default object
    effdet_index.base = IXTbase;
    effdet_index.good_index = [int32(1)];
    effdet_index.total_index = [int32(1)];
    effdet_index = class(effdet_index,'IXTeffdet_index');
if (nargin > 0)
    effdet_index = libisisexc('IXTeffdet_index','create',effdet_index,varargin);
end
