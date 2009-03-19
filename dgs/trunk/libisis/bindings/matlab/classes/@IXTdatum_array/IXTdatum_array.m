function datum_array = IXTdatum_array( varargin )
% % ! ISISEXCdatum_array Create a IXTdatum_array object 
% % ! REQUIRED INPUT PARAMETERS
% % ! datum_array = IXTdatum_array( );
% %
% % !! @author Dickon Champion
% % !! @V 1.0 Date: 2004/06/14
% % !-----------------------------------------------------------------------------------------------------------------------------------
% % ! IXTdatum_array
% % ! ==============
% % !	A 1  dimensional dataset that contains a value and associated error array
% % !
% % !
% % ! 1D case:
% % ! ---------
% % !	signal					real(1:nx)		Signal
% % !	error					real(1:nx)		Standard error
datum_array.base=IXTbase;
datum_array.signal=[0];
datum_array.error=[0];

datum_array = class(datum_array,'IXTdatum_array');    
if (nargin >0)           
datum_array = libisisexc('IXTdatum_array','create',datum_array,varargin);
end
