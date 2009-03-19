function datum = IXTdatum( varargin )
% % ! ISISEXCdatum_array Create a IXTdatum object 
% % ! REQUIRED INPUT PARAMETERS
% % ! datum_array = IXTdatum( );
% %
% % !! @author Dickon Champion
% % !! @V 1.0 Date: 2004/06/14
% % !-----------------------------------------------------------------------------------------------------------------------------------
% % ! IXTdatum_array
% % ! ==============
% % !	A 1  dimensional dataset that contains a value and associated error
% % !
% % !
% % ! 1D case:
% % ! ---------
% % !	val					real		Signal
% % !	err					real		Standard error
datum.val=[0];
datum.err=[0];

datum = class(datum,'IXTdatum');    
if (nargin > 0)
    datum = libisisexc('IXTdatum','create',datum,varargin);
end           

