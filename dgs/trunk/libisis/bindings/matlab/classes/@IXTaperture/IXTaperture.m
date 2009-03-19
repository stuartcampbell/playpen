function aperture = IXTaperture( varargin )
% ! Create an IXTaperture object
% ! REQUIRED INPUT PARAMETERS
% ! aperture = IXTaperture( IXTbase, 'name', [distance], 'shape',
%                               [horiz_posn], [vert_posn], [width], 
%                               [height], [radius]);
%                                         
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCaperture
% ! ===============
% ! NeXus class: NXaperture
% !
% !	base			IXTbase	name of entry in NeXus file
% ! name				char	name of the aperture
% !	distance			real	distance of centre from sample (m) (-ve if upstream of sample)
% !	shape				char	'rectangular', 'circular'
% !	horiz_posn			real	horizontal position of aperture centre (m)
% !	vert_posn			real	vertical      :      :     :        :   :
% !	width			real	aperture width if rectangular (m)
% !	height				real	    :    height :     :        :
% !	radius				real	aperture radius if circular (m)
% !	
% !
% ! NOTES:
% ! ------
% !	Some of the names have been changed from the NeXus standard simply to keep them to a reasonable length
% !


    % if no input arguments, create a default object
    aperture.base = IXTbase;
    aperture.name = ' ';
    aperture.distance = 0.0;
    aperture.shape = ' ';
    aperture.horiz_posn = 0.0;
    aperture.vert_posn = 0.0;
    aperture.width = 0.0;
    aperture.height = 0.0;
    aperture.radius = 0.0;
    
    aperture = class(aperture,'IXTaperture');
if (nargin > 0)
    aperture = libisisexc('IXTaperture','create',aperture,varargin);
end    
 
