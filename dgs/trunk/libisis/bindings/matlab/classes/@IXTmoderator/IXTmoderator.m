function moderator = IXTmoderator( varargin )
% ! Create an IXTmoderator object 
% ! REQUIRED INPUT PARAMETERS
% ! moderator = IXTmoderator ( IXTbase, 'name', [distance], [width], 
%                                   [height], [thickness], [angle], [temperature], 
%                                   'pulse_model', [pulse_pars] )
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCmoderator
% ! ================
% ! NeXus class: Not defined in NeXus, but should be. The ISISI proposed NeXus standard contains NXmoderator
% !
% !	base			IXTbase	Name of entry in NeXus file
% !	name				char	name of the moderator
% !	distance			real	distance of moderator face from sample (m)
% !	width				real	moderator width (m)
% !	height				real	moderator height (m)
% !	thickness			real	moderator thickness (m)
% !	angle				real	angle of normal to incident beam (rad): +ve if normal is anticlockwise from incident beam
% !	temperature			real	temperature (k)
% ! *	pulse_model			char	Moderator pulse shape model
% ! *	pulse_pars(:)		real	Moderator pulse-shape parameters (interpretation depends on pulse_model)
% !
% !
    % if no input arguments, create a default object
    moderator.base = IXTbase;
    moderator.name = ' ';
    moderator.distance = 0.0;
    moderator.width = 0.0;
    moderator.height = 0.0;
    moderator.thickness = 0.0;
    moderator.angle = 0.0;
    moderator.temperature= 0.0;
    moderator.pulse_model = 'model';
    moderator.pulse_pars = [0];
    moderator = class(moderator,'IXTmoderator');
    if (nargin > 0)
    moderator = libisisexc('IXTmoderator','create',moderator,varargin);
    end
