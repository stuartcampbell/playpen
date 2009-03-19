function chopper_instrument = IXTchopper_instrument( varargin )
% ! Create an ISISEXCchopper_instrument object
% ! REQUIRED INPUT PARAMETERS
% ! chopper_instrument  = ISISEXCchopper_instrument( [entry_name], [name], [source], 
%                                                    [moderator], [monochromator], 
%                                                    [aperture], [attenuator],
%                                                    [workspaces]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTchopper_instrument
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
% !	monochromator		IXTfermi_chopper		Fermi chopper information  (see above for definition of type chopper)
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	

    % if no input arguments, create a default object
    chopper_instrument.base = IXTbase;
    chopper_instrument.monochromator = IXTfermi_chopper;
    chopper_instrument = class(chopper_instrument,'IXTchopper_instrument');
if (nargin > 0)
    chopper_instrument = libisisexc('IXTchopper_instrument','create',chopper_instrument,varargin);
end
