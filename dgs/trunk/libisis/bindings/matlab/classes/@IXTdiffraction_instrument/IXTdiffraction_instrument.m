function diffraction_instrument = IXTdiffraction_instrument( varargin )
% ! Create an ISISEXCchopper_instrument object
% ! REQUIRED INPUT PARAMETERS
% ! diffraction_instrument  = ISISEXCchopper_instrument( [entry_name], [name], [source], 
%                                                    [moderator], [monochromator], 
%                                                    [aperture], [attenuator],
%                                                    [workspaces]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTdiffraction_instrument
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	

    % if no input arguments, create a default object
    diffraction_instrument.base = IXTbase;
    diffraction_instrument = class(diffraction_instrument,'IXTdiffraction_instrument');
if (nargin > 0)
    diffraction_instrument = libisisexc('IXTdiffraction_instrument','create',diffraction_instrument,varargin);
end
