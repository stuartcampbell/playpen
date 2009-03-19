function instrument = IXTinstrument( varargin )
% ! Create an ISISEXCchopper_instrument object
% ! REQUIRED INPUT PARAMETERS
% ! instrument  = ISISEXCchopper_instrument( [entry_name], [name], [source], 
%                                                    [moderator], [monochromator], 
%                                                    [aperture], [attenuator],
%                                                    [workspaces]);
%
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTchopper_instrument
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
% ! name				char				Name on instrument (HET, MARI, MAPS etc.)
% !	source				IXTsource		Source information (see above for definition of type source)
% !	moderator			IXTmoderator	Moderator information  (see above for definition of type moderator)
% !   instrument.ci = IXTchopper_instrument
% !   instrument.di = IXTdiffraction_instrument
% !	apertures			IXTaperture		Aperture info. can be more than one, hence an allocatable array
% !	attenuators			IXTattenuator	Attenuator info. can be (in principle) more than one, hence a allocatable array
% ! spectra             IXTspectra
% ! detector            IXTdetector     pointer to the detector information
% in the instrument
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	

    % if no input arguments, create a default object
    instrument.base = IXTbase;
    instrument.inst_type=int32(-1);
    instrument.name = ' ';
    instrument.source = IXTsource;
    instrument.moderator = IXTmoderator;
    instrument.ci = IXTchopper_instrument;
    instrument.di = IXTdiffraction_instrument;
    instrument.apertures = IXTaperture;
    instrument.attenuators = IXTattenuator;
    instrument.spectra = IXTspectra;
    instrument.detector = IXTdetector;    
    instrument = class(instrument,'IXTinstrument');
if (nargin > 0)
    instrument = libisisexc('IXTinstrument','create',instrument,varargin);
end
