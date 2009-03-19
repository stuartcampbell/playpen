function attenuator = IXTattenuator(  varargin )
% ! Create an ISISEXCattenuator object
% ! REQUIRED INPUT PARAMETERS
% ! attenuator = ISISEXCattenuator( [entry_name], [name], [distance], 
%                                   [material], [thickness],
%                                   [attenuation]);
%                                         
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCattenuator
% ! =================
% ! NeXus class: NXattenuator
% !
% !	base			IXTbase				name of entry in NeXus file
% ! name				char				name of the attenuator
% !	distance			real				distance of centre from sample (m) (-ve if upstream of sample)
% !	material			char				type (e.g. polythene)
% !	thickness			real				thickness (m)
% !	attenuation			IXTdataset_1d	attenuation factor as a function of energy (meV)
% !	
% !
% ! NOTES:
% !	NeXus standard inadequate at moment - attenuation will be energy dependent. Lets store the
% !	attenuation factor as a function of energy
% !

    % if no input arguments, create a default object
    attenuator.base = IXTbase;
    attenuator.name = ' ';
    attenuator.distance = 0.0;
    attenuator.material = ' ';
    attenuator.thickness = 0.0;
    attenuator.attenuation = IXTdataset_1d;

    attenuator = class(attenuator,'IXTattenuator');
if (nargin > 0)
    attenuator = libisisexc('IXTattenuator','create',attenuator,varargin);
end 