function crystal = ISISEXCcrystal( entry_name, varargin )
% ! Create an ISISEXCcrystal object
% ! REQUIRED INPUT PARAMETERS
% ! crystal = ISISEXCcrystal( [entry_name], [name], [distance], [energy], 
%                             [dspace], [relection], [rho_h], [rho_v], 
%                             [horiz_ap], [vert_ap]);
%                                         
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCcrystal
% ! ==============
% ! NeXus class: NXcrystal
% !
% !	entry_name				char	Name of entry in NeXus file
% ! name					char	name of the crystal
% !	distance				real	distance from sample (m) (-ve if upstream of sample)
% !	energy					real	energy of reflected neutrons (mev)
% !	dspace					real	lattice parameter of nominal reflection (ang)
% !	relection(3)			int		reflection [h,k,l]
% !	rho_h					real	radius of curvature of crystal (m)
% !	rho_v					real	    :      
% !	horiz_ap				real	aperture width if rectangular (m)
% !	vert_ap					real	    :    height :     :        :
% !
% ! NOTES:
% ! ------
% !	Some of the names have been changed from the NeXus standard simply to keep them to a reasonable length
% !	Take the energy as the ACTUAL energy, not simply the nominal energy
% !


switch nargin
case 0
    % if no input arguments, create a default object
    crystal.entry_name = 'none';
    crystal.name = 'none';
    crystal.distance = NaN;
    crystal.energy= NaN;
    crystal.dspace = NaN;
    crystal.relection = int32([NaN, NaN, NaN]);
    crystal.rho_h = NaN;
    crystal.rho_v = NaN;
    crystal.horiz_ap = NaN;
    crystal.vert_ap = NaN;
    
    crystal = class(crystal,'ISISEXCcrystal');
    
case 1
    % if single argument of class ISISEXCcrystal, return it
    if isa(entry_name,'ISISEXCcrystal')
        crystal = entry_name;
    else
        
        error('!! It is not a valid ISISEXCcrystal constructor');
        return
    end
    

    % create object using specified arguments
    
case 10    
    check=1;
    crystal.entry_name = entry_name;
    if(isa(varargin{1},'char')), crystal.name = varargin{1};  else check =0;  end
    if(isa(varargin{2},'double')&&(length(varargin{2})==1)), crystal.distance = varargin{2};  else check =0;  end
    if(isa(varargin{3},'double')&&(length(varargin{3})==1)), crystal.energy = varargin{3};   else check =0; end
    if(isa(varargin{4},'double')&&(length(varargin{4})==1)), crystal.dspace = varargin{4};   else check =0; end
    if(isa(varargin{5},'int32')&&(length(varargin{5})==3)), crystal.relection = varargin{5};   else check =0; end %%% Matlab, doesn't like plot integers...
    if(isa(varargin{6},'double')&&(length(varargin{6})==1)), crystal.rho_h = varargin{6};   else check =0; end   
    if(isa(varargin{7},'double')&&(length(varargin{7})==1)), crystal.rho_v = varargin{7};  else check =0;  end
    if(isa(varargin{8},'double')&&(length(varargin{8})==1)), crystal.horiz_ap = varargin{8};   else check =0; end    
    if(isa(varargin{9},'double')&&(length(varargin{9})==1)), crystal.vert_ap = varargin{9};  else check =0;  end
    

    
    if (check==1), 
        crystal = class(crystal,'ISISEXCcrystal'); 
    else 
        error('!! It is not a valid ISISEXCcrystal constructor');
        crystal=[];
    end
    
    
    
otherwise
    error('!! It is not a valid ISISEXCcrystal constructor');
end
