function out = project( sample, proj_info )
%PROJECT Summary of this function goes here
%  Detailed explanation goes here


   
% % -- NATURE OF THE GEOMETRY --
%     
% if((distance==0)||(isnan(distance))),
%     emode=2;    %indirect geometry
%     efixed=crystal.energy;
% else
%     emode=1;    %direct geometry
%     efixed=monochromator.energy;
% end

   
% -- NATURE OF THE SAMPLE --
if (is_crystal(sample)),  
        samp=1; %sample is single crystal  
else    
        samp=2; %sample is powder 
end



% -- ANALYSIS TYPE --

analmode=get_analmode(proj_info); % +++++++++++ not defined yet +++++++++++

if (analmode==1),
        disp('## Analysed As Single Crystal');

% -- DETECTOR TYPE --

        if (is_psd(proj_info)),
                disp('## Position Sensitive Detectors');
                out  = project_crystal_psd( sample, proj_info ); % CRYSTAL & PSD
                
        else
                disp('## "Normal" Detectors');
                out  = project_crystal( sample, proj_info );     % CRYSTAL & NON PSD                
        end
% -------------------
        
elseif (analmode==2), 
        disp('## Analysed As Powder');        
        out  = project_powder( sample, proj_info );             % POWDER
        
end