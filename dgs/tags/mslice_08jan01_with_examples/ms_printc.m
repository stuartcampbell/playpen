function ms_printc(name)

% function ms_printc(name)
% used to produce color hardcopy of 1d plots 
% (using the matlab Figure Plot submenu produces a b/w hardcopy)
% <name> is the network name of a colour printer 
% click on figure and then write in the command line 
% >>ms_printc(' network name of printer')
%     example >> ms_printc(' \\NDAIRETON\Phaser560:')	for R3 3rd floor fax room
%             >> ms_printc(' \\NDAIRETON\Phaser0:') 	for R3 2nd floor printer room
%             >> ms_printc(' \\NDAIRETON\PS11PS:') 	for b/w printer in HET cabin
%             >> ms_printc(' \\NDAIRETON\PS3PS:') 	   for b/w printer closest to MARI cabin
% or edit in ms_printc.m the line with the default color printer path
%     and then >>ms_printc or select ColourPrint, default printer in figure top menu

tempfile=[tempdir 'mslice.eps'];
eval(['print ' tempfile ' -noui -depsc']);
if ~exist('name','var')|isempty(name)|~ischar(name),
   % === default color printer network path  
   %   name=' \\NDAIRETON\PS11PS:';
   %   name=' \\NDAIRETON\Phaser560:';	
   %   name=' \\NDAIRETON\Phaser0:';	    
   %   name=' \\NDAIRETON\PS11PS:';   % for printer in HET cabin 
   %   name=' \\NDAIRETON\PS3PS:';    % for printer closest to MARI cabin    
   %   name=' \\NDAIRETON\MAPSColour:';   % for MAPS colour printer   
   name=' \\ndablagrave\LASER00:';
end
command=['! copy /B ' tempfile name];
%command=['! copy /B ' tempfile ' \\NDAIRETON\Phaser0:'];

disp(['Printing current colour figure using command ']);
disp(command);
eval(command);
