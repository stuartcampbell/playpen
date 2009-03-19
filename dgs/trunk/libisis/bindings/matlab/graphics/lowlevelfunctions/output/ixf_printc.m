function ixf_printc(name)

% function ixf_printc(name)
% used to produce color hardcopy of 1d plots 
% (using the matlab Figure Plot submenu produces a b/w hardcopy)
% <name> is the network name of a colour printer 
% click on figure and then write in the command line 
% >>ixf_printc(' network name of printer')
%     example >> ixf_printc(' \\NDAIRETON\Phaser560:')	for R3 3rd floor fax room
%             >> ixf_printc(' \\NDAIRETON\Phaser0:') 	for R3 2nd floor printer room
%             >> ixf_printc(' \\NDAIRETON\PS11PS:') 	for b/w printer in HET cabin
%             >> ixf_printc(' \\NDAIRETON\PS3PS:') 	   for b/w printer closest to MARI cabin
% or edit in ixf_printc.m the line with the default color printer path
%     and then >>ixf_printc or select ColourPrint, default printer in figure top menu
IXG_ST_PRINTERS =  ixf_global_var('libisis_graphics','get','IXG_ST_PRINTERS');

tempfile=[tempdir 'mslice.eps'];
eval(['print ' tempfile ' -noui -depsc']);
if ~exist('name','var')
   % === default color printer network path  
   %   name=' \\NDAIRETON\PS11PS:';
   %   name=' \\NDAIRETON\Phaser560:';	
   %   name=' \\NDAIRETON\Phaser0:';	    
   %   name=' \\NDAIRETON\PS11PS:';   % for printer in HET cabin 
   %   name=' \\NDAIRETON\PS3PS:';    % for printer closest to MARI cabin    
   %   name=' \\NDAIRETON\MAPSColour:';   % for MAPS colour printer   
   name=IXG_ST_PRINTERS.default;
elseif isempty(name)||~ischar(name)
   name=IXG_ST_PRINTERS.default;
end
command=['! copy /B ' tempfile name];
%command=['! copy /B ' tempfile ' \\NDAIRETON\Phaser0:'];

disp('Printing current colour figure using command ');
disp(command);
eval(command);
