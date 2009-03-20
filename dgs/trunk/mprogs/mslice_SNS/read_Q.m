function read_Q

% function read_Q
% read interactively Q values from a horizontal-plane tobyplot diagram

%=== if 'tobyplot' figure does not exist, exit
fig=findobj('Tag','tobyplot');
if isempty(fig),
   disp('Can only read Q values from a horizontal-plane tobyplot diagram.');
   return
end

figure(fig);
[qx,qy]=ginput(1);
disp(sprintf('Wavevector transfer is Q = %5.3f Å^{-1}',norm([qx qy])));
