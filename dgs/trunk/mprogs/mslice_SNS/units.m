function [unitlabel,unitlength]=units(U)

% function [unitlabel,unitlength]=units(U)
% returns type of units and norm of 4-d vector U(1,4) in wavevector-energy space
% ex: U=[1.16 0 0 0], unitlabel='1.16 Å^{-1}'  unitlength=1.16
%     U=[0 0 0 1],    unitlabel='(meV)'        unitlength=1

if all(U(1:3)==0)&(U(4)~=0),% Energy axis
   unitlength=1;   
   unitlabel=['(meV)'];   
elseif any(U(1:3)~=0)&(U(4)==0),	% Wavevector direction
   unitlength=norm(U);
   unitlabel=[' in ' num2str(unitlength,'%5.3f') ' Å^{-1}' ];   
else
   unitlength=NaN;
   unitlabel='mixed units';
   disp(sprintf('Warning: mixed wavevector and energy units in vector U=[%5.3f,%5.3f,%5.3f,%5.3f]',U(1:4)));
end

