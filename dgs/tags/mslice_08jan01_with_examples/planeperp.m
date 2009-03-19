function planeperp(hkl,ar,br,cr)

% function planeperp(hkl,ar,br,cr)
% hkl=(1,3) rlu axis
% ar,br,cr(1,3) reciprocal basis vectors
% displays equation of plane perpendicular to a hkl axis 

% === if hkl=[0 0 0 1] (energy) then say any wavevector is perp to this axis and return
if exist('hkl','var')&(~isempty(hkl))&(size(hkl,1)==1)&(size(hkl,2)==4)&all(hkl(1:3)==0)&(hkl(4)~=0),
   disp(['Any wavevector axis is perpendicular to the energy axis in the 4d wavevector-energy space.']);
	return;   
end

% === if hkl axis not given return
if ~exist('hkl','var')|isempty(hkl)|(size(hkl,1)~=1)|~((size(hkl,2)==3)|(size(hkl,2)==4)),
   disp(['Wrong syntax. Command not executed.']);
   disp(['Check size of hkl vector.']);
   help planeperp;
	return;   
end

% === eliminate the energy componenet if hkl vector is given as [h k l E] 
if size(hkl,2)==4,
   hkl=hkl(1,1:3);
end

% === if reciprocal lattice basis not given and not accessible from MSlice Control Window also return
fig=findobj('Tag','ms_ControlWindow');
if ~exist('ar')
   if isempty(fig)|isempty(get(fig,'UserData'))|(~isfield(get(fig,'UserData'),'ar')),
   	disp(['Reciprocal lattice vectors not given and not accessible from the MSlice ControlWindow']);
   	disp('Command not executed');
   	help planeperp;
      return;
   else
      data=get(fig,'UserData');
      ar=data.ar;
      br=data.br;
      cr=data.cr;
	end
end

% === calculate equation of plane perpendicular to the hkl direction
vec_hkl=hkl(1)*ar+hkl(2)*br+hkl(3)*cr;
hh=dot(ar,vec_hkl);
kk=dot(br,vec_hkl);
ll=dot(cr,vec_hkl);
scale=max([hh kk ll]);
hh=hh/scale;
kk=kk/scale;
ll=ll/scale;
form='%+5.3f';
disp(['Plane perpendicular to axis [ ' num2str(hkl(1),form) ' ' num2str(hkl(2),form) ' ' num2str(hkl(3),form)  '] is']);;
disp([ num2str(hh,form) ' *h ' num2str(kk,form) ' *k ' num2str(ll,form) ' *l = 0']);