function [ar,br,cr]=basis_r(as,bs,cs,aa,bb,cc)

% returns reciprocal basis vectors, 2 possible calling syntaxes
% (1) function [ar,br,cr]=basis_r(a,b,c)
%     assumes a,b,c are 3-component vectors of the direct lattice in some coordinate system
%     returns ar,br,cr (1,3) in the same coordinate system
% (2) function [ar,br,cr]=basis_r(as,bs,cs,aa,bb,cc)
%     assumes as, bs and cs are lattice parameters (Å^{-1}) and aa, bb and cc are angles (rad) 
%     returns ar,br,cr (1,3) components relative to a reference frame where [100]||as and [001]|| as x bs

if (length(as)==3)&(length(bs)==3)&(length(cs)==3)&~exist('aa','var')&~exist('bb','var')&~exist('cc','var'),
   % as, bs, cs are all 3-component vectors (1,3), so continue after the if statement
   a=as(:)';
   b=bs(:)';
   c=cs(:)';
elseif exist('aa','var')&exist('bb','var')&exist('cc','var'),
   % as, bs, cs are the lattice parameters (Å^{-1}) and aa, bb and cc are the angles (deg) between the unit cell vectors
   % create 3-component unit cell vectors a,b,c of the direct lattice 
   a=as*[1 0 0];
   b=bs*[cos(cc) sin(cc) 0];
   cx=cos(bb);
   if sin(cc)==0,
      disp('Check angle cc. Vectors as and bs seem to be colinear. The 3d reciprocal basis was not calculated.');
      ar=[];
      br=[];
      cr=[];
      return;
   end
	cy=(cos(aa)-cos(bb)*cos(cc))/sin(cc);
   cz=sqrt(1-cx^2-cy^2);
   c=cs*[cx cy cz];
else
    disp('Wrong parameter passing for reciprocal basis calculation.');
    help basis_r;
    ar=[];
    br=[];
    cr=[];
    return
end 

% now a,b,c are the 3-component vectors (1,3) of the unit cell of the direct lattice 
vol=dot(a,cross(b,c));	% unit cell volume in Å^{-3}
if real(vol)~=0,
	ar=2*pi*cross(b,c)/vol;
	br=2*pi*cross(c,a)/vol;
  	cr=2*pi*cross(a,b)/vol;
else
   ar=[];
   br=[];
   cr=[];
   disp('The three direct lattice vectors are coplanar. The 3d reciprocal basis was not calculated.');
   return;   
end
     