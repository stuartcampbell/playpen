function hkl=q2rlu(Q,ar,br,cr)

% function hkl=q2rlu(Q,ar,br,cr)
% transforms Q in Å^{-1} into rlu projections 
% on the reciprocal basis vectors ar,br,cr

[aa,bb,cc]=basis_hkl(ar,br,cr);
%aa=ar/dot(ar,ar);
%bb=br/dot(br,br);
%cc=cr/dot(cr,cr);

if ndims(Q)==3,
   h=Q(:,:,1)*aa(1)+Q(:,:,2)*aa(2)+Q(:,:,3)*aa(3);
   k=Q(:,:,1)*bb(1)+Q(:,:,2)*bb(2)+Q(:,:,3)*bb(3);
	l=Q(:,:,1)*cc(1)+Q(:,:,2)*cc(2)+Q(:,:,3)*cc(3);
   hkl=cat(3,h,k,l);
elseif ndims(Q)==2,
   h=Q(:,1)*aa(1)+Q(:,2)*aa(2)+Q(:,3)*aa(3);
   k=Q(:,1)*bb(1)+Q(:,2)*bb(2)+Q(:,3)*bb(3);
	l=Q(:,1)*cc(1)+Q(:,2)*cc(2)+Q(:,3)*cc(3);
   hkl=[h';k';l']';
end
   
   
   