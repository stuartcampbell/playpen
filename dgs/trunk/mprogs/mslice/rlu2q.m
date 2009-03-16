function Q=rlu2q(hkl,ar,br,cr)

% function Q=rlu2q(hkl,ar,br,cr)
% transforms rlu projections on the basis vectors ar,br,cr into Q in Å^{-1} 
% hkl has same dimensions as Q

if ndims(hkl)==3,
   Qx=hkl(:,:,1)*ar(1)+hkl(:,:,2)*br(1)+hkl(:,:,3)*cr(1);
   Qy=hkl(:,:,1)*ar(2)+hkl(:,:,2)*br(2)+hkl(:,:,3)*cr(2);
   Qz=hkl(:,:,1)*ar(3)+hkl(:,:,2)*br(3)+hkl(:,:,3)*cr(3);
   Q=cat(3,Qx,Qy,Qz);   
elseif ndims(hkl)==2,
   Qx=hkl(:,1)*ar(1)+hkl(:,2)*br(1)+hkl(:,3)*cr(1);
   Qy=hkl(:,1)*ar(2)+hkl(:,2)*br(2)+hkl(:,3)*cr(2);
   Qz=hkl(:,1)*ar(3)+hkl(:,2)*br(3)+hkl(:,3)*cr(3);
   Q=[Qx'; Qy'; Qz']';
end
   
   
   