function modQ=spe2modQ(data)

Q=spe2sqe(data);
modQ=sqrt(Q(:,:,1).^2+Q(:,:,2).^2+Q(:,:,3).^2);
