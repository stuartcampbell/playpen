def calc_dspacings(latt_parm,latt_ang,hlims,klims,llims):
    """
       latt_parm is a 3 element numpy array in inverse angstroms
       latt_ang is a 3 element numpy arry in degrees
    """
    from UB import Bmat_gen
    from numpy.linalg import norm
    B=Bmat_gen(latt_parm,latt_ang)
    h=range(hlims[0],hlims[1])
    k=range(klims[0],klims[1])
    l=range(llims[0],llims[1])
    hklmat=zeros((len(h)*len(k)*len(l),3))
    idx4=0
    for idx in h:
      for idx2 in k:
        for idx3 in l:
           hklmat[idx4]=array([idx,idx2,idx3])
	   idx4=idx4+1
    d=zeros(hklmat.shape[0])
    for idx in range(len(d)):
        Qvec=dot(B,hklmat[idx])
	d[idx]=2.0*pi/norm(Qvec)
    out=[hklmat,d]
    return out
    
