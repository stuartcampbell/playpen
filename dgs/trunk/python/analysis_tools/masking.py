

def mask_file_write(filename,lst):
   fid=open(filename,'w')
   for idx in range(len(lst)):
     fid.writelines(lst[idx])
   fid.close()

def mask_create(bank):
    """ routine that produces a list to completely mask out a bank
        bank = banknum
    """
    pixels=range(128)
    tubes=range(8)
    lst=[]
    for idx2 in range(len(bank)):
      for idx in tubes:
         lst.append(map(lambda x: "bank%d_%d_%d\n"%(bank[idx2],idx,x),pixels))
    return lst
    
def mask_tubes(bank,tubes):
    """
       routine to mask out a single single tubes
       bank is a list of bank numbers 
       tubes is a list of tubes ids
    """
    pixels=range(128)
    lst=[]
    for idx2 in range(len(bank)):
      for idx in tubes:
         lst.extend(map(lambda x: "bank%d_%d_%d\n"%(bank[idx2],idx,x),pixels))
    return lst    

def bank_read(filename):
    f=open(filename,'r')
    jk=f.readlines()
    bnk=zeros(len(jk))
    pixstp=zeros(len(jk))
    pixstrt=zeros(len(jk))
    for idx in range(len(jk)):
      jktmp=jk[idx].strip().split(',')
      print "bank %s\n" %jktmp[0]
      print jk[idx]+"\n"
      bnk[idx]=int64(jktmp[0])
      
      if len(jktmp[1])>0:
         pixstrt[idx]=int64(jktmp[1])
         pixstp[idx]=int64(jktmp[2])
    lst=[]
    for idx in range(len(bnk)):
       pixbtm=range(pixstrt[idx])
       print 'pixstart:'+str(pixstrt[idx])+'\n'
       pixtp=range(pixstp[idx]+1,128)
       print 'pixstop:'+str(pixstp[idx])+'\n'
       for idx2 in range(8):
           for idxp1 in pixbtm:
	      lst.append('bank%i_%i_%i\n'%(bnk[idx],idx2,idxp1))
	   for idxp1 in pixtp:
	      lst.append('bank%i_%i_%i\n'%(bnk[idx],idx2,idxp1))  
   
    
    return lst
    
