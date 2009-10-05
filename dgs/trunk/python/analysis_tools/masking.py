def mask_file_write(filename,lst):
   fid=open(filename,'w')
   for idx in range(len(lst)):
     fid.writelines(lst[idx])
   fid.close()

def mask_create(bank):
    pixels=range(128)
    tubes=range(8)
    lst=[]
    for idx in tubes:
       lst.append(map(lambda x: "bank_%d_%d_%d\n"%(bank,idx,x),pixels))
    return lst    
