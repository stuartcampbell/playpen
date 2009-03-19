clear classes;
nx=6;
ny=4;
x=[1:nx];
y=[1:ny];
%s(2:4)=0;
 w1=IXTdataset_1d(IXTbase('ename1'),'name1','title1',int32(nx),rand(1,nx),rand(1,nx),'s-label',x,'x-label','x-units',logical(0),logical(0));
 w4=IXTdataset_1d(IXTbase('ename1'),'name1','title1',int32(nx),rand(1,nx),rand(1,nx),'s-label',x,'x-label','x-units',logical(0),logical(0));
w2=IXTdataset_2d(IXTbase('ename1'),'name1','title1',int32(nx),int32(ny),rand(nx-1,ny-1),rand(nx-1,ny-1),'s-label',x,'x-label','x-units',logical(0),logical(1),y,'y-label','y-units',logical(0),logical(1));
w3=IXTdataset_2d(IXTbase('ename1'),'name1','title1',int32(nx),int32(ny),rand(nx-1,ny-1),rand(nx-1,ny-1),'s-label',x,'x-label','x-units',logical(0),logical(1),y,'y-label','y-units',logical(0),logical(1));
w5= w2 + w3
w6 = w1 + w4