function smooth_curve(filename,np)

data=load_fit(filename);
[x,perm]=sort(data.x);
y=data.y(perm);
xi=linspace(x(1),x(end),np);
yi=interp1(x,y,xi,'spline');
data.x=xi;
data.y=yi;
data.e=[];
delete(filename);
save_cut(data,filename,'xye');
