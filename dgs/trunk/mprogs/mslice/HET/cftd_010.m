function cftd_010 

ax=axis;
k=linspace(ax(1),ax(2),100);
J=14.8/2;
w1=2*J*abs(sin(pi*k));
hold on ;
h=plot(k,w1,'r-');
set(h,'Linewidth',2);
hold off;
%set(h,'Zdata',zeros(size(get(h,'Xdata'))));