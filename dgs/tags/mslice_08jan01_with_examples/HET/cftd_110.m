function cftd_110

ax=axis;
k=linspace(ax(1),ax(2),100);
J=14.8/2;
w1=2*J*sqrt(1-cos(pi*k).^4);
hold on ;
h=plot(k,w1,'r-');
set(h,'Linewidth',3);
hold off;
