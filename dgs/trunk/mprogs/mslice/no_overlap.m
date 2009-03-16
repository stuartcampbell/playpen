function no_overlap(h)

% h = handle array to labels
set(h(:),'Visible','off','FontName','times','FontSize',13);
n=length(h);
cen=zeros(n,2);
len=zeros(n,2);
m=1;
e=get(h(m),'Extent');
set(h(m),'Visible','on');
cen(m,:)=[e(1)+e(3)/2 e(2)+e(4)/2];
len(m,:)=[e(3) e(4)];

for i=2:n,
   e=get(h(i),'Extent');
   cen_i=[e(1)+e(3)/2 e(2)+e(4)/2];
   len_i=[e(3) e(4)];
   temp=(abs(cen_i(1)-cen(1:m,1))<=2*(len_i(1)/2+len(1:m,1)/2));
   temp=temp&(abs(cen_i(2)-cen(1:m,2))<=2*(len_i(2)/2+len(1:m,2)/2));
   if sum(temp(:))==0, 
      m=m+1;
      cen(m,:)=cen_i;
      len(m,:)=len_i;
      set(h(i),'Visible','on'); 
   end
end
