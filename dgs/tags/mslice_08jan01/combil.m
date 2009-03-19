function label=combil(label2,u2,u1)

% function label=combil(label2,u2,u1)
% combines labels of const line vector u1(1,4)+label2*u2
% u1=[ 1 0 0 0 ]
% u2=[ 0 0 1 0 ] label2='Q_l'
% R.C. 10-August-1998

if ~exist('u1','var'),
   u1=[0 0 0 0];
end

if (u2==[0 0 0 1]),
   if u1(4)==0,
      %  scan along energy at a constant wavevector
      label='Energy ';
      %      label=[sprintf('Const Q=[%7.3g, %7.3g, %7.3g] ',u1(1:3)) label2];
   else
      label=[sprintf('[ %7.3g, %7.3g, %7.3g, %7.3g] + [ %7.3g, %7.3g, %7.3g, %7.3g]',u1(1:4),u2(1:4)) '*' label2];
      disp(['Scan along a mixed wavevector-energy direction. Labels not combined.']);
   end
	return
end
if (u2(4)~=0),
   label=[sprintf('[ %7.3g, %7.3g, %7.3g, %7.3g] + [ %7.3g, %7.3g, %7.3g, %7.3g]',u1(1:4),u2(1:4)) '*' label2];
   disp(['Scan along a mixed wavevector-energy direction. Labels not combined.']);
   return
end   
label=['[ '];	% begin label
for i=1:3,
   if u2(i)==0,
      label=[label num2str(u1(i),'%7.3g') ', '];	% then next i
   else
      % u2(i) will be part of the label	
		b=sprintf('%-+7.3g',u2(i));
      if abs(u2(i))==1,
      	b=b(1);	% keep only sign as a string
      end       
      if u1(i)~=0,	% both u1(i) and u2(i) non-zero
         label=[label num2str(u1(i),'%7.3g') b label2 ', '];     
      else
         if u2(i)~=1,
            label=[label b label2 ', '];	% 0.5*Q_l
         else
            label=[label label2 ', '];
         end   
      end      
	end % if
end % for

label=[label(1:(length(label))-2) ' ]'];
%label=[label(1:(length(label))-2) ' ], E=' num2str(u1(4),'%7g') ' meV' ];	% end label
