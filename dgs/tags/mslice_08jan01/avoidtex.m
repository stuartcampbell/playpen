function s2=avoidtex(s1)

% function s2=avoidtex(s1)
% s1, s2 strings of characters (one row)
% PUT '\' IN FRONT OF '\','_' and '^' CONTROL CHARACTERS 
% TO AVOID Tex Interpretor Warning message 
% used to put exact filename in figure titles
% Radu Coldea 02-Oct-1999

s2=s1;
pos=sort([findstr(s1,'\') findstr(s1,'_') findstr(s1,'^')]);

for i=1:length(pos),
   s2=[s2(1:(pos(i)+i-2)) '\' s2((pos(i)+i-1):length(s2))];
end   
   



