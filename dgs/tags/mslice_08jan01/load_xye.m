function cut=load_xye(filename)

% function cut=load_xye(filename)
% function to read a .xye file and return a structure with fields
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
% 	   title: 'cut\spe750_12s_apr98_all_nesw_cut_i_a.cut'

% === open <filename> for reading
fid=fopen(filename,'rt');
if fid==-1,
   disp([ 'Error opening file ' filename ]);
   cut=[];
   return
end

% === advance until have found a line beginning with numbers
number =[];
header=0;
while isempty(number)& not(feof(fid)),
   temp=fgetl(fid);
   header=header+1;
	number=sscanf(temp,'%f');
end
if isempty(number),	% have reached end of file without encountering numbers
   disp(sprintf('Have reached end of file without encountering numbers'));
   return;
end
if header>1,
   disp(sprintf('Skip %d line(s) of text at beginning',header-1));
end

% === read x,y,e 
data=[number(:); fscanf(fid,'%f')]; % read until the end of file
n=length(data)/3;
if floor(n)~=n,
   disp(['File ' filename ' not in expected 3-column format. Data not read.']);
   data=[];
   return;
end
d=reshape(data,3,n)';
cut.x=d(:,1);
cut.y=d(:,2);
cut.e=d(:,3);
cut.title=avoidtex(stripath(filename));
disp(['Loading .xye cut ( ' num2str(n) ' data points) from file : ']);
disp(filename);
cut.x_label=[];
cut.y_label=[];

% === check to see if label information is appended at the end of the .cut file
% === x_label (1 line) , y_label (1 line) and title (can be many lines)
temp=fgetl(fid);
while ischar(temp)&(isempty(temp)|isempty(temp(~isspace(temp)))),
   temp=fgetl(fid);
end
if ischar(temp)&(~isempty(temp)&~isempty(temp(~isspace(temp)))),
   cut.x_label=temp;
   temp=fgetl(fid);
	if ischar(temp)&(~isempty(temp)&~isempty(temp(~isspace(temp)))),
      cut.y_label=temp;
      temp=fgetl(fid);
      i=1;
      while ischar(temp)&(~isempty(temp)&~isempty(temp(~isspace(temp)))),
         if i==1,
            cut.title{i}=temp;
         else
            cut.title{i}=temp;            
         end
         i=i+1;
         temp=fgetl(fid);
      end
   end
end
fclose(fid);
if isempty(cut.x_label),
   cut.x_label='x-variable';
end
if isempty(cut.y_label),
   cut.y_label='y-variable';
end

   