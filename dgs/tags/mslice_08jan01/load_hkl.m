function cut=load_hkl(filename)

% function cut=load_hkl(filename)
% function to read a cut file in .hkl format 
% and return structure cut with fields (n=number of data points)
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
%h,k,l,energy: [1xn double]
%   x_label:
%   y_label:
% 	   title: 'cut\spe750_12s_apr98_all_nesw_cut_i_a.cut'

% === open <filename> for reading
fid=fopen(filename,'rt');
if fid==-1,
   disp([ 'Error opening file ' filename ]);
   cut=[];
   return
end

% === read header block first (axes labels)
cut.x_label=[];
cut.y_label=[];
cut.title=[];

temp=[];
% === if not end of file move on to the next line until a line with characters is found
while ~feof(fid)&isempty(temp),
	temp=fgetl(fid);		% read next complete line
end
if isempty(temp),
   disp('Have reached the end of file without finding any header or data blocks. Cut not read.');
   cut=[];
   fclose(fid);
   return;
end
% temp is now a non-empy string
data=sscanf(temp,'%f');	% try to see if it is made up of numbers separated by blanc spaces, data=[] if string of characters 
while isempty(data)&~isempty(temp),	% still in the header block, temp is a string of characters 
   % === try to see if it is the x_label
   if ~isempty(findstr(lower(temp),'%x_label= ')),	
      if isempty(cut.x_label),
         cut.x_label=temp((length('%x_label= ')+1):end);	% x_label is the string until the end of the line 
      else
         cut.x_label={cut.x_label,temp((length('%x_label= ')+1):end)};
      end
 	% === try to see if it is the y_label   
   elseif ~isempty(findstr(lower(temp),'%y_label= ')), 
      if isempty(cut.y_label),
         cut.y_label=temp((length('%y_label= ')+1):end);
      else
         cut.y_label={cut.y_label,temp((length('%y_label= ')+1):end)};
      end
 	% === try to see if it is the title label   
 	elseif ~isempty(findstr(lower(temp),'%title= ')),	
    	if isempty(cut.title),
         cut.title={avoidtex(filename), temp((length('%title= ')+1):end)};	% append cut filename at beginning of title
      elseif iscell(cut.title),
         cut.title{length(cut.title)+1}=temp((length('%title= ')+1):end);
      end
   % == try to see if is line with column titles    
   elseif ~isempty(findstr(lower(temp),'h(rlu)'))&~isempty(findstr(lower(temp),'k(rlu)'))&...
         ~isempty(findstr(lower(temp),'l(rlu)'))&~isempty(findstr(lower(temp),'energy(mev)')),
		%disp('Have read all header lines'); 
   else
      disp(sprintf('Cannot interpret header line %s',temp));
   end   
   temp=[];
   % === if not end of file move on to the next line until a line with characters is found
   while ~feof(fid)&isempty(temp),
      temp=fgetl(fid);		% read next complete line
   end
   % temp is either a string of characters or a list of numbers or is empty 
   if ~isempty(temp),      
      data=sscanf(temp,'%f');	% try to see if it is made up of numbers separated by blanc spaces   
   end   
end   
if isempty(temp),
   disp('Have reached the end of file without finding any data block. Cut not read.');
   cut=[];
   fclose(fid);
   return;
end

% === if no labels are stored in the file give some by default
if isempty(cut.x_label),
   cut.x_label='x-variable';
end
if isempty(cut.y_label),
   cut.y_label='y-variable';
end
if isempty(cut.title),
   cut.title=filename;
end

% === now read the rest of the data block and reshape as an (nx7) matrix
data=[data(:);fscanf(fid,'%f')];
if rem(length(data),7)~=0,
   disp(sprintf('File does not appear to have the standard 7-column format. Cut not read.'));
   fclose(fid);
   cut=[];
   return;
end

fclose(fid);
data=reshape(data,7,length(data)/7)';
cut.h=data(:,1);
cut.k=data(:,2);
cut.l=data(:,3);
cut.energy=data(:,4);
cut.x=data(:,5);
cut.y=data(:,6);
cut.e=data(:,7);

disp(['Loading cut in .hkl format (' num2str(length(cut.x)) ' data points) from file : ']);
disp(filename);
