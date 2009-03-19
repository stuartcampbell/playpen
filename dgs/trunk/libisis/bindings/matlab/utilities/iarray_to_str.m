function str_out=iarray_to_str (val)
% Convert array of integers to cell array of strings suitable for writing to
% a text file.
% Consecutive numbers M, M+1, M+2,...,N in the input array are written
% as M-N in the cell string:
%   e.g. [-5,-4,-3,-1,0,1,2,5,7,9,10,11,12] is written '-5--3 -1-2 5 7 9-12'

% Trivial case of empty array
if isempty(val)
    str_out='';
    return
end

% Find ranges of consecutive elements
diff_unity=[false,diff(val)==1,false];
ibeg=find(diff(diff_unity)==1);   % those elements that start a series of consecutive elements
iend=find(diff(diff_unity)==-1);  % those elements that end a series of consecutive elements

% *** LOGIC ERROR: doesn't work with:
% str_out=iarray_to_str([1,2,12:20])


% Find ranges of elements not part of a consecutive sequence
if ~isempty(ibeg)
    istart=[1,1+iend];
    ifinish=[ibeg-1,length(val)];
    strtmp=cell(1,1+2*length(ibeg));    % Create temporary cellstr to hold output
    lentmp=zeros(size(strtmp));
else
    istart=1;
    ifinish=length(val);
    strtmp=cell(1);
end

% Write to strings:
for i=1:length(istart)
    if istart(i)<=ifinish(i)
        strtmp{2*i-1}=[int2str(val(istart(i):ifinish(i))),' '];
        lentmp(2*i-1)=length(strtmp{2*i-1});
    else
        strtmp{2*i-1}=' ';
        lentmp(2*i-1)=1;
    end
    if i~=length(istart)
        strtmp{2*i}=[int2str(val(ibeg(i))),'-',int2str(val(iend(i))),' '];
        lentmp(2*i)=length(strtmp{2*i});
    end
end

% Reformat into similar length strings

% Make one string:
str=blanks(sum(lentmp));
clen=cumsum([0,lentmp]);
for i=1:length(strtmp)
    str(clen(i)+1:clen(i+1))=strtmp{i};
end

% Find points where 
lenlin=50;  % Minimum length of line
itoken_new=find(diff(isspace(str))==-1);  % elements immediately preceeding another token
ibrk=find(diff(mod(itoken_new,lenlin))<0);      % last element number on a line
if ~isempty(ibrk)
    ilinbeg=[1,itoken_new(ibrk)+1];
    ilinend=[itoken_new(ibrk),length(str)];
    str_out=cell(1,length(ibrk)+1);
    for i=1:length(ilinbeg)
        str_out{i}=strtrim(str(ilinbeg(i):ilinend(i)));
    end
else
    str_out{1}=strtrim(str);
end
