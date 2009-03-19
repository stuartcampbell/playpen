function msk=sort_msk(file1,file2)

% function to sort a .msk file
% function msk=sort_msk(file1,file2)

% === read in .msk file with spectrum numbers
fid=fopen(file1,'r');
if fid==-1,
   disp(['Error opening input .msk file ' file1]);
   return
end
msk=fscanf(fid,'%d');
fclose(fid);


% === sort spec numbers in ascending order
msk=sort(msk);

% === eliminate double masking
n=1;
while n<length(msk),
   if msk(n)==msk(n+1),
      disp(['Eliminate double masking of spec #' num2str(msk(n))]);
      msk(n+1)=[];
   else
      n=n+1;
   end
end

% === open output file as an ASCII text file 
save_msk(msk,file2);