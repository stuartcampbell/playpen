function save_msk(msk,filename)

% function save_msk(msk,filename)

% === open output file as an ASCII text file 
fid=fopen(filename,'wt');
if fid==-1,
   disp(['Error opening output .msk file ' filename]);
   return
end
disp(sprintf('Saving masking file (%d spectra) to file \n%s',length(msk),filename));
drawnow;

% === save msk in format of 10 spec numbers per line
fprintf(fid,'%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d\n',msk);
fclose(fid);