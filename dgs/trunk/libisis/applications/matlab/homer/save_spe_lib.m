function save_spe_lib(data_in,spe_filename)

format long
 fid=fopen(spe_filename,'wt');
 if fid==-1,
    disp(['Error opening .spe file ' spe_filename ' . Data not saved.']);
    return
 end


data=data_in;
ndet=data.total_ndet;
ne=length(data.en);
disp(['Saving .spe file ' spe_filename]);
en_grid=data.en;

% jj=find(isnan(data.S)==1);
% data.S(jj)=0;
% jj=find(isnan(data.ERR)==1);
% data.ERR(jj)=0;

% Correction by TGP:
nulldata=-1e+30;
index=(isnan(data.S)|isinf(data.S)|isnan(data.ERR)|isinf(data.ERR));
index=(index|abs(data.S)>abs(nulldata)|(abs(data.S)<1/abs(nulldata)&abs(data.S)>0));
if sum(index(:))>=1,
   disp(sprintf('%d points with NaN or Inf data. ',sum(index(:))));
   data.S(index)=nulldata;
   data.ERR(index)=0;
end

% === write ndet, ne
fprintf(fid,'%8d%8d\n',ndet,ne-1);    
ne=ne-1;
% === write phi grid (unused)
% if isfield(data,'det_theta'),
%    phi_grid=data.det_theta'*180/pi;	% radians --> degrees
%    phi_grid=[phi_grid 0];   
% else
   phi_grid=zeros((ndet+1),1);
% end
fprintf(fid,'%s\n','### Phi Grid');
fprintf(fid,'%-10.4g%-10.4g%-10.4g%-10.4g%-10.4g%-10.4g%-10.4g%-10.4g\n',phi_grid(:));
%if rem(ndet+1,8)~=0,
   fprintf(fid,'\n');
%end

% === write energy grid
fprintf(fid,'%s\n','### Energy Grid');
en_grid=round(en_grid*1e5)/1e5;	%truncate at the 5th decimal point
fprintf(fid,'%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G\n',en_grid(:));

if rem(ne+1,8)~=0,
  	fprintf(fid,'\n');
end

% === write S(det,energy) and ERR(det,energy)
for i=1:ndet,
   fprintf(fid,'%s\n','### S(Phi,w)');
   fprintf(fid,'%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G\n',data.S(i,:)');   
     
   if rem(ne,8)~=0,
   	fprintf(fid,'\n');
	end
   fprintf(fid,'%s\n','### Errors');
   fprintf(fid,'%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G\n',data.ERR(i,:)');
    
   if rem(ne,8)~=0,
   	fprintf(fid,'\n');
	end
end
fclose(fid);
disp('--------------------------------------------------------------');
