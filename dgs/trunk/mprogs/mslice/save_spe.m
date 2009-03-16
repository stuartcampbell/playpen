function save_spe(data_in,spe_filename)

% function save_spe(data_in,spe_filename)
% save data in the format of an .spe file
% data should have fields .en (1,ne), .S (ndet,ne) and .ERR (ndet,ne)
% R.C. 24-July-1998, 17-Mar-1999

% === return if data_in is not an spe data structure with appropriate fields
if ~exist('data_in','var')|isempty(data_in)|~isstruct(data_in)|...
      ~isfield(data_in,'en')|isempty(data_in.en)|...
      ~isfield(data_in,'S')|isempty(data_in.S)|...
      ~isfield(data_in,'ERR')|isempty(data_in.ERR),
   disp('No data to save.');
   return;
end

% === return if error opening .spe filename
if ~exist('spe_filename','var')|isempty(spe_filename)|~ischar(spe_filename),
   disp(['Incorrect filename given. Spe data not saved.']);
   help save_spe;
   return;
end
fid=fopen(spe_filename,'wt');
if fid==-1,
   disp(['Error opening .spe file ' spe_filename ' . Data not saved.']);
   return
end

% === reconstruct data with unmasked detectors
data=data_in;
ne=length(data.en);
nulldata=-1e+30;
if isfield(data,'det_group')&isfield(data,'total_ndet'),
   if data.det_group(end)>data.total_ndet,
         format short g;
         disp(['data structure is incompatible with the total number of detectors' num2str(data.total_ndet)]);
         return;
   end
   ndet=data.total_ndet;
   if isfield(data,'det_theta'),
	   data.det_theta=zeros(ndet,1);
      data.det_theta(data.det_group)=data_in.det_theta;
   end
   data.S=nulldata*ones(ndet,ne);
   data.S(data.det_group,:)=data_in.S;
   data.ERR=zeros(ndet,ne);
   data.ERR(data.det_group,:)=data_in.ERR;
else
   % no masked detectors, data probably comes from a command line operation
   ndet=size(data.S,1);
end

% === eliminate spurious NaN or Inf points
index=(isnan(data.S)|isinf(data.S)|isnan(data.ERR)|isinf(data.ERR));
if sum(index(:))>=1,
   disp(sprintf('%d points with NaN or Inf data. ',sum(index(:))));
   data.S(index)=nulldata;
   data.ERR(index)=0;
end
disp(['Saving .spe file ' spe_filename]);

% === write ndet, ne
fprintf(fid,'%5d%5d\n',ndet,ne);    

% === write phi grid (unused)
if isfield(data,'det_theta'),
   phi_grid=data.det_theta'*180/pi;	% radians --> degrees
   phi_grid=[phi_grid 0];   
else
   phi_grid=1:(ndet+1);
end
fprintf(fid,'%s\n','### Phi Grid');
fprintf(fid,'%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G%-10.4G\n',phi_grid(:));
if rem(ndet+1,8)~=0,
   fprintf(fid,'\n');
end

% === write energy grid
fprintf(fid,'%s\n','### Energy Grid');
de=data.en(1,2)-data.en(1,1);
en_grid=[ data.en-de/2 data.en(1,ne)+de/2];
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
   % fprintf(fid,'%10.1E%10.1E%10.1E%10.1E%10.1E%10.1E%10.1E%10.1E\n',data.ERR(i,:)');
   if rem(ne,8)~=0,
   	fprintf(fid,'\n');
	end
end
fclose(fid);
disp('--------------------------------------------------------------');
