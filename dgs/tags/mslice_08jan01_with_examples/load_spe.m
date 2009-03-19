function data=load_spe(spe_filename)

% function data=load_spe(spe_filename)
% loads data from an ASCII .spe file   
% returns data.en (1,ne)[meV]    
%             .det_group (ndet,1) 
%             .det_theta (ndet,1)[radians] total scattering angle of each detector 
%             (to be superseded by angles from the .phx file if using function builspe(spe_filename,phx_filename,...))
%             .S (ndet,ne)[intensity in units as given by data.axislabel(4,:)] 
%             .ERR (ndet,ne) [errors of .S, same units]
%             .filename [string] = spe_filename
%             where ne = number of points along the energy axis, 
%                   ndet = number of detector groups
% R.C. 24-July-1998
% 6-August-1998 incorporate field .det_group and .title

% === if no input parameter given, return
if ~exist('spe_filename','var'),
   help load_spe;
   return
end

filename=deblank(spe_filename); % remove blancs from beginning and end of spe_filename
filename=fliplr(deblank(fliplr(filename)));
% === if error opening file, return
fid=fopen(filename,'rt');
if fid==-1,
   disp(['Error opening file ' filename ' . Data not read.']);
   data=[];
   return
end
fclose(fid);

try % fortran algorithm
   [data.S,data.ERR,data.en]=load_spe_df(filename);
   disp(['Fortran loading of .spe file : ' filename]);      
   [ndet,ne]=size(data.S);
   disp([num2str(ndet) ' detector(s) and ' num2str(ne) ' energy bin(s)']);   
   data.det_theta=ones(ndet,1);
catch % matlab algorithm  
   disp(['Matlab loading of .spe file : ' filename]);         
   fid=fopen(filename,'rt');
   % === read number of detectors and energy bins
   ndet=fscanf(fid,'%d',1);   % number of detector groups 
   ne=fscanf(fid,'%d',1);  % number of points along the energy axis
   temp=fgetl(fid);	% read eol
   disp([num2str(ndet) ' detector(s) and ' num2str(ne) ' energy bin(s)']);
   drawnow;

   % === read 2Theta scattering angles for all detectors
   temp=fgetl(fid);	% read string '### Phi Grid'
   det_theta=fscanf(fid,'%10f',ndet+1); % read phi grid, last value superfluous
   det_theta=det_theta(1:ndet)*pi/180;  % leave out last value and transform degrees --> radians
   temp=fgetl(fid);	% read eol character of the Phi grid table
   temp=fgetl(fid);	% read string '### Energy Grid'
   en=fscanf(fid,'%10f',ne+1); % read energy grid
   en=(en(2:ne+1)+en(1:ne))/2; % take median values, centres of bins

   S=zeros(ndet,ne);
   ERR=S;
   for i=1:ndet,
      temp=fgetl(fid);
      %while isempty(temp)|isempty(findstr(temp,'### S(Phi,w)')),
      temp=fgetl(fid);			% get rid of line ### S(Phi,w)
      %end
      temp=fscanf(fid,'%10f',ne);
      S(i,:)=transpose(temp);
      temp=fgetl(fid);
      %while isempty(temp),
      %   temp=fgetl(fid),			
      %end
      temp=fgetl(fid);
      temp=fscanf(fid,'%10f',ne);
      ERR(i,:)=transpose(temp);   
   end
   fclose(fid);

   % BUILD UP DATA STRUCTURE 
   data.en=en';
   data.det_theta=det_theta(:);
   if exist('det_dtheta','var'),
      data.det_dtheta=det_dtheta;
   end
   data.S=S;
   data.ERR=ERR;
end

data.det_group=(1:ndet)';
[name,pathname]=stripath(spe_filename);  
data.filename=name;
data.filedir=pathname;
data.total_ndet=ndet;