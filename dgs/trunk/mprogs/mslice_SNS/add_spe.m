function data=add_spe(weight,spedir,spefiles,spefileout,scale)

% function data=add_spe(weights,spedir,spefiles,spefileout)
% adds several .spe files to build up statistics
% parameters: weight={100 200 } (proton current in uAhrs);
%             spedir='m:\matlab\anal_spe\spe\'; (directory of .spe files)
%             spefiles={'','',}; (names of .spe files to be added)
%             spefileout='combinedfiles.spe'; (name of file with combined spe files to be saved in directory spedir)												
% scale number to multiply the overall scale of the resulting data file
% example: look up function add_spe_example
% Radu Coldea 02-Oct-1999 


% === define value for nulldata
nulldata=-1e+30;

% === return if number of weights and spefiles are inconsistent
if length(weight)~=length(spefiles),
   disp(['Error: ' num2str(length(weight)) ' weights not consistent with ' num2str(length(spefiles)) ' spe files given.']);
   data=[];
   return;
end

%detail.index=zeros(length(weight),1);
%detail.weight=weight(:)';
%detail.S=zeros(length(weight),1);
%detail.ERR=zeros(length(weight),1);

% === read one file at a time
for i=1:length(weight),
%   disp([spedir spefiles{i}]);
   data=load_spe([spedir spefiles{i}]);
   if isempty(data),
      disp(['Could not load .spe file ' spedir spefiles{i}]);
      return;
   end
   index=~(data.S(:,1)<=nulldata);	% (ndet,1) is 1 where pixel is data and 0 where detector has 'nulldata' in current data set
   %drawdet([400+data.det_group(~index)'],'y');
   %hold on;
   if ~exist('cumm_index','var'),	% if it is the first file to be loaded, initaialize cumm_index, cumm_S, ERR2 ...
      det_theta=data.det_theta;
      en=data.en;
      cumm_index=index; 	% cummulative index 1 (data) 0 (nulldata) for all files loaded so far  	
      cumm_weight=weight(i)*index;	% (ndet,1) contains cummulative weights of each detector, 0 if pixel is 'nulldata' in all sets loaded so far    
      cumm_S=weight(i)*(index*ones(size(data.en))).*data.S;	% summulative S summation, has 0 where cumm_index is 0
      cumm_ERR2=(weight(i)^2)*((data.ERR).^2).*(index*ones(size(data.en)));	% cummulative ERR2 summation, has 0 where cumm_index is 0
   else
      if any(size(data.S)~=size(cumm_S)),
         disp('Current data set not consistent with number of detectors or energy bins of previous data sets. Return.');
         return;
      end
      if any(det_theta~=data.det_theta),
         disp('Warning: phi grid not equivalent');
      end
      if any(en~=data.en),
         disp('Warning: energy grid not equivalent');
      end
      cumm_index=cumm_index|index;	% one detector has data if it either has had data 
      	% in one of the previous data sets or has data in this current data set
      cumm_weight=cumm_weight+weight(i)*index;	% (ndet,1)
      cumm_S=cumm_S+weight(i)*(index*ones(size(data.en))).*data.S;	% (ndet,ne)
	   cumm_ERR2=cumm_ERR2+(weight(i).^2)*((data.ERR).^2).*(index*ones(size(data.en)));	% (ndet,ne)
   end
%   detail.index(i)=index(det);
%   detail.S(i)=data.S(det,enbin);
%   detail.ERR(i)=data.ERR(det,enbin);
   disp(sprintf('masked detectors %d and weight %15.5g',sum(not(index(:))),weight(i)));
   disp(sprintf('overall masked detectors %d',sum(not(cumm_index(:)))));      
end

% === put 'nulldata' with 0 error bar in final data set for pixel positions which were 'nulldata' in ALL data sets
data.S=nulldata*ones(size(data.S));
data.S(cumm_index,:)=cumm_S(cumm_index,:)./(cumm_weight(cumm_index)*ones(size(data.en)));
data.ERR=zeros(size(data.ERR));
data.ERR(cumm_index,:)=sqrt(cumm_ERR2(cumm_index,:))./(cumm_weight(cumm_index)*ones(size(data.en)));

%detail.cumm_index=cumm_index(det);
%detail.cumm_weight=cumm_weight(det);
%detail.cumm_S=cumm_S(det,enbin);
%detail.cumm_ERR2=cumm_ERR2(det,enbin);
%detail.data_S=data.S(det,enbin);
%detail.data_ERR=data.ERR(det,enbin);

if exist('scale','var')&~isempty(scale)&isnumeric(scale),
   data.S=data.S*scale;
   data.ERR=data.ERR*scale;
end
   
% === save result in filename fileout
data.filename=spefileout;
save_spe(data,spefileout);
disp(sprintf('masked detectors %d and weight %15g',sum(not(index(:))),sum(weight(:))));


