function data=read_spe(spe_filename,phx_filename,sum_filename,data_plot_symbol)

% function data=read_spe(spe_filename,phx_filename,sum_filename,data_plot_symbol)
% reads a .spe file   
% R.C. 20-July-1998

data=load_spe(spe_filename); % data.en (1,ne), data.S (ndet,ne),  data.ERR (ndet,ne)
phx=load_phx(phx_filename);	% [det_num, theta(deg), psi(deg)] (ndet,3)
sum_spec=load_sum(sum_filename);	% [spec_num,int,err] (ndet,3)

% check compatibility of .spe and .phx files against same number of detector groups
if size(data.S,1)~=size(phx,1),
   disp(['.spe file ' spe_filename ' and .phx file ' phx_filename ' not compatible']);
   disp(['Number of detector grous is different: ' num2str(size(data.S,1)) ' and ' num2str(size(phx,1))]);
   data=[];
   return
end

% REMOVE DATA IN MASKED DETECTORS
masked=(data.S(:,1)<=-1e+30);
disp(['Total number of masked detectors is ' num2str(sum(masked))]);
%disp(['Masked detectors in file have numbers : ' sprintf(' %d ',phx(masked,1))]);
data.S(masked,:)=[];
data.ERR(masked,:)=[];
phx=phx(~masked,:);

% BUILD UP DATA STRUCTURE 
data.det_num=phx(:,1);
data.spec_num=det2spec(data.det_num);
data.det_theta=phx(:,2)*pi/180;
data.det_psi=phx(:,3)*pi/180;
sum_det=sum2det(phx(:,1),sum_spec);	% extract white vanadium integrals for the unmasked detectors
data.det_whitevan_int=sum_det(:,2);
data.det_whitevan_err=sum_det(:,3);
if exist('data_plot_symbol','var'),
   data.symbol=data_plot_symbol;
else
   data.symbol='ro';
end
   
