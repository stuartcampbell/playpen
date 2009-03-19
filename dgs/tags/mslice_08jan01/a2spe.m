function a2spe(number)

% function a2spe(number)
% use to convert IRIS ipg data into spe format
% 1. use igis/icon on ISIS vms to convert .raw data into .ipg format
% 2. use g2a in genie2 to convert .ipg into ascii files for each detector
% 3. use matlab a2spe to read in all genie ascii files 
%     and save output to an spe file 
% R.Coldea 08-February-2000

%=== prompt for run number of none given
if ~exist('number','var')|isempty(number),
   number=input('Input run number ','s');
   number=number(~isspace(number));
end

%=== convert to string value if number given
if isnumeric(number),
   number=num2str(number); % convert to string
end   

%=== check files
if ~exist(['irs' number '.1'],'file')|~exist(['IRS' number '.1'],'file'),
   disp(['Can not locate files irs' number '.1, .2 etc. Command not executed.']);
end

%=== read files one by one and save final result in spe format
if exist(['irs' number '.1'],'file'),
   data=load_ipgascm(['irs' number]);
else
   data=load_ipgascm(['IRS' number]);
end
if ~isempty(data),
	save_spe(data,['irs' number '.spe']);
end   