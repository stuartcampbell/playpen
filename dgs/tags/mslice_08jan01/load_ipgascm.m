function data=load_ipgascm(ipgfile)

% load multiple jpg ASCII files 
% (generated using IGIS on an IRIS run to produce an .ipg binary file then
% read in genie2 and save individual workspaces in filename.'no'

% === open the ipg ASCII file as a text file in reading mode

data=[];
i=1;
filename=[ipgfile '.1'];
while exist(filename,'file'),
   [x,y,err]=genie(filename);
   if isempty(data),
      data.en=x';
      data.S=y';
      data.ERR=err';
   else
      data.S=[data.S; y'];
      data.ERR=[data.ERR; err'];
   end
   i=i+1;
   filename=[ipgfile '.' num2str(i)];
end
[ndet,ne]=size(data.S);
data.det_theta=zeros(ndet,1);
