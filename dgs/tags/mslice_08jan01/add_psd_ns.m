function data=add_psd_ns(spefile_in,spefile_out)

% function data=add_psd_ns(spefile_in,spefile_out)
% spefile_in, spefile_out = complete filenames (with paths included)
% combine north and south parts of an spe PSD file <spefile_in>
% and save result in file <spefile_out> just the north part 
% Radu Coldea 02-oct-1999

if nargin~=2,
   disp(['Both input and output filenames are required']);
   help add_psd_ns;
   return
end

[filename_in,dir_in]=stripath(spefile_in);
[filename_out,dir_out]=stripath(spefile_out);
if isempty(dir_out),
   dir_out=dir_in;
end
tubes=13;
channels=64;
% GENERATE INDEXES FOR NORTH AND SOUTH PARTS
south=[];
north=[];
for i=1:tubes,
   south=[south (i-1)*channels+(1:channels/2)];
   north=[north i*channels-(0:(channels/2-1))];   
end
south=south';
north=north';

data=load_spe(spefile_in);
data_south=data;
data_south.total_ndet=length(south);
data_south.S=data.S(south,:);
data_south.ERR=data.ERR(south,:);
data_south.det_group=(1:size(data_south.S,1))';
data_south.det_theta=data.det_theta(south);
data_north=data;
data_north.total_ndet=length(north);
data_north.S=data.S(north,:);
data_north.ERR=data.ERR(north,:);
data_north.det_group=(1:size(data_north.S,1))';
data_north.det_theta=data.det_theta(north);
file_south='data_south.spe';
file_north='data_north.spe';
save_spe(data_south,[dir_in file_south]);
save_spe(data_north,[dir_in file_north]);

data=add_spe([0.5 0.5],dir_in,{file_south,file_north},[dir_out filename_out]);
%delete([dir_in file_south]);
%delete([dir_in file_north]);
