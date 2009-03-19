function head(run_number)
% head(run_number)
%
% returns the header information for the raw file defined by the
% run_number, or path to a raw file defined in run_number
% a combined set of runs can also be defined, with limited practical use
%
% run numbers can be defnied in the following ways:
% 1) a numerical array of numbers -> [12345  45673 34436] or a single
% numeric
% 2) an array of strings -> char('12345' '45673' '34436') or a single
% string
% 3) a cell array of strings -> {'12345' '45673' '34436'}
% 4) a cell array of numerics -> {12345  45673 34436}
% 5) or a path to a particular raw file -> 'C:\data\MAR11060.RAW' or a cell
% array of paths
%
% See also mhead
[rawpath,len]=make_path_from_par({run_number});
%if len ~= 1
%    error('bad input to head')
%end
rawfile=IXTraw_file;
for i=1:len
    rawfile(i)=IXTraw_file(rawpath{i});
end
% use the fortran method less faffing about and more portable for other
% interfaces
libisisexc('IXTraw_file', 'head', rawfile);
%raw_head(rawfile);
% this works in matlab also
% hdr = gget(rf,'hdr');
% user= gget(rf,'user');
% titl= gget(rf,'titl');
% crpb= gget(rf,'crpb');
% line1 = ['Run ID : ' hdr(1:8) '        User: ' hdr(9:28) '  Inst: ' user(5,:)];
% line2 = ['Protons: ' hdr(73:80) ' uAhrs  Date: ' hdr(53:72) ' to ' deblank(crpb(17,:)) ' ' deblank(crpb(20,:))];
% disp(line1);
% disp(line2);
% disp(titl(1:80));