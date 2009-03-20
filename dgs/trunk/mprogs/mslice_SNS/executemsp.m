function executemsp(dirname,match,output_type)

% function executemsp(dirname)
% execute all msp files in on directory
% match = partial string in filenames
% output_type = can force to output files of a certain type
%               if absent or empty use default type in .msp files

if ~exist('dirname','var')|isempty(dirname)|~ischar(dirname),
   dirname=pwd;
end

if exist('output_type','var')&~isempty(output_type)&ischar(output_type),
   switch output_type,
   case 'none',
      disp(['No files are to be saved. Return']); 
      return;
   case '.cut',
      index=2;
      ext='cut';
   case '.xye',
      index=3;
      ext='xye';
   case '.smh',
      index=4;
      ext='smh';
   case 'Mfit .cut',
      index=5;
      ext='cut';
   case '.hkl',
      index=6;
      ext='hkl';
   otherwise
      disp(['Only the following options are allowed for output_type :']);
      disp(['none | .cut | .xye  | .smh | Mfit .cut | .hkl']);
      disp(['Currently output_type = ' output_type]);
      return;
   end
else
	output_type='default';   
end

% === run through all .msp files in <dirname> which match the requested criterion in <match>
files=dir(dirname);
for i=1:length(files),
   if ~(files(i).isdir)&~isempty(findstr(lower(files(i).name),'.msp'))& ...
      ~isempty(findstr(lower(files(i).name),lower(match))),   % if a true .msp file
      filename=[dirname filesep files(i).name];
      ms_load_msp(filename);
      h_output_type=findobj('Tag','ms_cut_OutputType');
		h_output_file=findobj('Tag','ms_cut_OutputFile');
		if isempty(h_output_file)|isempty(h_output_type),
   		disp(['Object holding output filename or type could not be located. Return.']);
   		return;
		end
      %      ms_load_data;
      ms_calc_proj;
      if strcmp(output_type,'default'),
         ms_cut;
      else   
         file=get(h_output_file,'String');
         file=file(~isspace(file));	% eliminate white spaces
         if isempty(file),
            disp(['Filename empty. No cut ploted or saved.']);
         else
	         pos=findstr(file,'.');
         	if ~isempty(pos),
               file=[file(1:pos(1)) ext];
            else
               file=[file '.' ext];
            end
            set(h_output_file,'String',file);
            if index<=length(get(h_output_type,'String')),
            	set(h_output_type,'Value',index);
               ms_cut;
            else
               disp(['Requested output ' output_type ' not possible for this configuration.']);
            end
         end
      end  
   end
end