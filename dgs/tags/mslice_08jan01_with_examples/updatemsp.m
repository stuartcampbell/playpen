function updatemsp(dirname,scrap_files,path1,path2,dirtype)

% function updatemsp(dirname,scrap_files,path1,path2)
% use to update earlier versions of .msp MSlice parameter files 
% in directory <dirname> to the format required by the latest 
% version of the MSlice programme 
% <scrap_files>={'xye','cut'}	latest version of MSlice (after April 1999) 
% allows only one type of output cut file to be specified at one time, 
% specify which output formats are to be LOST during this update
% example >> updatemsp('c:\radu\temp','xye') will keep references to .cut files
% replaces all occurrences of path1 with path2 in entry dirtype (all dirtypes MspDir, DataDir, PhxDir, OutputDir)
% >> updatemsp  % check and update all .msp files in current directory
% >> updatemsp(pwd,'xye') % eliminate references to .xye files in .msp files in current directory
% >> updatemsp(pwd,'xye','','m:\matlab\kcuf3\spe\','DataDir'); % put a noiminal Data Directory in all .msp file in current dir 

if ~exist('dirname','var')|isempty(dirname)|~ischar(dirname),
   dirname=pwd;
end
if ~exist('scrap_files','var')|isempty(scrap_files)|~ischar(scrap_files)|...
      ~isempty(findstr(lower(scrap_files),'xye')),
   scrap_file={'XyeDir', 'XyeFile'}; % default option
   scrap_files='xye';
else
   scrap_file={'CutDir', 'CutFile'};
   scrap_files='cut';
end

files=dir(dirname);
for i=1:length(files),
   if ~(files(i).isdir)&~isempty(findstr(lower(files(i).name),'.msp')),	% if a true .msp file
      %temp_filename=[dirname filesep 'temp.msp'];
      temp_filename=tempname;
      %disp(temp_filename);
      ftemp=fopen(temp_filename,'wt');	% temporary file 
      if ftemp==-1,
%         disp(['Error opening temporary file ' dirname filesep 'temp.msp']);
			disp(['Error opening temporary file ' temp_filename ]);
			disp(['Check writing priviledges and disk space for this directory']);
         return;
      end
      filename=[dirname filesep files(i).name];
      fid=fopen(filename,'rt');
      disp(['CHECKING .MSP FILE : ' dirname filesep files(i).name]);
      temp=fgetl(fid);
      samp=[];	% sample type = {'sigle crystal','powder'}
      anal_mode=[];	% analysis mode {'single crystal','powder'} for single crystal sample
      det_type=[];	% detector type for single crystal analysis{'psd','conventional non-psd'} 
      output_type=[];	% output cut file type {'none','.cut','.xye','.smh','Mfit .cut','.hkl'}
      while (ischar(temp))&(~isempty(temp(~isspace(temp)))),
         %disp([temp]);
         change=(0>1);	% false
        	temp0=temp;         
         % ==== find output_type if present                     
			fpos=findstr(temp,'OutputType');
         if ~isempty(fpos),
            output_type=temp;
          end
         % ==== replace 'LogDir' and 'CutDir' with 'OutputDir'
         % === if these files are not scraped altogether         
         fpos=[findstr(temp,'LogDir') findstr(temp,'CutDir')];
         if ~isempty(fpos)&isempty(findstr(lower(scrap_files),'cut')),
            fpos=findstr(temp0,'=');
            temp=['cut_OutputDir                   ' temp0(fpos:end)];            
            change=(1>0);	% true
         end
         % === replace 'LogFile' and 'CutFile' with 'OutputFile' 
         % === if these files are no scraped altogether
         fpos=[findstr(temp,'LogFile') findstr(temp,'CutFile')];
         if ~isempty(fpos)&isempty(findstr(lower(scrap_files),'cut')),
            fpos=findstr(temp0,'=');
            temp=['cut_OutputFile                  ' temp0(fpos:end)];            
            change=(1>0);	% true
         end
         % ==== replace 'XyeDir' with 'OutputDir'
         % === if these files are no scraped altogether         
         fpos=findstr(temp,'XyeDir');
         if ~isempty(fpos)&isempty(findstr(lower(scrap_files),'xye')),
            fpos=findstr(temp0,'=');
            temp=['cut_OutputDir                   ' temp0(fpos:end)];            
            change=(1>0);	% true
         end
         % === replace 'XyeFile' with 'OutputFile' 
         % === if these files are no scraped altogether
         fpos=findstr(temp,'XyeFile');
         if ~isempty(fpos)&isempty(findstr(lower(scrap_files),'xye')),
            fpos=findstr(temp0,'=');
            temp=['cut_OutputFile                  ' temp0(fpos:end)];            
            change=(1>0);	% true
         end         
         % ==== replace 'log' in path name
         fpos=findstr(temp,[filesep 'log' filesep]);
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length([filesep 'log' filesep])-1))=[filesep 'cut' filesep];
            end
            change=(1>0);	% true
         end
         % ==== replace '.log' as file extension with '.cut'
         fpos=findstr(temp,'.log');
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length('.log')-1))='.cut';
            end
            change=(1>0);	% true
         end
         % ==== replace 'zrange_min' with 'i_min'         
         fpos=findstr(temp,'zrange_min');
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length('zrange_min')-1))='i_min     ';
            end
            change=(1>0);	% true
         end
         % ==== replace 'zrange_max' with 'i_max'                  
         fpos=findstr(temp,'zrange_max');
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length('zrange_max')-1))='i_max     ';
            end
            change=(1>0);	% true
         end
         % ==== replace 'yrange_min' with 'i_min'                  
         fpos=findstr(temp,'yrange_min');
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length('yrange_min')-1))='i_min     ';
            end
            change=(1>0);	% true
         end
         % ==== replace 'yrange_max' with 'i_max'                  
			fpos=findstr(temp,'yrange_max');
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length('yrange_max')-1))='i_max     ';
            end
            change=(1>0);	% true
         end
         % ==== replace 'slice_grid_onoff' with 'slice_shad'                  
			fpos=findstr(temp,'slice_grid_onoff');
         if ~isempty(fpos),
            for i=fpos,
               temp(i:(i+length('slice_grid_onoff')-1))='slice_shad      ';
            end
            change=(1>0);	% true
         end
         % ==== replace 'plot_traj_cont' with 'plot_traj_cont_lines'                  
			fpos=findstr(temp,'plot_traj_cont');
         if ~isempty(fpos),
            fpos1=[findstr(temp,'plot_traj_cont_lines') findstr(temp,'plot_traj_cont_step') ...
                  findstr(temp,'plot_traj_cont_min') findstr(temp,'plot_traj_cont_max') ...
                  findstr(temp,'plot_traj_cont_step1') findstr(temp,'plot_traj_cont_step2')];
            if isempty(fpos1),
               fpos=findstr(temp,'=');
               if ~isempty(fpos),
                  temp=['plot_traj_cont_lines            =' temp((fpos(1)+1):end)];
                  change=(1>0);	% true   
               else
                  disp(['WARNING ' temp]);                  
      	      end
            end
         end
         % ==== replace 'plot_traj_cont_step' with 'plot_traj_cont_step1'                  
         fpos=findstr(temp,'plot_traj_cont_step');
         if ~isempty(fpos),
            fpos1=[findstr(temp,'plot_traj_cont_step1') findstr(temp,'plot_traj_cont_step2')];
            if isempty(fpos1),
               fpos=findstr(temp,'=');
               if ~isempty(fpos),
                  temp=['plot_traj_cont_step1            =' temp((fpos(1)+1):end)];
	               change=(1>0);	% true                  
               else
                  disp(['WARNING ' temp]);
               end
            end
         end
         
         % ==== find sample type                   
			fpos=findstr(temp,'sample');
         if ~isempty(fpos),
            fpos=findstr(temp,'=');
            samp=sscanf(temp((fpos+1):length(temp)),'%d');
         end
         % ==== find analysis mode if sample is single crystal                    
			fpos=findstr(temp,'analysis_mode');
         if ~isempty(fpos),
            fpos=findstr(temp,'=');
            anal_mode=sscanf(temp((fpos+1):length(temp)),'%d');
         end                     
         % ==== find detector type if sample is single crystal and analysis mode = single crystal                     
			fpos=findstr(temp,'det_type');
         if ~isempty(fpos),
            fpos=findstr(temp,'=');
            det_type=sscanf(temp((fpos+1):length(temp)),'%d');
         end         
               
         % ==== check consistency cut_output list                    
         fpos=findstr(temp,'cut_OutputType'); 
         if ~isempty(fpos),
               fpos=findstr(temp,'=');
               value=sscanf(temp((fpos+1):length(temp)),'%d');
               if isempty(samp)|~isnumeric(samp)|prod(size(samp))~=1|sum(samp==[1 2])~=1,
                  disp(['WARNING Sample type not defined or incorrect. Check .msp file.']);
               	maxvalue=[];   
               elseif samp==2,
                	% powder sample 'none','.cut','.xye','.smh','Mfit .cut' 
                  maxvalue=5;
               elseif samp==1,
                	%  single crystal sample 'none','.cut','.xye','.smh','Mfit .cut', '.hkl' 
                  maxvalue=6;
               end
               if ~isempty(maxvalue)&(value>maxvalue),
               	temp=[temp(1:(fpos+1))  sprintf('%d',1)];
                  change=(1>0);
               end
         end
         
         % ==== check consistency of index for detector trajectory plot and cut_intensity list                    
         fpos=[findstr(temp,'cut_intensity') findstr(temp,'cut_xaxis') findstr(temp,'plot_traj_x') ...
               findstr(temp,'plot_traj_y') findstr(temp,'plot_traj_z') ]; 
         if ~isempty(fpos),
               fpos=findstr(temp,'=');
               value=sscanf(temp((fpos+1):length(temp)),'%d');
               if isempty(samp)|~isnumeric(samp)|prod(size(samp))~=1|sum(samp==[1 2])~=1,
                  disp(['WARNING Sample type not defined or incorrect. Check .msp file.']);
               	maxvalue=[];   
               elseif samp==2,
                	% powder sample Energy,|Q|,2Theta,Az,Det 
                  maxvalue=5;
               elseif (samp==1)&~isempty(anal_mode)&isnumeric(anal_mode)&(anal_mode==2),
                	% powder mode for single crystal sample Q_x,Q_y,Q_z,H,K,L,Energy,|Q|,2Theta,Az,Det 
                  maxvalue=11;
               elseif (samp==1)&~isempty(anal_mode)&isnumeric(anal_mode)&(anal_mode==1)&...
                     ~isempty(det_type)&isnumeric(det_type)&(det_type==1),
                	% single crystal and default PSD mode Qx,Qy,Qz,u1,u2,u3,H,K,L,E,|Q|,2Theta,Az,Det
                  maxvalue=14;
               elseif (samp==1)&~isempty(anal_mode)&isnumeric(anal_mode)&(anal_mode==1)&...
                     ~isempty(det_type)&isnumeric(det_type)&(det_type==2),
                	% single crystal and conventional detectors Qx,Qy,Qz,H,K,L,u1,u2,E,|Q|,2Theta,Az,Det
                 	maxvalue=13;
               else
                  disp(['WARNING Inconsistent sample, analysis mode and detector type. Check .msp file.']);
                  maxvalue=[];
               end
               if (~isempty(findstr(temp,'plot_traj_z'))|~isempty(findstr(temp,'cut_intensity'))|...
                     ~isempty(findstr(temp,'cut_xaxis')))&~isempty(maxvalue),
                  maxvalue=maxvalue+1;
               end
               if ~isempty(maxvalue),
                  if (value>maxvalue),
                     temp=[temp(1:(fpos+1)) sprintf('%d',maxvalue)];
                     change=(1>0);
                  elseif (value<1),
                     temp=[temp(1:(fpos+1)) sprintf('%d',1)];
                     change=(1>0);
                  end
               end
         end

         % ==== check consistency of index to contour lines                    
         fpos=findstr(temp,'plot_traj_cont_lines'); 
         if ~isempty(fpos),
        		fpos=findstr(temp,'=');
            value=sscanf(temp((fpos+1):end),'%d');
            if isempty(value)|(value>2),
	         	temp=[temp(1:(fpos+1)) sprintf('%d',1)];
               change=(1>0);
            end
         end

         % === insert lines for sample type, analysis and detector type for consistency
         unchanged=(1>0);	%true 
         if ~isempty(samp)&(samp==1),
            if ~isempty(findstr(temp,'det_type'))&isempty(anal_mode),
	         	anal_mode=1;
            	temp1='analysis_mode                   = 1';
            	disp(['INSERT ' temp1]);
            	fprintf(ftemp,'%s\n',temp1);
         	end               
         	if ~isempty(findstr(temp,'u11')),
            	if isempty(anal_mode),
	   	      	anal_mode=1;
	            	temp1='analysis_mode                   = 1';
   	         	disp(['INSERT ' temp1]);
         	   	fprintf(ftemp,'%s\n',temp1);
            	end
            	if isempty(det_type),               
				   	det_type=1;
             		temp1='det_type                        = 1';
            		disp(['INSERT ' temp1]);
               	fprintf(ftemp,'%s\n',temp1);
            	end
         	end 
         	if ~isempty(findstr(temp,'u1 '))&isempty(anal_mode),
	   			anal_mode=1;
	         	temp1='analysis_mode                   = 2';
   	      	disp(['INSERT ' temp1]);
         		fprintf(ftemp,'%s\n',temp1);
         	end
         end
         if ~isempty(findstr(temp,'OutputDir'))&isempty(output_type),
            output_type=1;
            temp1='cut_OutputType                  = 1';
            disp(['INSERT ' temp1]);
            fprintf(ftemp,'%s\n',temp1);
        	end
         % === remove lines with 'DefaultMspDir','DefaultMspFile', scrap_files{1 and 2}           
         if ~isempty(findstr(temp,'DefaultMspDir'))|...
               ~isempty(findstr(temp,'DefaultMspFile'))|...
               ~isempty(findstr(temp,scrap_file{1}))|...
               ~isempty(findstr(temp,scrap_file{2})),
            disp(['REMOVE ' temp]);
    			unchanged=(1<0);	%false   
          end
         % === if current line gives a directory path (or path section), test if path1 and path2 strings are to be swapped 
         if exist('path1','var')&exist('path2','var')&~isempty(path2)&ischar(path2), 
            if isempty(path1),	% path1='', insert path2 after the = sign
               fpos=findstr(temp0,'=');
               if ~isempty(fpos),
                  fpos=fpos+1;
               else
                  disp(sprintf('WARNING could not find = character in line \n %s',temp0));
               end
            elseif ischar(path1), 	  
               fpos=findstr(lower(temp0),lower(path1));
            else
               fpos=[];
               disp('WARNING incorrect type for path1. For more info try >>help updatemsp');               
            end
            if ~isempty(fpos),
               if exist('dirtype','var')&~isempty(dirtype)&ischar(dirtype),
                  if ~isempty(findstr(lower(temp0),lower(dirtype))),
                     change=(1>0);
                  end
               elseif ~isempty(findstr(temp0,'DataDir'))|...
      	         	~isempty(findstr(temp0,'PhxDir'))|...
         	      	~isempty(findstr(temp0,'MspDir'))|...
            	   	~isempty(findstr(temp0,'cut_OutputDir')),
                  change=(1>0);
               end
               if change,
                  if ~isempty(path1)&ischar(path1),
                     temp=[temp0(1:(fpos-1)) path2 temp0((fpos+length(path1)):end)];
                  else
                     temp=[temp0(1:(fpos-1)) ' ' path2];
						end                     
					end                  
            end
         end
         if change,
            fprintf(ftemp,'%s\n',temp);
            disp(['REPLACE ' temp0]);
            disp(['WITH    ' temp]);
         	unchanged=(1<0);	%false    
         end
         if unchanged,  
            fprintf(ftemp,'%s\n',temp);            
         end
         temp=fgetl(fid);
      end
      fclose(fid);
      fclose(ftemp);
      %dir(filename);
      %pause;
      delete(filename);
      copyfile(temp_filename,filename);
      delete(temp_filename);
      %pause;
   end
end