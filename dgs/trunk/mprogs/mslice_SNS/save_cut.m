function save_cut(cut,filename,option)

% function save_cut(cut,filename,option)
% function to save the complete cut data in <filename>
% cut has required fields (n=number of data points)
% filename='cut\spe750_12s_apr98_all_nesw_cut_i_a.cut'
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
% for 'cut' format(default) extra fields
%   npixels: [1xn double]
%    pixels: [mx6 double] m=sum(npixels(:))
% for 'smh' format extra fields
% 	efixed	: number	(meV)
% 	emin	: number	(meV)
% 	psi_samp: number (deg)
% 	h,k,l,energy : [1xn double] 
% 	det_index, en_index : [mx1 double]
% option='cut' {default, saves in .cut format with full pixel information for tobyfit} 
%       ='smh'	{saves in .smh format with partial pixel information for smhfit}
%		  ='xye' {x,y,error ascii 3-column format}
%       ='Mfit .cut' {saves in .cut format with full pixel information, also appends x-label, y-label and title, if present}

% === return if cut is empty
if isempty(cut),
   disp('Cut is empty. Cut not saved. Return.');
   return
end

% === open <filename> for writing
fid=fopen(filename,'wt');
if fid==-1,
   disp([ 'Error opening file ' filename ]);
   fclose(fid);
   return
end

% ===========================================================================================   
if ~exist('option','var')|isempty(option)|~ischar(option)|~isempty(findstr(option,'cut')),
% ===========================================================================================      
   % === default saving option is 'cut' format
   % === check presence of required fields for the cut data structure
   if ~isfield(cut,'x')|~isfield(cut,'y')|~isfield(cut,'e')|~isfield(cut,'npixels')|...
         ~isfield(cut,'pixels'),
      disp(['Do not have enough information to save cut in .cut format']);
      fclose(fid);
      return;
   end
   
   % === save as a 'cut' file with full pixel information {default option}   
   disp(['Saving cut ( ' num2str(length(cut.x)) ' point(s) and ' num2str(size(cut.pixels,1)) ' pixel(s)) in .cut format to file : ']);
   disp([filename]);
	drawnow;
	n=length(cut.x);
   fprintf(fid,'%5d\n',n);
   index=[0 cumsum(cut.npixels(:)')];
   for i=1:n,
   	fprintf(fid,'%14.5f%17.5f%17.5f%12d\n',cut.x(i),cut.y(i),cut.e(i),cut.npixels(i));
      fprintf(fid,'%9d%10.4f%12.4f%13.5f%13.5f%13.5f\n',cut.pixels((1+index(i)):index(i+1),:)');
   end    
   % === if save option is 'Mfit .cut' then append labels and cut parameters + lattice parameters if single crystal
   if ~isempty(findstr(lower(option),'mfit')),
      % === return if insufficient parameters
      if ~isfield(cut,'x_label')|~isfield(cut,'y_label')|~isfield(cut,'title')|~isfield(cut,'efixed')|...
         ~isfield(cut,'emode')|~isfield(cut,'sample')|...
         ~isfield(cut,'MspDir')|~isfield(cut,'MspFile'),
         disp(['Do not have enough information to append labels at end of .cut file']);
      	fclose(fid);
   		return;   
   	end
   	if cut.sample==1&(~isfield(cut,'abc')|~isfield(cut,'uv')|~isfield(cut,'psi_samp')),
      		disp(['Do not have enough lattice parameter information to append at end of .cut file']);
      		fclose(fid);
            return;  
   	end   
   
   	% === write x_label, y_label, and title
   	header='efixed                          =';	% typical header
   	len=num2str(length(header)-1);	% length of defining field
   	if ischar(cut.x_label),
	   	fprintf(fid,['%-' len 's%2s%-s\n'],'x_label','= ',cut.x_label);
   	elseif iscell(cut.x_label),
      	for i=1:length(cut.x_label),
		   	fprintf(fid,['%-' len 's%2s%-s\n'],'x_label','= ',cut.x_label{i});
      	end
   	end
   	if ischar(cut.y_label),
	   	fprintf(fid,['%-' len 's%2s%-s\n'],'y_label','= ',cut.y_label);
   	elseif iscell(cut.y_label),
      	for i=1:length(cut.y_label),
		   	fprintf(fid,['%-' len 's%2s%-s\n'],'y_label','= ',cut.y_label{i});
      	end
   	end
   	if ischar(cut.title),
	   	fprintf(fid,['%-' len 's%2s%-s\n'],'title','= ',cut.title);
   	elseif iscell(cut.title),
      	for i=1:length(cut.title),
		   	fprintf(fid,['%-' len 's%2s%-s\n'],'title','= ',cut.title{i});
      	end
   	end
   
   	% === write MspDir, MspFile 
   	fprintf(fid,['%-' len 's%2s%-s\n'],'MspDir','= ',cut.MspDir);
   	fprintf(fid,['%-' len 's%2s%-s\n'],'MspFile','= ',cut.MspFile);
   
   	% === write efixed, emode and sample, distinguish if info comes in as strings or as values
      if ischar(cut.efixed),
         efixed=str2num(cut.efixed);
      else
         efixed=cut.efixed;
      end
      if ischar(cut.emode),
         emode=str2num(cut.emode);
      else
         emode=cut.emode;
      end
      if ischar(cut.sample),
         sample=str2num(cut.sample);
      else
         sample=cut.sample;
      end

      fprintf(fid,['%-' len 's%2s%-7.5g\n'],'efixed','= ',efixed);
   	fprintf(fid,['%-' len 's%2s%-7.5g\n'],'emode','= ',emode);
   	fprintf(fid,['%-' len 's%2s%-7.5g\n'],'sample','= ',sample);
   
   	% ==== is sample is single crystal write lattice parameters and crystal orientation
      if sample==1,
         if isfield(cut,'abc')&isfield(cut,'uv'),
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'as','= ',cut.abc(1,1));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'bs','= ',cut.abc(1,2));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'cs','= ',cut.abc(1,3));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'aa','= ',cut.abc(2,1));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'bb','= ',cut.abc(2,2));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'cc','= ',cut.abc(2,3));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'ux','= ',cut.uv(1,1));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'uy','= ',cut.uv(1,2));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'uz','= ',cut.uv(1,3));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'vx','= ',cut.uv(2,1));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'vy','= ',cut.uv(2,2));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'vz','= ',cut.uv(2,3));
            fprintf(fid,['%-' len 's%2s%-7.5g\n'],'psi_samp','= ',cut.psi_samp*180/pi);
         elseif isfield(cut,'as')&isfield(cut,'bs')&isfield(cut,'cs')&...
               isfield(cut,'aa')&isfield(cut,'bb')&isfield(cut,'cc')&...
               isfield(cut,'ux')&isfield(cut,'uy')&isfield(cut,'uz')&...
               isfield(cut,'vx')&isfield(cut,'vy')&isfield(cut,'vz'),
            fprintf(fid,['%-' len 's%2s%s\n'],'as','= ',cut.as);
            fprintf(fid,['%-' len 's%2s%s\n'],'bs','= ',cut.bs);
            fprintf(fid,['%-' len 's%2s%s\n'],'cs','= ',cut.cs);
            fprintf(fid,['%-' len 's%2s%s\n'],'aa','= ',cut.aa);
            fprintf(fid,['%-' len 's%2s%s\n'],'bb','= ',cut.bb);
            fprintf(fid,['%-' len 's%2s%s\n'],'cc','= ',cut.cc);
            fprintf(fid,['%-' len 's%2s%s\n'],'ux','= ',cut.ux);
            fprintf(fid,['%-' len 's%2s%s\n'],'uy','= ',cut.uy);
            fprintf(fid,['%-' len 's%2s%s\n'],'uz','= ',cut.uz);
            fprintf(fid,['%-' len 's%2s%s\n'],'vx','= ',cut.vx);
            fprintf(fid,['%-' len 's%2s%s\n'],'vy','= ',cut.vy);
            fprintf(fid,['%-' len 's%2s%s\n'],'vz','= ',cut.vz);
            fprintf(fid,['%-' len 's%2s%s\n'],'psi_samp','= ',cut.psi_samp);               
         end
    	end  % if sample==1
      disp(['Appended parameter information at end of cut file.']);
   end % append label information
   fclose(fid);
   % ================================================================         
elseif ~isempty(findstr(option,'smh')),
% ================================================================      
   % === check presence of required fileds of the cut data structure
   if ~isfield(cut,'x')|~isfield(cut,'y')|~isfield(cut,'e')|~isfield(cut,'efixed')|...
         ~isfield(cut,'emin')|~isfield(cut,'ebin')|...
         ~isfield(cut,'det_index')|~isfield(cut,'en_index')|~isfield(cut,'npixels')|...
         ~isfield(cut,'h')|~isfield(cut,'k')|~isfield(cut,'l')|~isfield(cut,'energy'),
      disp(['Do not have enough information to save cut in .smh format']);
      fclose(fid);
   	return;   
   end
   % === save as a '.smh' file with full pixel information
   disp(['Saving cut ( ' num2str(length(cut.x)) ' point(s) and ' num2str(length(cut.det_index(:))) ' pixel(s)) in .smh format to file : ']);
   disp([filename]);
	drawnow;
	n=length(cut.x);
   fprintf(fid,'%4d%10.3f%9.3f%9.3f%9.3f%9.3f%9.3f\n',n,cut.efixed,cut.emin,cut.ebin,cut.psi_samp,0,0);
   index=[0 cumsum(cut.npixels(:)')];
   for i=1:n,
      fprintf(fid,'%3d%9.3f%9.3f%9.3f%9.3f%10.5f%14.5f%8d\n',...
         1,cut.h(i),cut.k(i),cut.l(i),cut.energy(i),cut.y(i),cut.e(i),cut.npixels(i));
      temp=[cut.det_index((1+index(i)):index(i+1))'; cut.en_index((1+index(i)):index(i+1))']';
      % fprintf(fid,'%1s',' ');
      fprintf(fid,'%6d%4d%6d%4d%6d%4d%6d%4d%6d%4d%6d%4d%6d%4d%6d%4d\n',temp');
      if rem(length(temp(:)),16)~=0,
         fprintf(fid,'\n');
		end
	end    
   fclose(fid);   
% ================================================================      
elseif ~isempty(findstr(option,'hkl')),
% ================================================================      
   % === check presence of required fileds of the cut data structure
   if ~isfield(cut,'x')|~isfield(cut,'y')|~isfield(cut,'e')|...
         ~isfield(cut,'h')|~isfield(cut,'k')|~isfield(cut,'l')|~isfield(cut,'energy'),
      disp(['Do not have enough information to save cut in .hkl format']);
      fclose(fid);
   	return;   
   end
   % === save as an '.hkl' file 
   disp(['Saving cut ( ' num2str(length(cut.x)) ' point(s)) in .hkl format to file : ']);
   disp([filename]);
	drawnow;
   
   % === save x-label, if present
   if isfield(cut,'x_label')&~isempty(cut.x_label)&(ischar(cut.x_label)|iscell(cut.x_label)),
      if iscell(cut.x_label),
	      for i=1:length(cut.x_label),
   	      fprintf(fid,'%%x_label= %s\n',cut.x_label{i});
      	end
      elseif ischar(cut.x_label),
	      fprintf(fid,'%%x_label= %s\n',cut.x_label);
		end            
   else
      disp('No x-label information available.');
   end
   
   % === save y-label, if present
   if isfield(cut,'y_label')&~isempty(cut.y_label)&(ischar(cut.y_label)|iscell(cut.y_label)),
 	   if iscell(cut.y_label),
	      for i=1:length(cut.y_label),
   	      fprintf(fid,'%%y_label= %s\n',cut.y_label{i});
      	end
   	elseif ischar(cut.y_label),
	      fprintf(fid,'%%y_label= %s\n',cut.y_label);
		end            
   else
      disp('No-y label information available.');
   end
   % === save title, if present
   if isfield(cut,'title')&~isempty(cut.title)&(ischar(cut.title)|iscell(cut.title)),
	   if iscell(cut.title),
	      for i=1:length(cut.title),
   	      fprintf(fid,'%%title= %s\n',cut.title{i});
      	end
   	elseif ischar(cut.title),
      	fprintf(fid,'%%title= %s\n',cut.title);
		end            
   else
      disp('No title information available');
   end

   fprintf(fid,'%12s %12s %12s %12s %12s %12s %12s\n','H(rlu)','K(rlu)','L(rlu)','Energy(meV)','x','y','error');
   d=[cut.h(:)';cut.k(:)';cut.l(:)';cut.energy(:)';cut.x(:)';cut.y(:)';cut.e(:)']';
   % === remove numbers of order e-16 or smaller
   index=(abs(d)<=10*eps);
   d(index)=0;
   fprintf(fid,'%12.5g %12.5g %12.5g %12.5g %12.5g %12.5g %12.5g\n',d');
   fclose(fid);      
% ================================================================   
elseif ~isempty(findstr(option,'xye')),
% ================================================================   
   % === check presence of required fileds of the cut data structure
   if ~isfield(cut,'x')|~isfield(cut,'y'),
      disp(['Do not have enough information to save cut in .xy or .xye format']);
   	return;   
   end      
   % === save as an 'xye' file 
   if isfield(cut,'e')&~isempty(cut.e)&isnumeric(cut.e),
      disp(['Saving cut ( ' num2str(length(cut.x)) ' point(s)) in .xye format to file : ' ]);
      disp([filename]);
      d=[cut.x(:)';cut.y(:)';cut.e(:)']';
      fprintf(fid,'%14.5g %14.5g %14.5g\n',d');
   else	% save as xy only
      disp(['Saving cut data as an xy file ( ' num2str(length(cut.x)) ' point(s)) to : ' ]);
      disp([filename]);
      d=[cut.x(:)';cut.y(:)']';
      fprintf(fid,'%14.5g %14.5g\n',d');
	end
   fclose(fid);
else
   disp(['Unknown file format ' option]);
	return;   
end   
disp('--------------------------------------------------------------');

