function ms_bkg(cmd)

% function ms_bkg(cmd)
% cmd='subtract' subtract stored energy-dependent background from data
% cmd='add' add stored energy-dependent background back to data
% cmd='display' display in Matlab Command line history of stored background
% cmd='delete' deletes the curently stored background

% === if MSlice Control Window non-existent, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window non-existent. Return.')
   return;
end

% === if no background stored in ControlWindow, return
h_bkg=findobj(fig,'Tag','ms_bkg_E');
if isempty(h_bkg)|isempty(get(h_bkg,'Userdata')),
   disp('No background stored in Control Window.');
   return;
end

cmd=lower(cmd(~isspace(cmd)));
if ~strcmp(cmd,'subtract')&~strcmp(cmd,'add')&~strcmp(cmd,'display')&~strcmp(cmd,'delete'),
   disp(['Requested background operation not performed.']);
   disp(['Only background subtraction, addition and history display currently supported.']);
   return;
else
   bkg=get(h_bkg,'UserData');   
   % === display background history in Matlab command line
	if (isfield(bkg,'title')&~isempty(bkg.title))|...
         (isfield(bkg,'datafile')&~isempty(bkg.datafile)&ischar(bkg.datafile)),
      disp(['            ']);
   	disp(['History of stored background : '])
   	if iscell(bkg.title),
      	for i=1:length(bkg.title),
				disp(bkg.title{i});
      	end
   	elseif ischar(bkg.title),
      	disp(bkg.title);
   	end
   	if isfield(bkg,'datafile')&~isempty(bkg.datafile)&ischar(bkg.datafile),
      	disp(['Datafile :' bkg.datafile]);
   	end
	else
   	disp('No history available for the currently stored background.');
	end
   
   if strcmp(cmd,'delete'),
      set(h_bkg,'UserData',[]);
      disp('Currently stored background deleted.');
      return;
   end
   
   % === interpolate background values over the actual energy bins of the data
   % === assume values of bkg.x and of data.en are stored in ascending order
   % === if no data loaded, return
	data=get(fig,'UserData');   
	if isempty(data),
   	disp(['No data stored in ControlWindow']);
   	disp(['Load data first, then subtract/add stored ''background''']);
   	return;
   end
   
   index=(data.en>=bkg.x(1))&(data.en<=bkg.x(end));
 	data.en=data.en(index);
   bkg.y=interp1(bkg.x,bkg.y,data.en,'linear');
   bkg.e=interp1(bkg.x,bkg.e,data.en,'linear');
   bkg.x=data.en;
   format short g;
   disp(['''Background'' data available for the energy range  ' num2str(bkg.x(1)) '  to  ' num2str(bkg.x(end)) ' meV']);
   
   if strcmp(cmd,'subtract'),
	   % === Subtract stored energy-dependent background from data  	  
		disp('Subtracting stored energy-dependent ''background'' from data.');
   	data.S=data.S(:,index)-ones(size(data.det_group))*bkg.y;
   	data.ERR=sqrt(data.ERR(:,index).^2+(ones(size(data.det_group))*bkg.e).^2);
      hh=findobj(fig,'Tag','ms_TitleLabel');
      if ~isempty(hh),
         set(hh,'String',[get(hh,'String') '-bkg(E)']);
      end   
      if isfield(data,'title_label'),
         data.title_label=[data.title_label '-bkg(E)'];   
      end
      set(fig,'UserData',data);
   elseif strcmp(cmd,'add'),
      % === Adding stored energy-dependent background back to data 
      % === in calculating errorbars it assume that this background cut 
      % === was subtracted from the data earlier
		disp('Adding stored energy-dependent ''background'' back to data.');
   	data.S=data.S(:,index)+ones(size(data.det_group))*bkg.y;
   	data.ERR=sqrt(data.ERR(:,index).^2-(ones(size(data.det_group))*bkg.e).^2);
      hh=findobj(fig,'Tag','ms_TitleLabel');
      if ~isempty(hh),
         set(hh,'String',[get(hh,'String') '+bkg(E)']);
      end   
      data.title_label=[data.title_label '+bkg(E)'];   
      set(fig,'UserData',data);
   end   
end