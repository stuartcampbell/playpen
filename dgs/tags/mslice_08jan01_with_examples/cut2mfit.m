function cut2mfit(cut_data)

% function cut2mfit(cut_data)
% send cut data directly to MFit 

% === get handles of Mfit ControlWindow and DataWindow 

a=which('mf_figs');
if isempty(a),
   disp('Mfit not installed. Return.');
	return;   
end   
[hmf_ctrl,hmf_data]=mf_figs;
if isempty(hmf_ctrl)|(hmf_ctrl==0),
   disp(['Start MFit, then send cut data.']);
   return;
end

% === extract x,y,err from cut_data structure  
x=cut_data.x;	% (npts,1) line vector
y=cut_data.y;
err=cut_data.e;

% === Sort data in ascending order of x-values
i=find(~isnan(y) & ~isinf(y));
x=x(i);
y=y(i);
err=err(i);
[x, i]=sort(x);
y=y(i);
err=err(i);

%=== Eliminate data with zero error bar 
i=find(err==0);
if ~isempty(i)
	mf_msg('Data with zero error eliminated');
	xs = x; ys = y; 
	x(i)=[];
	y(i)=[];
	err(i)=[];
	if isempty(x)
	   mf_msg('Data has zero error');
	   disp('Data has zero error. Assign small error bars.');
	   x=xs; y=ys;
	   err=1e-3*max(abs(y))*ones(size(y));       % equal error bars - so equal weights
  	end
end
  
%=== Make MFit data window if not present, and delete previous graph if window already present  
mf_msg('Plotting data');
if (~isempty(hmf_data) & hmf_data)
	delete(findobj(hmf_data,'type','text'));
	delete(findobj(hmf_data,'type','line'));
end
hmf_data=mf_dwin([],[]);

%---------- Attach data to userdata ------------------
[n,c]=size(x);
if c>n x=x'; end
[n,c]=size(y);
if c>n y=y'; end
[n,c]=size(err);
if c>n err=err'; end
selected=ones(size(y));
set(hmf_data,'userdata',[x y err selected ]);

%----------- Do the plot ------------------------------
figure(hmf_data);
if isfield(cut_data,'axis')&~isempty(cut_data.axis)&isnumeric(cut_data.axis),
   axis(cut_data.axis);
else
   xmin=min(cut_data.x);
   xmax=max(cut_data.x);
   ymin=min(cut_data.y-cut_data.e);
   ymax=max(cut_data.y+cut_data.e);   
   axis([xmin-0.1*(xmax-xmin) xmax+0.1*(xmax-xmin) ymin ymax]);
end   
mf_uplot('all');
hmf_data=mf_dwin(cut_data.x_label,cut_data.y_label);


set(findobj(hmf_data,'Tag','mf_text_xlabel'),'String',cut_data.x_label,'FontSize',11);
set(findobj(hmf_data,'Tag','mf_text_ylabel'),'String',cut_data.y_label,'FontSize',11);
if isfield(cut_data,'title')&~isempty(cut_data.title),
   if iscell(cut_data.title),
      cut_title=cut_data.title{1};
      for i=2:length(cut_data.title),
         cut_title=str2mat(cut_title,cut_data.title{i});
      end
   elseif ischar(cut_data.title),
      cut_title=cut_data.title;
   else
      cut_title='unknown';
   end
else
   cut_title='unknown';
end
set(findobj(hmf_data,'Tag','mf_text_title'),'String',cut_title,'FontSize',11);

