function ms_plot_traj

% function ms_plot_traj
% plot detector trajectories picking up parameters from the MSlice ControlWindow
% draw a solid line for each detector   

% === return if no Control Window present or if data not read yet 
fig_cw=findobj('Tag','ms_ControlWindow');
if isempty(fig_cw),
   disp('Control Window not active. Return.');
   return;
end
data=get(fig_cw,'UserData');
if isempty(data),
   disp('Load in data first, then plot detector trajectories');
   return;
end

% ===== read plot parameters from ControlWindow
vars=str2mat('x','vx_min','vx_max','y','vy_min','vy_max','z','vz_min','vz_max');
vars=str2mat(vars,'cont_lines','cont_min','cont_max','cont_step1','cont_step2');
vars=str2mat(vars,'command');
h_samp=findobj(fig_cw,'Tag','ms_sample');
if isempty(h_samp),
   disp(['Not able to identify sample type. Return.']);
   return
end
samp=get(h_samp,'Value');
if samp==1,	% if sample is single crystal, add options about (hkl) points on graphs
   vars=str2mat(vars,'hkl_points','hkl_labels');
end  
for i=1:size(vars,1),
   name=vars(i,:);
   name=name(~isspace(name));
   h=findobj(fig_cw,'Tag',['ms_plot_traj_' name]);
   if isempty(h),
      disp(['Warning: object ms_plot_traj_' name ' not found']);
   end
   if strcmp(get(h,'Style'),'popupmenu')|strcmp(get(h,'Style'),'checkbox'),
      value=num2str(get(h,'Value'));
   else
      value=get(h,'String');
   end
   if ~isempty(value),	    
      if strcmp(name,'command'),		% value should be interpreted as a string
         eval([name '=' ''''  value  '''' ';']);
      else % === value should be interpreted as a number
      	eval([ name '=' value ';']); 
         %	disp([name '=' value ';']);   
      end   
   else
      eval([name '=[];']);
   %  disp([name '=[];']);
   end
end

% === pick up data fields to be displayed 
Q=[];
[vx,axislabelx,unitnamex,unitlengthx,shortlabelx,Q]=pickvar(data,x,Q);
[vy,axislabely,unitnamey,unitlengthy,shortlabely,Q]=pickvar(data,y,Q);
[vz,axislabelz,unitnamez,unitlengthz,shortlabelz,Q]=pickvar(data,z,Q);

% === establish which range of points of each detector is to be plotted
index=(vx==vx);
if ~isempty(vx_min),
   index=index&(vx>=vx_min);   
end
if ~isempty(vx_max),
   index=index&(vx<=vx_max);      
end

if sum(index(:))==0,
   disp(['No points to plot. Return.']);
   return;
end

if ~isempty(vy_min),
   index=index&(vy>=vy_min);   
end
if ~isempty(vy_max),
   index=index&(vy<=vy_max);      
end

if sum(index(:))==0,
   disp(['No points to plot. Return.']);
   return;
end

if ~isempty(vz),	% if vz ~= 'none'
	if ~isempty(vz_min),
   	index=index&(vz>=vz_min);
	end
	if ~isempty(vz_max),
   	index=index&(vz<=vz_max);
   end
end

if sum(index(:))==0,
   disp(['No points to plot. Return.']);
   return;
end

if isempty(vx_min),
   vx_min=min(vx(index));
end
if isempty(vx_max),
   vx_max=max(vx(index));
end
if isempty(vy_min),
   vy_min=min(vy(index));
end
if isempty(vy_max),
   vy_max=max(vy(index));
end

if ~isempty(vz),	% if vz ~= 'none'
	if isempty(vz_min),
   	vz_min=min(vz(index));
	end
	if isempty(vz_max),
   	vz_max=max(vz(index));
   end
end

% === index (ndet,ne) of 0 and 1 to encode which points to plot
[ndet,ne]=size(index);
red_index=reshape([index';(ones(1,ndet)==1)],ndet*(ne+1),1);	% reduced index (ndet*(ne+1),1)
vx=reshape([vx';NaN*ones(1,ndet)],ndet*(ne+1),1);
vy=reshape([vy';NaN*ones(1,ndet)],ndet*(ne+1),1);
vx=vx(red_index);	% retain only pixel points within the plotting range
vy=vy(red_index);

% === draw trajectories in figure with 'tag' property 'plot_traj' 
fig=findobj('Tag','plot_traj');
if isempty(fig),
   %=== if 'plot_traj' figure does not exist, create a new one
   colordef none;	% black background
   % === create a new figure with 'Tag' property 'plot_traj'
   fig=figure('Tag','plot_traj','PaperPositionMode','auto','Name','MSlice : Detector Trajectories');
   % === find any old plot_traj figures and set current figure size to match the dimensions of the 
   % === most recent old figure of the same type (most recent = highest figure number)
   oldfig=findobj('Tag','old_plot_traj'); 
   if ~isempty(oldfig),
      oldfig=sort(oldfig);
      set(fig,'Position',get(oldfig(end),'Position'));
   end
else
   figure(fig);
   % === if not plotting over clear current figure
   if ~ishold,
      clf;
   end
end

% === create menu option to be able to do colour hardcopy printouts   
h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
uimenu(h,'Label','DACColour','Callback','ms_printc('' \\NDAIRETON\DACColour:'');');
uimenu(h,'Label','CRISPColour','Callback','ms_printc('' \\NDAIRETON\CRISPColour:'');');
uimenu(h,'Label','MAPSColour','Callback','ms_printc('' \\NDAIRETON\MAPSColour:'');');   
uimenu(h,'Label','default printer','Callback','ms_printc;');   
% === create menu option to be able to keep figure
h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
% === create menu option to be able to make old slice figures current
h=uimenu(fig,'Label','Make Current','Tag','make_cur','Enable','off');
uimenu(h,'Label','Make Figure Current','Callback','make_cur(gcf);');

% === plot detector trajectories as a line for each detector and 
if 1>0,	% === if this is FALSE do not plot detector trajectory lines, 
   % ========= just add (hkl) point labels and circles on top of existing graph 
	plot(vx,vy,'w-');
   box on;
	axis auto;
	ax=axis;
	if vx_min<vx_max,
   	axis([vx_min vx_max ax(3) ax(4)]);
   	ax(1:2)=[vx_min vx_max];
	end
	if vy_min<vy_max,
   	axis([ax(1) ax(2) vy_min vy_max]);
	end
end

% === establish axis labels and title
xlabel([axislabelx unitnamex]);
ylabel([axislabely unitnamey]);
form='%7.5g';
titlestr=[avoidtex(data.filename)];
if data.emode==1,
   titlestr=[titlestr ', E_{i} = ' num2str(data.efixed,form) ' meV'];
else
   titlestr=[titlestr ', E_{f} = ' num2str(data.efixed,form) ' meV'];
end   

% === if sample is single crystal add sample orientation to the title
if samp==1,	
   titlestr=[titlestr ', Psi = ' num2str(data.psi_samp*180/pi,form)];
end
if ~isempty(vz),
	titlestr=[titlestr ', ' num2str(vz_min,form) '<' shortlabelz '<' num2str(vz_max,form)];
end
if cont_lines>=2,
   % === put in title info about contour lines    
   titlestr=[titlestr ', Contours ' ms_getstring(fig_cw,'ms_plot_traj_cont_lines') '=' num2str(cont_min,form) ':' num2str(cont_step1,form) ];
   if ~isempty(cont_step2)&isnumeric(cont_step2),
      titlestr=[titlestr '[' num2str(cont_step2,form) ']'];
   end
   titlestr=[titlestr ':' num2str(cont_max,form)];
end
title(titlestr);

%=== Impose aspect ration 1:1 if vx and vy axes are in the same units
if strcmp(deblank(unitnamex),deblank(unitnamey)),	% same lengths along x and y
   set(gca,'DataAspectRatioMode','manual');
   a=get(gca,'DataAspectRatio');
   set(gca,'DataAspectRatio',[min(a(1:2))./[unitlengthx unitlengthy] a(3)]);
   set(gcf,'PaperPositionMode','manual');
end

% === put (h,k,l) labels if the two plotting directions are wavevectors axes
h=findobj(fig_cw,'Tag','ms_plot_traj_x');
strings=get(h,'String');
if exist('hkl_points','var')&(hkl_points==1)&...
      (sum(x==[1:(length(strings)-5)])==1)&(sum(y==[1:(length(strings)-5)])==1),	
   Q=spe2sqe(data);
   Qx=Q(:,:,1);
   Qy=Q(:,:,2);
   Qz=Q(:,:,3);	% Qz(ndet,ne)
   Q=cat(2,Qx(index),Qy(index),Qz(index));	% Q(n,3)
   Q=sqe2samp(Q,data.psi_samp);
   hkl=q2rlu(Q,data.ar,data.br,data.cr);
   [h,k,l]=meshgrid(round(min(hkl(:,1))):1:round(max(hkl(:,1))),...
      round(min(hkl(:,2))):1:round(max(hkl(:,2))),...
      round(min(hkl(:,3))):1:round(max(hkl(:,3))));
   hkl=[h(:)';k(:)';l(:)']';
   vx=pickvar_hkl(data,hkl,x);
   index=(h(:)==h(:));
   if ~isempty(vx),
      index=index&(vx>=vx_min)&(vx<=vx_max);
   	vy=pickvar_hkl(data,hkl,y);
   	if ~isempty(vy),
      	index=index&(vy>=vy_min)&(vy<=vy_max);
  	 		hold on;
   		if sum(index)~=0,
	   		plot(vx(index),vy(index),'mo');
				if hkl_labels==1,
      			no_overlap(text(vx(index),vy(index),miller(hkl(index,:))));
      		end
   		end
      	hold off;
      end
   end
end

% === draw contour lines if required
if cont_lines==2,
   % === draw contour energy lines
   n=length(strings)-5;	% number of wavevector directions Qx, Qy, Qz, H, K, L, u1, u2, (u3 if PSD),
   % === transform wavevectors directions in the sample reference frame
   u1=pick_wv(x,n,data.psi_samp,data.u,data.ar,data.br,data.cr);
   u2=pick_wv(y,n,data.psi_samp,data.u,data.ar,data.br,data.cr);
   if ~isempty(u1)&~isempty(u2),
      uu1=u1/norm(u1);
      uu2=u2/norm(u2);
      % === if angle between u1 and u2 are more than 1 deg away from 90 deg then warn that axes are not orthogonal 
		if abs(dot(uu1,uu2))>=cos(pi/2*(1-1/90)),	   
         disp(['Wavevector axes along x and y are not orthogonal. Angle is ' num2str(acos(dot(uu1,uu2))*180/pi,form)]);
         return;
      end
      uu3=cross(uu1,uu2);
      uu3=uu3/norm(uu3);
      
      % === different algorithms for direct/indirect geometry
      if data.emode==1,
      	% === transform ki in the sample reference frame
      	ki=sqe2samp(sqrt(data.efixed/2.07)*[1 0 0],data.psi_samp);
      	% === transform ki into the (u1,u2,u3) reference frame
      	ki1=uu1(1)*ki(1)+uu1(2)*ki(2)+uu1(3)*ki(3);
      	ki2=uu2(1)*ki(1)+uu2(2)*ki(2)+uu2(3)*ki(3);
      	ki3=uu3(1)*ki(1)+uu3(2)*ki(2)+uu3(3)*ki(3);
      	figure(fig);
      	hold on;
      	% === draw contour lines one by one as circles
      	for E=cont_min:cont_step1:cont_max,
         	if data.efixed>=E,
	         	kf=sqrt((data.efixed-E)/2.07);
   	      	if kf>ki3,
	   	      	q=sqrt(kf^2-ki3^2);
         			th=linspace(0,2*pi,201);
   	            h=plot((q*cos(th)+ki1)/norm(u1),(q*sin(th)+ki2)/norm(u2),'w-');
      	         % === draw thick lines at major steps
         	      if ~isempty(cont_step2)&isnumeric(cont_step2),
            	      if mod(E-cont_min,cont_step2)==0,
               	      set(h,'LineWidth',3);
                  	end
               	end
            	end
         	end
      	end
      	hold off;
      else % indirect geometry
         th=linspace(min(data.det_theta),max(data.det_theta),length(data.det_theta))';
         kf=sqrt(data.efixed/2.07);
        	figure(fig);
      	hold on;
         for E=cont_min:cont_step1:cont_max,
 	      	ki=sqrt((E+data.efixed)/2.07);
            Q=ones(length(th),1)*[ki 0 0]-[kf*cos(th) kf*sin(th) 0*th]; % (ntheta,3)
            Q=sqe2samp(Q,data.psi_samp);
            q1=Q(:,1)*uu1(1)+Q(:,2)*uu1(2)+Q(:,3)*uu1(3);
            q2=Q(:,1)*uu2(1)+Q(:,2)*uu2(2)+Q(:,3)*uu2(3);            
            h=plot(q1/norm(u1),q2/norm(u2),'w-');
      	     % === draw thick lines at major steps
         	  if ~isempty(cont_step2)&isnumeric(cont_step2),
                  if mod(E-cont_min,cont_step2)==0,
              	      set(h,'LineWidth',3);
                 	end
              end
         end
         hold off;  
      end
   else
      disp('Only drawing contour energy lines in single crystal mode ');
      disp(' and when both axes are wavevector directions.');
   end   
end
% === if command is given (for example to draw the dispersion relation on top of 
% === the detector trajectories plot) then execute command
if ~isempty(command)&~isempty(command(~isspace(command))),
   evalin('base',command,'disp([''Cannot execute command  '' command ''. Check path.''])');
end   
set(gcf,'PaperPositionMode','auto');   
