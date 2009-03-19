function plot_sum(spec_data,name,i_min,i_max)

% function plot_sum(spec_data,name,i_min,i_max)
% spec_data=[spec_number intensity error] (nspectra,3)

if isempty(spec_data),
   disp(sprintf('No sum data to plot. Return.'));
   return;
end

% === construct matrix with intensities for the WEST PSD low-angle bank on HET
tubes=13;   % number of PSD tubes in the bank
channels=64;% number of independent detectpr elements along the tube
l0=4020;		% mm  sample-detector plane distance
l1=175;		% mm  
l2=27;		% mm  distance between adjacent detectors
l3=914;		% mm	tube length
s1=401;	% first spectrum number in the bank
s2=s1-1+channels*tubes;	% last spectrum number in the bank
% === put data in matrix form and 
% === account for the numbering of detectors starting in bottom left-hand corner
[x_w,y_w,int_w,err_w]=put_in_matrix(spec_data,s1,s2,tubes,channels,-l1-l2*tubes,-l3/2,l2,l3/channels,'fliplr');

% === construct matrix with intensities for the EAST PSD low-angle bank on HET
s1=1233;	% first spectrum number in the bank
s2=s1-1+channels*tubes;	% last spectrum number in the bank
% === put data in matrix form and 
[x_e,y_e,int_e,err_e]=put_in_matrix(spec_data,s1,s2,tubes,channels,l1,-l3/2,l2,l3/channels);

% === construct matrix with intensities for the NORTH PSD low-angle bank on HET
channels_ns=26;	% channels along a tube in the N & S PSD 
tubes_ns=10;
l1_ns=-l2*tubes_ns/2;
l3_ns=channels_ns/channels*l3;	% make vertical length for one detector element the same as for the E&W PSD
s1=2065;	% first spectrum number in the bank
s2=s1-1+channels_ns*tubes_ns;	% last spectrum number in the bank
% === put data in matrix form and 
[x_n,y_n,int_n,err_n]=put_in_matrix(spec_data,s1,s2,tubes_ns,channels_ns,l1_ns,l3/2-l3_ns,l2,l3_ns/channels_ns);


% === construct matrix with intensities for the SOUTH PSD low-angle bank on HET
s1=2325;	% first spectrum number in the bank
s2=s1-1+channels_ns*tubes_ns;	% last spectrum number in the bank
% === put data in matrix form and 
[x_s,y_s,int_s,err_s]=put_in_matrix(spec_data,s1,s2,tubes_ns,channels_ns,l1_ns,-l3/2,l2,l3_ns/channels_ns,'flipud');

% === establish axes limits
if ~exist('i_min','var')|isempty(i_min)|~isnumeric(i_min)|(prod(size(i_min))>1),
   i_min=0;
end 
if ~(exist('i_max','var')&~isempty(i_max)&isnumeric(i_max)&prod(size(i_max))>=1),
   i_ew=max([int_e(~isnan(int_e)); int_w(~isnan(int_w))]);
   i_ns=max([int_n(~isnan(int_n)); int_s(~isnan(int_s))]);
   if i_ns<i_ew,
      if i_ns>i_min, 
         i_max=i_ns;
      else
         i_max=i_ew;
      end
   else
      i_max=i_ew;
   end
end 
if i_max<=i_min,
   disp('Can not plot data if i_max<=i_min. Return.');
   return;
end

% === plot intensity colour map over the detector positions 
fig=findobj('Tag','det_view');
if isempty(fig),	% new figure
 	colordef none;	% black background
   % === create a new figure with 'Tag' property 'detector_map'
   fig=figure('Name','DetectorView: Plot Sum','Tag','det_view','PaperPositionMode','auto');	
   % === find any old figures and set current figure size to match the dimensions of the 
   % === last old figure of the same type
   oldfig=findobj('Tag','old_detector_map'); 
   if ~isempty(oldfig),
      oldfig=sort(oldfig);
     	set(fig,'Position',get(oldfig(end),'Position'));
   end
   % === define 'UserData' field of current figure     
   set(fig,'UserData',[]);
   % === create menu option to be able to do colour hardcopy 
   h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
   uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
   uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
   uimenu(h,'Label','default printer','Callback','ms_printc;');   
   % === create menu option to be able to keep figure
   h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
   uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
   % === create menu option to be able to make old plot cut figures current
   h=uimenu(fig,'Label','Make Current','Tag','make_cur','Enable','off');
   uimenu(h,'Label','Make Figure Current','Callback','make_cur(gcf);');
end
figure(fig);
hold off;
box on;
load coltab.dat -ascii;
colormap(coltab);
pcolor(x_w,y_w,int_w);
hold on;

pcolor(x_e,y_e,int_e);
pcolor(x_n,y_n,int_n);
pcolor(x_s,y_s,int_s);
hold off;
caxis([i_min i_max]);
colorbar;
shading faceted;
axis([-l1-l2*tubes l1+tubes*l2 -l3/2 l3/2]);
aa=get(gca,'DataAspectRatio');
set(gca,'DataAspectRatioMode','manual','DataAspectRatio',[max(aa(1:2)) max(aa(1:2)) aa(3)]);
xlabel('Horizontal Direction (mm)');
ylabel('Vertical Direction (mm)');
h=title(name);
YLim=get(gca,'YLim');
pos=get(h,'Position');
set(h,'Position',[pos(1) YLim(2) pos(3)]);