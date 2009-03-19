function plot_det(data,det_index,e_min,e_max,bin_e,i_range,out_filename)

% function plot_det(data,det_index,e_min,e_max,bin_e,i_range,out_filename)
% plot data in the det_index'th detector (default=1)
% if out_filename present save results in an ASCII file (x,y,error)

%=== if 'plot_det' figure does not exist, create
fig=findobj('Tag','plot_det');
if isempty(fig),
   figure;
	set(gcf,'Tag','plot_det');   
else
   figure(fig);
end

% === establish which detectors to plot
x=data.en;
if ~exist('det_index','var')|(exist('det_index','var')&(isempty(det_index))),
   det_index=1;
else
   index=((det_index>=1)&(det_index<=length(data.det_group)));
   det_index=det_index(index);
   if length(det_index)==0,
	   disp(['Detector index ' num2str(det_index) ' out of range. Plot not performed.'])
   	return   
   end
end

% === do the average of nearby detectors 
if length(det_index)>=2,
	y=sum(data.S(det_index,:),1)/length(det_index);
	err=sqrt(sum(data.ERR(det_index,:).^2,1))/length(det_index);
else
   y=data.S(det_index,:);
   err=data.ERR(det_index,:);
end

% === rebin data along the energy axis
vx=e_min:bin_e:e_max;
n=length(vx);
intensity=NaN*ones(size(vx));
error_int=intensity;
for i=1:n,
   index=((x>=(e_min+(i-1-1/2)*bin_e))&(x<(e_min+(i-1+1/2)*bin_e))); 
	number=sum(index);	% number of pixels to contribute to the intensity in bin 'i'
   if number>=1, 
     	vx(i)=mean(x(index));	% simple average of pixel positions
      intensity(i)=mean(y(index));	% simple average of pixel intensities
      error_int(i)=norm(err(index))/number;   
   end      
end   

% === choose plotting symbol
if isfield(data,'symbol'),
   symbol=data.symbol;
else
   symbol='wo';
end

% === plot cut 
errorbar(vx,intensity,error_int,symbol);

% === establish axis labels, title and axis limits
xlabel('Energy (mev)');
if isfield(data,'axis_unit'),
   ylabel(data.axis_unit(4,:));
else
	ylabel('Intensity (arb. units)');
end;
if length(det_index)<5,
	title([avoidtex(data.filename) ',  '  'Det. group no ' num2str(det_index)]);
else
   title([avoidtex(data.filename) ',  ' 'Det. group no ' num2str(min(det_index(:))) ':' num2str(max(det_index(:)))]);
end   
if exist('i_range','var')&(~isempty(i_range)),
   axis([e_min e_max i_range]);
else
   axis auto;
   ax=axis;
   axis([e_min e_max ax(3) ax(4)]);
end

% === save cut as an xye ASCII file, if out_filename given
if exist('out_filename','var'),
   d=[vx ; intensity ; error_int]';
   eval(['save ' out_filename ' d -ascii']);
	disp(['Saved plot as an (x,y,err) column ASCII format to file ' out_filename ]);   
end   