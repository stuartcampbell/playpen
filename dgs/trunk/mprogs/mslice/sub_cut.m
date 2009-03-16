function cut=sub_cut(cut1,cut2,noebars)

% function cut=sub_cut(cut1,cut2,noebars)
% subtract a 'background' {cut2} from cut1 
% returns structure cut with same fields as cut1
% cut1 is structure with required fields (n=number of data points)
%         x: [1xn double]
%         y: [1xn double]
%         e: [1xn double]
%   npixels: [1xn double]
%    pixels: [Nx6 double], N=sum(npixels(:))
% cut2 is structure with required fields
%         x: [1xm double]
%         y: [1xm double]
%         e: [1xm double] {if this is absent error bars are unchanged under subtraction}
% cut.y=cut1.y-cut2.y{interpolated over the x-values of cut1}
% cut.pixels=cut1.pixels {leave pixel information unchanged}
% cut.e=sqrt((cut1.e).^2+(cut2.e{interpolated...}).^2)
% if noebars='noebars' then cut.e=cut1.e 
% {leave errorbars of cut1 unchanged by the 'background' subtraction}

% === identify if error bars are to be adjusted as well
if (~exist('noebars','var')|isempty(noebars)|~ischar(noebars)|...
      ~strcmp(noebars(~isspace(noebars)),'noebars'))&...
      isfield(cut2,'e')&~isempty(cut2.e)&isnumeric(cut2.e),
   with_ebars=(1>0);	% true DEFAULT VALUE if cut2.e exist
else
   with_ebars=(1<0);	% false
end

n=length(cut1.x); % original bins in cut1
% === identyfy overlapping x-ranges for the two cuts
if ((length(cut1.x)~=length(cut2.x))|~all(cut1.x==cut2.x)),   
   % perform linear interpolation of cut2.x values onto the overlapping range of cut1.x values
   %   disp(['The two cut data sets are not on the same x-grid.'])
   format short g;
   disp(['Interpolate cut2 values ('num2str(length(cut2.x)) ...
         ' points with ' num2str(cut2.x(1)) '<= x <= ' num2str(cut2.x(end)) ') ']);
   disp(['on the grid of cut1 (' num2str(length(cut1.x)) ...
         ' points with ' num2str(cut1.x(1)) '<= x <= ' num2str(cut1.x(end)) ') ']);
   index=((cut1.x>=min(cut2.x))&(cut1.x<=max(cut2.x)));
   m=sum(index); % final points in the subtracted cut
   if m==0,	% === no overlapping points
      disp(['The two cuts have no overlapping x-ranges. Subtraction not performed.']);
      cut=[];
      return;
   else
      % assume the two cuts are sorted in ascending order of x-values 
      y2i=interp1(cut2.x,cut2.y,cut1.x(index),'linear'); % m points
      if with_ebars,	
         e2i=interp1(cut2.x,cut2.e,cut1.x(index),'linear');
      end
   end
else
   m=n; % final points in the subtracted cut (keep all points) 
   % both cuts have the same x-grid
   index=(cut1.x==cut1.x);	% all y-values can be subtracted
   y2i=cut2.y;
   if with_ebars,
      e2i=cut2.e;
   end
end

% === adjust title
cut=cut1;	% transfer all labels and title
if ischar(cut1.title),
   cut.title={cut1.title, ['(' avoidtex(cut1.CutFile) ')-(' avoidtex(cut2.CutFile) ')']};
else % iscell(cut1.title)
   cut.title={cut1.title{1}, ['(' avoidtex(cut1.CutFile) ')-(' avoidtex(cut2.CutFile) ')']};
end   

% === do subtraction
cut.x=cut1.x(index);
cut.y=cut1.y(index)-y2i;
cut.npixels=cut1.npixels(index);
if with_ebars,
   cut.e=sqrt(cut1.e(index).^2+e2i.^2);
else
   cut.e=cut1.e(index);
end

% === do subtraction in the pixels as well and eliminate non-cotributing blocks
pixel_index=[0 cumsum(cut1.npixels(:)')];
cut.pixels=[];
% === build up pixel matrix for the resulting cut
bin_index=0;
for i=1:n,
	if index(i)==1, % keep this bin and the pixels in it
	   bin_index=bin_index+1;
	   pixels=cut1.pixels((pixel_index(i)+1):pixel_index(i+1),:);
   	pixels(:,5)=pixels(:,5)-y2i(bin_index);
   	if with_ebars,
      	pixels(:,6)=sqrt(pixels(:,6).^2+e2i(bin_index)^2);      
   	end   
      cut.pixels=[cut.pixels;pixels];      
   end
end
eliminated=n-m;
pixels=size(cut1.pixels,1)-size(cut.pixels,1);
if eliminated>=1,
   disp(sprintf('Eliminated %d bins of total %d pixels from original cut',eliminated,pixels));
end
   
if with_ebars,
   disp(['Cuts subtracted with error bars adjusted accordingly.']);   
else
   disp(['Cuts subtracted with no errorbar adjustment.']);   
end