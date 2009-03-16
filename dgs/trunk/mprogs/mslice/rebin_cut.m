function cut_d=rebin_cut(cut,dx,flag)

% function cut_out=rebin_cut(cut_in,dx,flag)
% rebin existing cut using same pixels but with a different 
% bin width bin_vx=dx or different grid altogether dv=[vx_min vx_max bin_vx]
% non-contributing pixels are eliminated frm final result cut_d
% if flag='plot' then plot rebinned cut

cut_d=cut;
v=cut.pixels(:,4);
if prod(size(dx))==1,
   % === interpret dx=bin_dx, limits default values
	bin_vx=dx;
	vx_min=min(v(:));
   vx_max=max(v(:));
elseif prod(size(dx))==3,
   % === interpret dx=[vx_min vx_max bin_vx]
   bin_vx=dx(3);
   vx_min=dx(1);
   vx_max=dx(2);
	% === eliminate pixels not in the range of the rebinned cut 
	n=floor((vx_max+bin_vx-(vx_min-bin_vx/2))/bin_vx);	% number of vx bins         
   index=(v>=(vx_min-bin_vx/2))&(v<=(vx_min-bin_vx/2+n*bin_vx));
   cut_d.pixels=cut_d.pixels(index,:);
   v=v(index);
else
   disp(sprintf('Indeterminate form of rebinning information.'));
   disp(sprintf('Either dx=bin_vx or dx=[vx_min vx_max bin_vx]'));
   disp(sprintf('It appears dx has %d elements',prod(size(dx))));
end

% === sort contributing pixels according to the bin grid requested
try,
	[cut_d.x,cut_d.npixels,perm]=sort1d_df(v,[vx_min vx_max bin_vx]);
   clear mex sort1d_df   
catch,
   [cut_d.x,cut_d.npixels,perm]=sort1d_m(v,[vx_min vx_max bin_vx]);
end 
cut_d.pixels=cut_d.pixels(perm,:); % permute pixels info

% === determine average intensity  
try,
   [cut_d.y,cut_d.e]=avpix_df(cut_d.pixels(:,5),cut_d.npixels);
catch,
   [cut_d.y,cut_d.e]=avpix_m(cut_d.pixels(:,5),cut_d.npixels);         
end 
      
% === determine average errors
try,
   [cut_d.e,temp]=avpix_df(cut_d.pixels(:,6).^2,cut_d.npixels);
   clear mex avpix_df
catch,
   [cut_d.e,temp]=avpix_m(cut_d.pixels(:,6).^2,cut_d.npixels);
end
cut_d.e=sqrt(cut_d.e)./sqrt(cut_d.npixels);

% === adjust title
cut_d.title=cell(1,2);
titlestr=sprintf('rebin x=%g:%g:%g',vx_min,bin_vx,vx_max);
if iscell(cut.title),
   cut_d.title{1}=cut.title{1};
   cut_d.title{2}=titlestr;
else % cut.title is string char
   cut_d.title={deblank(cut.title),titlestr};
end   

% === plot_cut if required
if exist('flag','var')&~isempty(flag),
   colordef none;
	plot_cut(cut_d);
	hold on;
   ax=axis;
   h=line([ax(1) ax(2)],[0 0]);
	set(h,'Color','y','LineStyle','-');
	h=line([ax(1) ax(2)],mean(cut_d.y)*[1 1]);
	set(h,'Color','r','LineStyle','--');
   hold off;
end

