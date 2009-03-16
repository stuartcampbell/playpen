function cut1=interp_cut(cut)

% assume sorted in ascending order of x values

cut1=cut;
cut1.x=(cut.x(2:end)+cut.x(1:(end-1)))/2; % n-1 points
cut1.y=(cut.y(2:end)+cut.y(1:(end-1)))/2;
cut1.e=sqrt(cut.e(2:end).^2+cut.e(1:(end-1)).^2)/2;

% === append pixels
cut1.npixels=cut.npixels(2:end)+cut.npixels(1:(end-1)); % n-1 bins
cut1.pixels=[];
index=[0 cumsum(cut.npixels)];
n=length(cut.x);
for i=1:(n-1),
   cut1.pixels=[cut1.pixels; cut.pixels((index(i)+1):(index(i)+cut.npixels(i)),:);...
         cut.pixels((index(i+1)+1):(index(i+1)+cut.npixels(i+1)),:)];
end

%cut1.symbol='rs';
plot_cut(cut1);
hold on;
ax=axis;
h=line([ax(1) ax(2)],[0 0]);
set(h,'Color','y','LineStyle','-');
h=line([ax(1) ax(2)],mean(cut.y)*[1 1]);
set(h,'Color','r','LineStyle','--');
