function firstzone(x)

data=fromwindow;
if isempty(data),
   disp('Load data first.');
   return;
end

disp(['Folding onto the first zone projections along axis ' num2str(x)]);
if ~isfield(data,'v'),
   disp('Calculate projections first');
   return;
end
k=data.v(:,:,x);
k=mod(k,1);
data.v(:,:,x)=k;
towindow(data);
