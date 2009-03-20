function swapEmodQ

% replace E with |Q|
% Energy should be the last viewing axis (3 PDS mode and 2 in no_PDS mode)

data=fromwindow;
if ~isfield(data,'v')|isempty(data.v),
   disp('Calculate projections first');
   return;
end

disp('Assumes Energy is the last viewing axis.');
disp('Swaps labels and projections with |Q|.');

if size(data.v,3)==3,
   % === single crystal PSD mode
   data.v(:,:,3)=spe2modQ(data);
	data.axis_label=str2mat(deblank(data.axis_label(1,:)),deblank(data.axis_label(2,:)),'|Q|');	
	data.axis_unitlabel=str2mat(deblank(data.axis_unitlabel(1,:)),...
   	deblank(data.axis_unitlabel(2,:)),'(Å^{-1})',...
      deblank(data.axis_unitlabel(4,:)));
else
   % === single crystal conventional (no PDS) mode
   data(:,:,2)=spe2modQ;
	data.axis_label=str2mat(deblank(data.axis_label(1,:)),'|Q|');	
	data.axis_unitlabel=str2mat(deblank(data.axis_unitlabel(1,:)),'(Å^{-1})',...
      deblank(data.axis_unitlabel(3,:)));   
end
towindow(data);
