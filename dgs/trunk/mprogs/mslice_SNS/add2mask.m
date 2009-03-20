function add2mask(spec)

% function add2mask(spec)
% add specrea list to current mask

% === return if DetectorView ControlWindow not opened 
h_cw=findobj('Tag','detectorview');
if isempty(h_cw),
   disp(['No DetectorView Control Widow found. Return.']);
   return;
end

% === get existing mask, otherwise create a new one 
h_msk=findobj(h_cw,'Tag','dv_MskFile');
if isempty(h_msk)|~ishandle(h_msk),
   disp('Could not locate stored msk data.');
   return;
end
msk=get(h_msk,'UserData'); % masked spectrum numbers 
number=length(msk); % number of masked spectra already in the list
msk=[msk;spec(:)]; % add recently masked spectra to the overall mask list

% === sort new mask in ascending order of spectrum numbers
msk=sort(msk);

% === eliminate double masking
n=1;
while n<length(msk),
   if msk(n)==msk(n+1),
%     disp(['Eliminate double masking of spec #' num2str(msk(n))]);
      msk(n+1)=[];
   else
      n=n+1;
   end
end

% === store new masked spectrum numbers 
set(h_msk,'UserData',msk); 
disp(sprintf('Added %d spectra (%d new) to existing mask list',length(spec),length(msk)-number));
drawnow;

