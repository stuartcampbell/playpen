function dv_mask

% function dv_mask
% mask spectra with intensity below a given threshold value

% === return if DetectorView ControlWindow not opened 
h_cw=findobj('Tag','detectorview');
if isempty(h_cw),
   disp(['No DetectorView Control Widow found. Return.']);
   return;
end

% === get spectra map and intensity threshold value
spectramap=ms_getstring(h_cw,'dv_spectramap'); 
threshold=ms_getstring(h_cw,'dv_threshold'); 
if isempty(threshold),
   disp('Specify threshold first.');
   return;
end
threshold=eval(threshold);

% === get intensity data 
h_sum=findobj(h_cw,'Tag','dv_SumFile');
if isempty(h_sum)|~ishandle(h_sum),
   disp('Could not locate sum data. Return.');
   return;
end
data=get(h_sum,'UserData');
if isempty(data),
   disp('Load Sum file first, then plot intensity contours.');
   return;
end

% === convert intensity data in matrix format 
[matrices,ax]=spec2matrix(data,spectramap);

% === find detectors with intensity below the threshold value
data=[];
for i=1:length(matrices),
   x=matrices{i}(:,:,1);
   y=matrices{i}(:,:,2);
   dx=x(1,2)-x(1,1); % assume all detector elements have the same size 
   dy=y(2,1)-y(1,1);
   int=matrices{i}(:,:,3);
   index=(~isnan(int)&(int<=threshold));
   x=x(index)+dx/2; % move to centre of detector
   y=y(index)+dy/2;
   data=[data; x(:) y(:)];
end   
spec=pos2spec(data,spectramap); % determine spectra numbers of detectors with intensity<=threshold 
spec=spec(spec>=1); % eliminate spectra numbers=0 (not in range)

% === get existing mask, otherwise create a new one 
h_msk=findobj(h_cw,'Tag','dv_MskFile');
if isempty(h_msk)|~ishandle(h_msk),
   disp('Could not locate stored msk data.');
   return;
end
msk=get(h_msk,'UserData'); % masked spectrum numbers 
number=length(msk); % number of masked spectra already in the list
msk=[msk;spec]; % add recently masked spectra to the overall mask list

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
disp(sprintf('Located %d spectra with intensity below the threshold value',length(spec)));
disp(sprintf('out of which %d were added to the existing list',length(msk)-number));
drawnow;


