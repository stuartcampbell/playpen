function [x,y,int,err]=put_in_matrix(spec_data,s1,s2,tubes,channels,x_bl,y_bl,dx,dy,flip_command);

% function [x,y,int,err]=put_in_matrix(spec_data,s1,s2,tubes,channels,x_bl,y_bl,dx,dy,flip_command);

if sum((spec_data>=s1)&(spec_data<=s2))==0,
   x=[];
   y=[];
   int=[];
   err=[];
   return;
end

int=NaN*ones(channels,tubes);	% will contain the intensities (channels,tubes)
err=int;	% errors (channels,tubes)
ij=find((spec_data(:,1)>=s1)&(spec_data(:,1)<=s2)); % index spectra in the bank
int(spec_data(ij(:),1)-s1+1)=spec_data(ij(:),2);	% put intensities in the <int> matrix
err(spec_data(ij(:),1)-s1+1)=spec_data(ij(:),3);	% put errors in the <err> matrix

% === flip data in given order if required
if exist('flip_command','var')&~isempty(flip_command)&ischar(flip_command),
   for i=1:size(flip_command,1),
      int=feval(deblank(flip_command(i,:)),int);
      err=feval(deblank(flip_command(i,:)),err);
   end
end

% === add an extra column and row to be able to use pcolor
int(channels+1,:)=NaN*ones(1,tubes);
int(:,tubes+1)=NaN*ones(channels+1,1);
err(channels+1,:)=NaN*ones(1,tubes);
err(:,tubes+1)=NaN*ones(channels+1,1);

[x,y]=meshgrid(x_bl:dx:(x_bl+dx*tubes),y_bl:dy:(y_bl+channels*dy)); % (channels+1,tubes+1)
