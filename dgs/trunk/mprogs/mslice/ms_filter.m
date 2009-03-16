function filter=ms_filter(hfile,ext)

% function filter=ms_filter(hfile,ext)
% hfile = handle of object holding a filename in the 'String' property
% ext = for example '*.msp' for MSlice parameter files
% returns filter for browser (either filename or wildcard extension)

% === return filter for browsing
if isempty(hfile)|isempty(get(hfile,'String')),
   filter=ext;
else	% if filename is present, then determine which file extension to use
   filter=get(hfile,'String');
   fpos=findstr(filter,'.'); 
   if isempty(fpos),	% e.g. filename = 'cut250'
      file_ext=[];
   else	% e.g. 'cut250.' or 'cut250.cut'
      file_ext=filter((fpos+1):end);
      if ~isempty(file_ext),
         file_ext=file_ext(~isspace(file_ext));	% for above example returns [] or 'cut'
      end
   end
   if isempty(file_ext),
      if ~isempty(fpos),
         filter=[filter(1:(fpos-1)) ext(2:end)];
      else
         filter= [filter ext(2:end)];
      end
   end
end
% === if is vms get rid of the version number for the filter
a=version;
if (a(1)<=5)&isvms,
    fpos=findstr(filter,';'); 
    if ~isempty(fpos),	   
      filter=filter(1:(fpos(1)-1));
    end
end	
