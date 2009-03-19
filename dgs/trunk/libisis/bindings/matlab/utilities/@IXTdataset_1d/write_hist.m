function [] = write_hist (w, file)
% WRITE_HIST writes histogram dataset to a file in histogram x,y,e format:
%
%       x(1)    y(1)    e(1)
%       x(2)    y(2)    e(2)
%        :       :       :
%       x(n)    y(n)    e(n)
%       x(n+1)   0       0
%
% Can be read back into libisis with the function READ_HIST
%
% Syntax:
%   >> write_hist (w)           % prompts for file to write to
%   >> write_hist (w, file)     % write to named file
%


if (size(w.x)~=size(w.signal)+1)    % not a histogram spectrum
    error ('Cannot write point data using WRITE_HIST')    
end


% Get file name - prompting if necessary
% --------------------------------------
if (nargin==1)
    file_internal = putfile;
    if (isempty(file_internal))
        error ('No file given')
    end
elseif (nargin==2)
    file_internal = file;
end

fid = fopen (file_internal, 'wt');
if (fid < 0)
    error (['ERROR: cannot open file ' file_internal])
end

% write data to file
% ------------------
fprintf (fid, '%30.16g %30.16g %30.16g \n', [w.x; [w.signal 0]; [w.error 0]]);

fclose(fid);
    