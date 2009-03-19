function [] = write_points (w, file)
% WRITE_POINTS writes an IXTdataset_1d to a file in point x,y,e format:
%
%       x(1)    y(1)    e(1)
%       x(2)    y(2)    e(2)
%        :       :       :
%       x(n)    y(n)    e(n)
%
% Can be read back into libisis with the function READ_POINTS
%
% Syntax:
%   >> write_points (w)           % prompts for file to write to
%   >> write_points (w, file)     % write to named file


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
if (size(w.x)==size(w.signal))   % point spectrum
    fprintf (fid, '%30.16g  %30.16g  %30.16g \n', [w.x; w.signal; w.error;]);
else
    fprintf (fid, '%30.16g  %30.16g  %30.16g \n', [(0.5.*(w.x(2:end)+w.x(1:end-1))); w.signal; w.error]);
end

fclose(fid);
    