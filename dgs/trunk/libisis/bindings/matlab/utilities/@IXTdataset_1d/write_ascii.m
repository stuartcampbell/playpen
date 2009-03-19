function [ok,mess] = write_ascii (w, file)
% WRITE_ASCII writes dataset_1d to an ascii file. 
%
% Syntax (basic useage):
%   >> write_ascii (w)           % prompts for file to write to
%   >> write_ascii (w, file)     % write to named file
%
% If a histogram dataset, the output file format is, for example:
%
%   name = name
%   title = La(Pb)MnO3 data
%   signal_units = S(Q,w)
%   x_units = meV
%   x_distribution = 1
%       x(1)    y(1)    e(1)
%       x(2)    y(2)    e(2)
%        :       :       :
%       x(n)    y(n)    e(n)
%       x(n+1)
%
% If point dataset, then the x-y-e columns terminate as
%        :       :       :
%       x(n)    y(n)    e(n)
%
%  - If w is an array of datasets, tben the dataset will be written out in series
%  - If title or xlabel extends over more than one line, then this will appear
%    in the ascii file as e.g.
%       title = La(Pb)MnO3 data
%       title = 300K
%       title = Run number 12345
%  - Can be read back into Matlab as a dataset_1d with the function READ_ASCII
%
% Advanced useage
% ---------------
%   >> write_ascii (w, fid)         % writes w to currently open text file
%   >> ok = write_ascii (w, fid)    % returns status (ok=1 all fine; ok=0 otherwise)
%   

% original: T.G. Perring, Modified by: Dean Whittaker 30/10/2006

ok = 1;
mess = '';

% Get file name - prompting if necessary
% --------------------------------------
if (nargin==1)
    file_internal = putfile;
    if (isempty(file_internal))
        ok = 0;
        mess = 'No file given';
    end
elseif (nargin==2)
    file_internal = file;
end

if isa_size(file_internal,'row','char')
    fid_given = 0;
    fid = fopen (file_internal, 'wt');
    if (fid < 0)
        ok = 0;
        mess = ['Cannot open file ' file_internal];
    end
elseif isnumeric(file_internal)
    fid_given = 1;
    fid = file_internal;
    [file, mode] = fopen(fid);
    if isempty(file)||~strcmpi(mode,'wt')
        ok = 0;
        mess = 'Check file is open and its mode is "wt"';
    end
else
    ok = 0;
    mess = 'File ID passed to write_ascii is non-numeric';
end
if ~ok; if nargout==0; error(mess); else return; end; end;


% write data to file
% ------------------
for i=1:length(w)
    [ok,mess] = write_ascii(w(i).title,fid,'title'); if ~ok; if nargout==0; error(mess); else return; end; end;
    [ok,mess] = write_ascii(w(i).s_axis.units,fid,'signal_units');  if ~ok; if nargout==0; error(mess); else return; end; end;
    [ok,mess] = write_ascii(w(i).x_axis.units,fid,'x_units'); if ~ok; if nargout==0; error(mess); else return; end; end;
    [ok,mess] = write_ascii(w(i).x_distribution,fid,'x_distribution'); if ~ok; if nargout==0; error(mess); else return; end; end;

    if length(w(i).x)==length(w(i).signal) % point data
        [ok,mess] = write_ascii([w(i).x',w(i).signal',w(i).error'],fid); if ~ok; if nargout==0; error(mess); else return; end; end;
    else
        [ok,mess] = write_ascii([w(i).x(1:end-1)',w(i).signal',w(i).error'],fid); if ~ok; if nargout==0; error(mess); else return; end; end;
        [ok,mess] = write_ascii(w(i).x(end)',fid); if ~ok; if nargout==0; error(mess); else return; end; end;
    end
end

% Close file if function was given a file name, not fid:
if ~fid_given
    fclose(fid);
end

% ensure that if no arguments, do not get any output (otherwise from command line
% a succesful >> write_acsii(w) would print "ans = 1")
if nargout==0
    clear ok mess
end
