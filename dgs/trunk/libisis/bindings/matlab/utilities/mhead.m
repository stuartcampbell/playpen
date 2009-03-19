function  mhead(varargin)
% head(run_number)
%
% returns the header information in sequence for the raw files defined by the
% run_numbers, or paths to  raw files 
%
% run numbers can be defined in the following ways:
% 1) a numerical array of numbers -> [12345  45673 34436] 
% 2) an array of strings -> char('12345' '45673' '34436')
% 3) a cell array of strings -> {'12345' '45673' '34436'}
% 4) a cell array of numerics -> {12345  45673 34436}
% 5) a cell or character array of paths to particular raw files ->
% 'C:\data\MAR11060.RAW'
%
% See also head
[files,num]=make_path_from_par({varargin{:}});

for i=1:num
%    disp(sprintf('\ndatafile %s',files{i}))
    head(files{i});
end
