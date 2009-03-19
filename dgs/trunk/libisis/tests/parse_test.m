  function parse_test (varargin)
  % 
  arglist = struct('background',[12000,18000], ...    % argument names and default values
                   'normalise', 1, ...
                   'modulation', 0, ...
                   'output', 'data.txt');
  flags = {'normalise','modulation'};                 % arguments which are logical flags
  
  [par,argout,present] = parse_arguments(varargin,arglist,flags);
  par
  argout
  present
