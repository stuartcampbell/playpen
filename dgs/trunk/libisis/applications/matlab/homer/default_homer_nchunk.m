function nchunk=default_homer_nchunk
% nchunk = default_homer_nchunk
% nchunk is a two membered array
% the first element is the default number of workspaces to be treated at 
% one time, and the second element is the maximum amount of memory (in MB)
% that you want the computer to use
nchunk=int32([100,500]);