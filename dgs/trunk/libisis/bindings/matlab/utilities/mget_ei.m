function  [varargout] = mget_ei(run_numbers,varargin)
% mget_ei(run_numbers)
% mget_ei(run_numbers,ei_guesses)
% ei=mget_ei(run_numbers,ei_guesses)
% [ei,properties]=mget_ei(run_numbers,ei_guesses)
%
% ei_guesses must be a numerical array equal to the number of run_numbers,
% containing the starting ei for get_ei to use in its calculation
%
% properties is an optional output containing all the peripheral output
% from the get_ei command ie peak position, area etc...
%
% run numbers can only be defined in the following ways:
% 1) a numerical array of numbers -> [12345  45673 34436] 
% 2) an array of strings -> char('12345' '45673' '34436')
% 3) a cell array of strings -> {'12345' '45673' '34436'}
% 4) a cell array of numerics -> {12345  45673 34436}
% 5) a cell or character array of paths to particular raw files ->
% 'C:\data\MAR11060.RAW'
%
% See also get_ei
[files,num]=make_path_from_par({run_numbers});
guesses=false;
if nargin>1
    guesses=true;
    if size(varargin{:},2)~= num
        error('number of ei guesses not equal to number of runs')
    end
end

a=zeros(num,1);
for i=1:num
    if guesses
        [a(i),ei_props(i)]=get_ei(files{i},varargin{1}(i));  
    else
        [a(i),ei_props(i)]=get_ei(files{i});      
    end
end
varargout(1)={a};
varargout(2)={ei_props};
