function string_out = string_replace(string_in,varargin)
% string_replace - takes an input string and input parameters and replaces
% some parts of the string according to the input.
%
% syntax:
% 
% >> string_out - string_replace(string_in, 'phrase1', 'replacement1',
% 'phrase2','replacement2',....)
%
% will replace any instances of phrase 1 in string_in with replacement1,
% and so on. 
%
% inputs: string_in - the string to look at, phrases - the phrases to look
% for, replacements - the strings to replace the phrases with. 
%
% outputs: string_out
%
% example:
%
% >> xlabel = string_replace(xlabel, 'micro', '\mu', 'second', 's')
%
% will replace any instance of the word micro in xlabel with '\mu' which is
% the latex sybol for mu, and any instance of the word second with s. 

% pre-allocate string_out
string_out = string_in;

if iscell(string_in)                    % if a cell, look at each element in turn
    for i = 1:length(string_in)
    for j = 1:2:length(varargin)
        if ischar(varargin{j}) && ischar(varargin{j+1})
            indexes = findstr(string_out{i},varargin{j});
        else
            error('string_replace requires string (or cells of strings) input only')
        end
   
        for k = 1:length(indexes)
            string_out{i}(indexes(k):(indexes(k)+length(varargin{j})-1)) = [];  
                                        % Erase the string
            string_out{i}=[string_out{i}(1:(indexes(k)-1)), varargin{j+1}, string_out{i}((indexes(k)):end)];
                                        % put new string in 
            indexes = indexes + ones(1,length(indexes)).*(length(varargin{j+1}) - length(varargin{j}));
                                        % change indexes to account for
                                        % different size of new string
        end
    end
    end
elseif ~ ischar(string_in)
    error('string replace requires string input')
else
    % if not a cell, use the same procedure omitting the cell notation for
    % string_in

    for j = 1:2:length(varargin)
        if ischar(varargin{j}) && ischar(varargin{j+1})
            indexes = findstr(string_out,varargin{j});
        else
            error('string_replace requires string (or cells of strings) input only')
        end
   
        for k = 1:length(indexes)
            string_out(indexes(k):(indexes(k)+length(varargin{j})-1)) = [];
            string_out=[string_out(1:(indexes(k)-1)), varargin{j+1}, string_out((indexes(k)):end)];
            indexes = indexes + ones(1,length(indexes)).*(length(varargin{j+1}) - length(varargin{j}));
        end
    end
end