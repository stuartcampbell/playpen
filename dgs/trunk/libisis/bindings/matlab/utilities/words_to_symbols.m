function string_out = words_to_symbols(string_in,varargin)
% words_to_symbols - is an interface into string_replace. A list of known
% abbreviations is used to replace words with symbols. More can be added
% with the optional inputs
%
% syntax:
% 
% >> string_out = words_to_symbols(string_in,'additional word','additional symbol',...)
%
% will replace any instances of 'additional word' in string_in with 'additional symbol',
% and so on as well as replacing any common words with their symbols.
%
% example:
%
% >> xlabel = words_to_symbols(xlabel, 'time of flight', 'TOF')
%
% will replace any instance of the word time of flight in xlabel with TOF
% as well as any 'micro' with '\mu' and various other common words.

string_use = lower(string_in);

symbol_list = {'micro','\mu','second','s','seconds','s','nano','n','milli','m',...
    'angstrom','\AA', 'pi', '\pi'};

string_out = string_replace(string_use, symbol_list{:}, varargin{:});

        
        
        
        