function output = ixf_remove_spaces(input)
%---------help for GTK ixf_remove_spaces-----------------
% purpose: Any elements in a given string which would not be allowed in a
% filename are replaced with '_'.
%
% syntax: output = ixf_remove_space(input)
%
% input is a string
%
%--------------------------------------------------------

if nargin == 0 || isempty(input)
    output = '';
    return
end

good_char = {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p', ...
    'q','r','s','t','u','v','w','x','y','z','.','1','2','3','4','5','6','7','8', ...
    '9','0','_'};
good_first_char =  {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p', ...
    'q','r','s','t','u','v','w','x','y','z','_'};
bad_char_index = ones(1,numel(input(:)));

for i = 1:length(good_first_char)
    bad_char_index(1) = bad_char_index(1) & (lower(input(1)) ~= good_first_char{i});
end

if numel(input) > 1
    for i = 1:length(good_char)
        bad_char_index(2:end) = bad_char_index(2:end) & (lower(input(2:end)) ~= good_char{i});
    end
end
temp = ((bad_char_index).*'_') + ((input == '.').*'P');
output = (~temp).*input + temp;
output = char(output);



