function answer = string_find (str, cellstr)
% string_find(str,cellstr)   Finds the index of the element of a cell array of strings
%                            for which the test string STR is an unambiguous abbreviation.
%                            If str is an exact match for an element of cellstr, then it is
%                           accepted as unique even if it is also an abbreviation for another element
%                            If more than one possible, answer < 0
%                            If no abbreviation, answer = 0
%
if ~(ischar(str) & size(str,1)==1)
    answer = 0;  % not a single string
end

if iscellstr(cellstr) & length(cellstr)>0
    l_str = length(str);
    matches = 0;
    equality = 0;
    for i = 1:length(cellstr)
        n = length(cellstr{i});
        if (n >= l_str)
            index = strncmpi(str,cellstr{i},l_str);
            if index==1
                i_match = i;
                matches = matches + 1;
                if n==l_str
                    i_equal = i;
                    equality = equality + 1;
                end
            end
        end
    end
    if equality == 1
        answer = i_equal;
    elseif matches == 0
        answer = 0;
    elseif matches == 1
        answer = i_match;
    elseif matches > 1
        answer = -1;
    end
else
    answer = 0;
end

