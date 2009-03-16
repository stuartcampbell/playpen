function fw=firstword(s)

% function fw=firstword(s)
% extracts first word from string s

index_space=find(isspace(s));
index_nospace=find(~isspace(s));
index_space=index_space(index_space>index_nospace(1));
fw=s(index_nospace(1):(index_space(1)-1));