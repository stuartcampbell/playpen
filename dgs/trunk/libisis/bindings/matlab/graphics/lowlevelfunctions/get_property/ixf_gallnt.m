function [names, tags] = ixf_gallnt()
% libisis gtk graphics package ixf_gallnt get all name tags - gets the names 
% and tags of all the current open plots
% 
% >> [names, tags] = gallnt()
% 
% Inputs:
% none
% 
% Outputs:
% names - cell array of strings, containing the names of all currently
% stored name tags
%
% tags - cell array of strings containing the tags of all currnetly stored 
% name tags. They are arranged such that a graph can be identified by it's
% name tag
%
% [name(i) tag(i)] where i can be any index. 

% Written by Dean Whittaker 21/12/2006

% get all handles


props = ixf_name_tag_properties('get');

if iscell(props) && ~isempty(props)
    for i = 1:numel(props)
        names{i} = props{i}.figure.name;
        tags{i} = props{i}.figure.tag;
    end
else
    names = {};
    tags = {};
end