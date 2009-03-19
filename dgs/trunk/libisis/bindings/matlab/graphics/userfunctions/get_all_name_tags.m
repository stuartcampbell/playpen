function [names tags] = get_all_name_tags()
% gtk gallnt - get all name tags function
% returns the names and tags of all currently open plots
%
% >> [names tags] = gallnt
%
% Inputs: none
%
% Outputs:
%
% names: cell array list of names of all current plots
%
% tags: cell array list of tags of all current plots
%
% names and tags are given such that, to identify a plot type the name tag
% will be
%
% name{i} tag{i}
% Dean Whittaker 2007
[names tags] = ixf_gallnt;
