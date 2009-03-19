function result = ixf_checkfornametag(name, tag)
% checks if the name and tag currently have default properties associated
% with them. returns true if they do, false if they don't.
%
% >> result = ixf_checkfornametag(name, tag)
%
% inputs:
% 
%       name        name to check for
%       tag         tag to check for
%
% outputs:
% 
%       result      Success if name and tag exist with defaults or false if
%                   not.
%

% Dean Whittaker 10/09/2007

if (~ischar(name) || ~ischar(tag)) || (~iscell(name) || ~iscell(tag))
    result = false;
    return
end

props = ixf_name_tag_properties('get',name, tag);

if isempty(props)
    result = false;
else
    result = true;
end