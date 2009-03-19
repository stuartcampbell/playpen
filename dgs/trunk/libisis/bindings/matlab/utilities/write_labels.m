function labels = write_labels (data, opt, optfields)
% Writes a structure to cell array of strings. See read-labels, of which this is the inverse
%
%   >> write_labels (fid, data)
%   >> write_labels (fid, data, opt, fields) 
%
%   fid         fid of an open file
%   data        structure
%   opt         Option: 'except' or 'only'  to indicate which fields shoudl be written
%   optfields   if opt='except': cell array of strings of fields not to be written
%                  opt='only'  : only write these fields
%
% Label information will be written in the form:
%   lhs_1 = rhs_1
%   lhs_2 = rhs_2
%       :    :
% 
% Will write numeric scalar or array, strings or cell arrays of strings. If cell array, then 
% writes on successive lines with the same variable name e.g. title={'Hello','Mister'} is written
%   title = Hello
%   title = Mister
%

% T.G.Perring   March 2008  Uses write_ascii - could be more efficient, but will do for now.

if ~(nargin==1||nargin==3)
    error('Check number of arguments')
end
   
labels=cell(0);
if nargin==1
    fields=fieldnames(data);
    for i=1:length(fields)
        labels=[labels,write_label(fields{i},data.(fields{i}))];
    end
elseif nargin==3
    if strcmp(opt,'only')
        for i=1:length(optfields)
            if isfield(data,optfields{i})
                labels=[labels,write_label(optfields{i},data.(optfields{i}))];
            end
        end
    elseif strcmp(opt,'except')
        fields=fieldnames(data);
        for i=1:length(fields)
            if ~any(strcmp(fields{i},optfields))
                labels=[labels,write_label(fields{i},data.(fields{i}))];
            end
        end
    end
end

%======================================================================================================
function label=write_label(name,var)

name=[name,' = '];
label = cell(0);

if isempty(var) && named % write out label if present, otherwise do nothing
    label = sprintf ('%s', name);
    
elseif iscellstr(var)
    for i=1:numel(var)
        label = [label, strtrim(sprintf('%s%s', name, var{i}))];
    end
    
elseif isa(var,'char')
    dims = size(var);
    if length(dims)>2  % multi-dimensional character array; 2nd dimension is the string
        var = reshape(var,dims(1),dims(2),prod(dims(3:end)));   % reshape to be array of matricies
        for j=1:prod(dims(3:end))
            for i=1:dims(1)
                label = [label, strtrim(sprintf('%s%s', name, var(i,:,j)))];
            end
        end
    else
        for i=1:dims(1)
            label = [label, strtrim(sprintf('%s%s', name, var(i,:)))];
        end
    end

elseif isa(var,'numeric')
    dims = size(var);
    fmt = repmat('%-25.16g',1,dims(2));
    if length(dims)>2  % multi-dimensional array; layout as pages of matricies
        var = reshape(var,dims(1),dims(2),prod(dims(3:end)));   % reshape to be array of matricies
        fmt = ['%s',fmt];
        for j=1:prod(dims(3:end))
            for i=1:dims(1)
                label = [label, strtrim(sprintf(fmt, name, var(i,:,j)))];
            end
        end
    else
        fmt = ['%s',fmt];
        for i=1:dims(1)
            label = [label, strtrim(sprintf(fmt, name, var(i,:)))];
        end
    end

end
