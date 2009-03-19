function [figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(varargin)
%----------------help for gtk ixf_get_related_handles----------------------
%
% [figureHandle_, axesHandle_, plotHandle_, otherHandle_] =
% ixf_get_related_handles(ident)
%
% [figureHandle_, axesHandle_, plotHandle_, otherHandle_] =
% ixf_get_related_handles(name, tag)
%
% inputs - ident = any handle related to any object.
%          OR
%           name    name of a plot
%           tag     tag of the plot
%
% outputs - the figure, axes, plot and other handles associated with that
% object -i.e. all parents and children.
%
%--------------------------------------------------------------------------
IXG_ST_STDVALUES=  ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');


handles = [];
plotIndex = [];

%-------------------error check and get handle if given nametag------------

if nargin == 1 && isnumeric(varargin{1})
    if isempty(varargin{1})
        ident = gco;
    else
        ident = varargin{1};
    end    
    try 
        get(ident,'type');
    catch
        error('incorrect handle, can not identify graph')
    end
         
elseif nargin == 2 && ischar(varargin{1}) && ischar(varargin{2})
    handles_init = get(0,'children');
    handleType = ixf_plotdata('get', handles_init, 'object_type');
if iscell(handleType)  % required becuase cell2mat eliminates empty spaces, whereas we want them as NaN.
    temp = handleType;
    handleType = nan(size(handleType));
    for i = 1:length(handleType)
        if ~isempty(temp{i})
            handleType(i) = temp{i};
        end
    end
end

if ~isempty(handleType)
    figureIndex = handleType == IXG_ST_STDVALUES.figure_object_type;
    handles_init = handles_init(figureIndex);
else
    handles_init = [];
end

if ~ isempty(handles_init)
    for i = 1:length(handles_init)
        [name{i} tag{i}] = ixf_get_nametag('handle',handles_init(i));
    end
    match_index = strcmp(name,varargin{1}) & strcmp(tag,varargin{2});
    if any(match_index)
        ident = handles_init(match_index);
    else
        error('incorrect name and tag, can not identify graph')
    end
else
     error('No open figures')
end

else
        error('can not identify graph, require a name and tag or plot handle, figure handle or axes handle')
end

%------------ search for all parents and children of ident-----------------
ident_temp = ident;

while ~isempty(ident_temp)
    %line 85 will *sometimes* fail for no reason unless the following line is
    %called first, it does not achieve anything at all, but prevents
    %redline errors
    get(ident_temp,'children');  
    ident_temp = get(ident_temp,'children');
    
    if iscell(ident_temp)
        ident_temp = cell2mat(ident_temp);
    end

    handles = [handles; ident_temp];

    
end

ident_temp = ident;

while ~isempty(ident_temp)
    
    if iscell(ident_temp)
        ident_temp = cell2mat(ident_temp);
    end
    
    handles = [handles; ident_temp];
    ident_temp = get(ident_temp,'parent');
    
end

%-------------find the indexes of the figure, axes, plot handles-----------
rootIndex = handles == 0;
rootIndex = rootIndex';
handleType = ixf_plotdata('get', handles, 'object_type');
   
if iscell(handleType)  % required becuase cell2mat eliminates empty spaces, whereas we want them as NaN.
    temp = handleType;
    handleType = nan(size(handleType));
    for i = 1:length(handleType)
        if ~isempty(temp{i})
            handleType(i) = temp{i};
        end
    end
end

figureIndex = handleType == IXG_ST_STDVALUES.figure_object_type;
axesIndex = handleType == IXG_ST_STDVALUES.axes_object_type;
plotIndex = handleType == IXG_ST_STDVALUES.plot_object_type;
otherIndex = ~figureIndex & ~axesIndex & ~rootIndex & ~ plotIndex;

figureHandle_ = handles(figureIndex);
axesHandle_ = handles(axesIndex);
plotHandle_ = handles(plotIndex);
otherHandle_ = handles(otherIndex);