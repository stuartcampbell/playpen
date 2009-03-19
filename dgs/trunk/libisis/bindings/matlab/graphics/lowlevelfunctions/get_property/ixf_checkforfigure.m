function [ret]=ixf_checkforfigure(varargin)
%-----------help for GTK application ixf_checkforfigure--------------------
% purpose: checks for all current open, unheld figures of the given input type
%
% call syntax: 
% 
% >> [figureHandle1_, figureHandle2_, etc.]=ixf_checkforfigure(figuretype) OR
% >> [figureHandles_]=ixf_checkforfigure(name,tag) OR
% >> [figureHandles_]=ixf_checkforfigure(handle,name,tag) 
% where handle is a handle to be ignored.
%
% inputs: 
% figure name and tag (either as one word or seperately)
% handle - a handle to be ignored (useful for ixf_figureclose in
% particular)
%
% output: array of handles or false (if not open)
%
% updated: 18/08/2006       Dean Whittaker
%--------------------------------------------------------------------------

[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

ret=IXG_ST_STDVALUES.false;
j=1;                                % j is index of outputs (i.e. ret(j))
fig_handles=get(0,'children');      % get all figures
totfigs = length(fig_handles);

if totfigs==0                       % give a false for no figures
    ret=IXG_ST_STDVALUES.false;
end

for i=1:totfigs
    
    figure_tag=ixf_check_graphicfigure('figure_handle',fig_handles(i))==IXG_ST_STDVALUES.true;
    
    if figure_tag
   
        [name tag] = ixf_get_nametag('figure',fig_handles(i));

        keep=getappdata(fig_handles(i),IXG_ST_STDVALUES.keep_tag);

        if nargin==1                    % else check for all the types of figure
            error('incorrect number of arguments')
        elseif nargin==2        % if the name and tag of the graph is given instead

            if (strcmp(name,varargin{1}) && ...
                    strcmp(tag,varargin{2})  && ...
                     ~keep)
                 ret(j)=fig_handles(i);
                 j=j+1;
            end

        elseif nargin==3               % case where you want 1 handle ignored (useful for 
                                       % the exit command)
            if fig_handles(i)~=varargin{1}
                if (strcmp(name,varargin{2}) && ...
                    strcmp(tag,varargin{3})  && ...
                     ~keep)
                    ret(j)=fig_handles(i);
                    j=j+1;
                end
            end


        else
            ixf_display_error(IXG_ST_ERROR.wrong_arg);
        end
    end
end

