function ixf_set_cscale(hid, axesHandle_, type, option)
%----- lbisis gtk ixf_set_cscale, log color axes standard user function-----------
%
% >> ixf_set_cscale('aid', axesHandle_, 'type', scale)
%
% changes the colour axes to a logorithmic or linear scale
%
% inputs:   axesHandle_     - handle of axes to be changed
%           scale           - 'log' or 'linear'

switch option
    case {'log', 'logarithmic'}
        
        [figureHandle_,axesHandle_,plotHandle_,otherHandle] = ixf_get_related_handles(axesHandle_);      

        can_log_flag = true;

       clog_flag=ixf_plotdata('get',axesHandle_,'clog_flag');

       if ~isempty(clog_flag) && clog_flag
                can_log_flag = false;
       end

        clog_flag = true;
        
        ixf_plotdata('set', axesHandle_, 'clog_flag', clog_flag);

        if can_log_flag
            for i = 1:length(plotHandle_)        
                    data=get(plotHandle_(i),'CData');
                    ndata = log(data);
                    set(plotHandle_(i),'CData',ndata);

                    h = colorbar;
                    axStr=get(h,'YTicklabel');
                    newAxStr=num2str(truncdig(exp(str2num(axStr)), 2));
                    set(h,'YTickLabel',newAxStr);
            end
        end
        
    case {'lin', 'linear'}
        
        ixf_redraw_graph(axesHandle_)
        colorbar

    otherwise
        error(['unrecognised option:  ' varargin{1}])
end