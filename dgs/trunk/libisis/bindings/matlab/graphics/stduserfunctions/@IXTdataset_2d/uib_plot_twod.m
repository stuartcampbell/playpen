function [figureHandle_ axesHandle_ plotHandle_]=uib_plot_twod(w,format,varargin)
%-----------------Help for GTK uib_plot_twod-------------------------------
%
% purpose: plot interface for a single dataset2d object
%
% Syntax: [figurehandle, axeshandle,
% plothandle]=uib_plot_twod(w,format,varargin)
%
% inputs: w - dataset 2d object, format - type of graph to plot, varargin -
% optional settings for graph
% outputs: figure handle, axes handle, plot handle
%
%-----------updated: 15/08/2006, Dean Whittaker----------------------------
%global structures
[IXG_ST_INTERFACEVALIDATION, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_STDVALUES');

tot=length(varargin);
separation=[];

% insure arguments match allowed values
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uib_plotfigure,nargin,'greater');
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(tot,2));
tot = numel(varargin);

separate_flag=IXG_ST_STDVALUES.false;    

totArg=(length(varargin)-1);

for iArg = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    if strcmpi(varargin{iArg},'separation')&&strcmpi(varargin{iArg+1},'on')
        separate_flag=IXG_ST_STDVALUES.true;    % look for separate graphs option and treat
                                                % data correctly if found
    end

end


% check this is a single dataset 2d object
len = length(w);
% REBUNCH required here, currently not a function

if len==1
[xlab, ylab,zlab] = make_label(w);
title = w(1).title;
separate_flag = true;
else
    if ~ ixf_check_labels(w) % check that labels are same, if not then display a warning
        display('WARNING: Axes labels, or distribution in the elements of the array are different, information from the first dataset has been used for plotting')
    end
    [xlab, ylab,zlab] = make_label(w(1));
    title = w(1).title;
end



switch format

    case 'area'    
        ixf_validate_plot('name',IXG_ST_STDVALUES.area_name,'tag',IXG_ST_STDVALUES.twod,varargin{:});
        separate_flag = IXG_ST_STDVALUES.true;
    if separate_flag == IXG_ST_STDVALUES.true
        for i = 1:len
            if i>1
                % set hold, and set default counter values
                counter_value = IXG_ST_STDVALUES.counter_increment;
                hold on
            else
                counter_value = IXG_ST_STDVALUES.counter_reset;
            end
            
            [figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','area',w(i),'format',format,'name',IXG_ST_STDVALUES.area_name,'tag',IXG_ST_STDVALUES.twod,'title',title,...
            'xlabel',xlab,'ylabel',ylab,'zlabel',zlab,'marker','none', 'counter', counter_value,varargin{:});
        
            if i > 1
                hold off
            end
       end
    else
        counter_value = IXG_ST_STDVALUES.counter_reset;

            [figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','area',w,'format',format,'name',IXG_ST_STDVALUES.area_name,'tag',IXG_ST_STDVALUES.twod,'title',title,...
            'xlabel',xlab,'ylabel',ylab,'zlabel',zlab,'marker','none','counter', counter_value,varargin{:});
    end
        colorbar;
        
    
    case 'surface'
        
        ixf_validate_plot('name',IXG_ST_STDVALUES.surface_name,'tag',IXG_ST_STDVALUES.twod,varargin{:});
        if separate_flag == IXG_ST_STDVALUES.true
        
            for i = 1:len
                
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end


            [figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','surface',w(i),'format',format,'name',IXG_ST_STDVALUES.surface_name,'tag',IXG_ST_STDVALUES.twod,'title',title,...
            'xlabel',xlab,'ylabel',ylab,'zlabel',zlab,'marker','none','counter', counter_value,'separation',separation,varargin{:},'shading','interp');
            
                if i > 1
                    hold off
                end

            end

        else
        counter_value = IXG_ST_STDVALUES.counter_reset;
            [figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','surface',w,'format',format,'name',IXG_ST_STDVALUES.surface_name,'tag',IXG_ST_STDVALUES.twod,'title',title,...
            'xlabel',xlab,'ylabel',ylab,'zlabel',zlab,'marker','none','counter', counter_value,varargin{:},'shading','interp');
            
        end
        
    case 'stem'
        ixf_validate_plot('name',IXG_ST_STDVALUES.stem_name,'tag',IXG_ST_STDVALUES.twod,varargin{:});
         for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end

        [figureHandle_ axesHandle_ plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','stem',w,'format',format,'name',IXG_ST_STDVALUES.stem_name,'tag',IXG_ST_STDVALUES.twod,'title',title,...
        'xlabel',xlab,'ylabel',ylab,'zlabel',zlab,'marker','none','counter', counter_value,'separation',separation,varargin{:});
                
                if i > 1
                    hold off
                end

         end
    otherwise
        error('incorrect format type')
end


