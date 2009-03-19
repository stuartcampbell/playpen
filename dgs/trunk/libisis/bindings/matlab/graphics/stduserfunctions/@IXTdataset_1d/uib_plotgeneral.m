function [figureHandle_,axesHandle_,plotHandle_] = uib_plotgeneral(w,format,varargin)
%--------------------------------------------------------------------------
%Function Syntax: 
%[figureHandle_,axesHandle_,plotHandle_] = 
%uib_plotgeneral(w,format,[property_name,property_value])
%Output: figure,axes and plot handle
%Input: 1d dataset object, format('e','m','h','l') and other control parameters (name value pairs)
%list of control propertie names
%>>IXG_ST_DEFAULT.figure
%>>IXG_ST_DEFAULT.plot
%>>IXG_ST_DEFAULT.axes
%Purpose: plot the data according to values and control properties (for
%figure, axes and plot)
%Example: 
%uib_plotgeneral(w,'h') --> (hist)default structure plot
%uib_plotgeneral(w,'h','Color','red') --> (hist)override default structure values 
%uib_plotgeneral(w,'e','default','my_struct','Color','red') --> (errorbar)override values 
%uib_plotgeneral(w,'m','default','my_struct') --> (marker) from structure
%--------------------------------------------------------------------------
%global structures

if isnumeric(w)
    w = getspectrum(w);
end

[IXG_ST_INTERFACEVALIDATION, IXG_ST_STDVALUES]= ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_STDVALUES');

% ixf_validate_arg(IXG_ST_USERVALIDATION.uib_plotfigure,nargin,'greater');   
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uib_plotfigure,nargin,'greater');
tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(tot,2));

len = length(w);

if len==1
% note: x_units is not xlabel? What is?

    [xlab, ylab] = make_label(w);
    title = w.title;

else
    if ~ixf_check_labels(w)
        display('WARNING: The axes labels in an array of datasets is different, the information from the first dataset in the array has been used on the plot')
    end
    
    [xlab, ylab] = make_label(w(1));
    title = w(1).title;

end

ixf_validate_plot('name', IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,varargin{:});

%----------------------- overplot the graphs-------------------------------

for i=1:len
    w(i) = rebunch(w(i),IXG_ST_STDVALUES.binning);
end
    switch(format)
        case 'h'
            for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end
            % [figureHandle_,axesHandle_,plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','plot',w.x,w.signal,'format','p','tag',IXG_ST_STDVALUES.oned,varargin{:},'Marker','none','title',char(title),'xlabel',xlab,'ylabel',ylab);
            [figureHandle_(:,i),axesHandle_(:,i),plotHandle_(:,i)] = ixf_gen_interface('iname','plot_interface','fname','plot',w(i),'format','p','name',IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,'Marker','none','title',char(title),'xlabel',xlab,'ylabel',ylab,'counter',counter_value,varargin{:});
                if i > 1 
                    hold off
                end
            end
            
        case 'l'
            for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end            
            % [figureHandle_,axesHandle_,plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','plot',w.x,w.signal,'format','l','tag',IXG_ST_STDVALUES.oned,varargin{:},'Marker','none','title',char(title),'xlabel',xlab,'ylabel',ylab);
            [figureHandle_(:,i),axesHandle_(:,i),plotHandle_(:,i)] = ixf_gen_interface('iname','plot_interface','fname','plot',w(i),'format','l','name',IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,'Marker','none','title',char(title),'xlabel',xlab,'ylabel',ylab,'counter',counter_value,varargin{:});
                if i > 1 
                    hold off
                end
            end
        case 'e'
            for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end   
            % [figureHandle_,axesHandle_,plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','ploterror',w.x,w.signal,w.error,'format','e','tag',IXG_ST_STDVALUES.oned,varargin{:},'Marker','none','title',char(title),'xlabel',xlab,'ylabel',ylab);
            [figureHandle_(:,i),axesHandle_(:,i),plotHandle_(:,i)] = ixf_gen_interface('iname','plot_interface','fname','ploterror',w(i),'format','e','name',IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,'Marker','none','title',char(title),'xlabel',xlab,'ylabel',ylab,'counter',counter_value,varargin{:});
            % hold on;
            % ixf_validate_plot('tag',IXG_ST_STDVALUES.oned,'format','p',varargin{:});
            % [figureHandle_,axesHandle_,plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','plot',w.x,w.signal,'format','p','tag',IXG_ST_STDVALUES.oned,varargin{:},'Marker','none');
                if i > 1 
                    hold off
                end
            end
        case 'm'
            for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end   
            % [figureHandle_,axesHandle_,plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','plot',w.x,w.signal,'format','m','tag',IXG_ST_STDVALUES.oned,varargin{:},'Linestyle','none','title',char(title),'xlabel',xlab,'ylabel',ylab);
            [figureHandle_(:,i),axesHandle_(:,i),plotHandle_(:,i)] = ixf_gen_interface('iname','plot_interface','fname','plot',w(i),'format','m','name',IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,'Linestyle','none','title',char(title),'xlabel',xlab,'ylabel',ylab,'counter',counter_value,varargin{:});
            % hold on;
            % ixf_validate_plot('tag',IXG_ST_STDVALUES.oned,'format','m',varargin{:});
            % [figureHandle_,axesHandle_,plotHandle_] = ixf_gen_interface('iname','plot_interface','fname','plot',w.x,w.signal,'format','p','tag',IXG_ST_STDVALUES.oned,varargin{:});
                if i > 1 
                    hold off
                end
            end
            % hold off;
        case 'p'
            for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end   
            [figureHandle_(:,i),axesHandle_(:,i),plotHandle_(:,i)] = ixf_gen_interface('iname','plot_interface','fname','ploterrorplus',w(i),'format','pp','name',IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,'title',char(title),'xlabel',xlab,'ylabel',ylab,'counter',counter_value,'linestyle','none',varargin{:});

                
                if i > 1 
                    hold off
                end
            end
        case 'd'
            for i = 1:len
                if i>1
                    % set hold, and set default counter values
                    counter_value = IXG_ST_STDVALUES.counter_increment;
                    hold on
                else
                    counter_value = IXG_ST_STDVALUES.counter_reset;
                end           
            [figureHandle_(:,i),axesHandle_(:,i),plotHandle_(:,i)] = ixf_gen_interface('iname','plot_interface','fname','ploterrorplus',w(i),'format','d','name',IXG_ST_STDVALUES.oned_name,'tag',IXG_ST_STDVALUES.oned,'title',char(title),'xlabel',xlab,'ylabel',ylab,'counter',counter_value,varargin{:});
                
                if i > 1 
                    hold off
                end
            end
    end

    ixf_set_axespos('hdl',gca,'title',title,'xlab',xlab,'ylab',ylab);
 

