function ui_color_slider(figureHandle_, varargin)
%------ libisis gtk std user ui_color_slider function help ------------------------------
%
% >> ui_color_slider(figureHandle_)
%
% adds sliders to the figure "figureHandle_" to control the colour scale of
% the plot. uses ixf_colour_slider function as callback.

if nargin == 2
    switch varargin{1}
        
        case 'create'
              ixf_gen_interface('iname','setprop_interface','fname','color_slider','figureHandle_',figureHandle_,'option','create');

        case 'delete'
              ixf_gen_interface('iname','setprop_interface','fname','color_slider','figureHandle_',figureHandle_,'option','delete');
        
        otherwise
              error(['unrecognised argument  ''' varargin{1} ' '' '])
    end
elseif nargin == 3
    ixf_gen_interface('iname','setprop_interface','fname','color_slider','figureHandle_',figureHandle_,varargin{:});
else 
    ixf_gen_interface('iname','setprop_interface','fname','color_slider','figureHandle_',figureHandle_,'option','create');
end