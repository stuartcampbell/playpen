function ui_make_current(hdl)
%------------ help for GTK ui_make_current---------------------------------
% purpose: to be run when a figure is selected (sets the title of the
% figure to "CURRENT" or to select a figure
%
% syntax: ui_make_current(hdl)
% input: hdl = handle of the figure to make current
%--------------------------------------------------------------------------
current_figures=get(0,'children');  % get all figures and clear labels
length_figs=length(current_figures);
display('invoked')
if length_figs>0
    for i=1:length_figs
        ixf_gen_interface('iname','setprop_interface','fname','labelcurrent','handle',current_figures(i),'label','false');
    end
else
    ixf_display_error(IXG_ST_ERROR.no_figure);
end
figure(hdl);        % select the given figure (in case not already selected)
ixf_gen_interface('iname','setprop_interface','fname','labelcurrent','handle',hdl,'label','true');

%