function display(datum_array)

% Display an IXTdatum_array
libisisexc('IXTdatum_array','display',datum_array);

% if(dataset_1d.x_histogram==logical(0)),
%     errorbar(dataset_1d.x,dataset_1d.s,dataset_1d.e)
%     title(dataset_1d.title);
%     xlabel([dataset_1d.x_label, ' in ', dataset_1d.x_units]);
%     ylabel(dataset_1d.s_label);
% end