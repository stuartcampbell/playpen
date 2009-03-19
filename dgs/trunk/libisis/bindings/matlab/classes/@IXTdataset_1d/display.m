function display(dataset_1d)
% Display an IXTdataset_1d object
libisisexc('IXTdataset_1d','display',dataset_1d);

% if(dataset_1d.x_histogram==logical(0)),
%     errorbar(dataset_1d.x,dataset_1d.s,dataset_1d.e)
%     title(dataset_1d.title);
%     xlabel([dataset_1d.x_label, ' in ', dataset_1d.x_units]);
%     ylabel(dataset_1d.s_label);
% end