function DSO=setup_maps_data_source
DSO=IXTdata_source;
% these can be instrument specific
%DSO=add_item(DSO,'inst_nxs:::attenuator_1.nxs','attenuator','attenuator_2');
% these are needed specifically for instrument population
% moderator.nxs defines sample distance
DSO=add_item(DSO,'inst_nxs:::source.nxs','source');
DSO=add_item(DSO,'inst_nxs:::moderator.nxs','moderator');
