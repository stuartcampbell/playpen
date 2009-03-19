function data_out=do_homer(indat,mask,wbrf,val_abs)

%%routine that creates a corrected, reduced data object
% requires indat data structure from the Homer GUI




DSOc=       setup_DSO_bank(indat.inst); % monochromatic data source sample
DSOc=       add_chopinst(DSOc);
DSOc=       add_item(DSOc,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',indat.choppertype);

mapfile=strcat('inst_maps:::',indat.map_file);
if ~isempty(indat.mon_mapfile)
    mon_map=strcat('inst_maps:::',indat.mon_mapfile);
else
    mon_map='';
end
if indat.sum_files == 1
    L=size(indat.run_num,1);
    for i=1:L
    DSOc=add_multrawfile(DSOc,strcat('inst_data:::',indat.inst,indat.run_num(i,:),'.RAW'));
    end
else
    DSOc=add_rawfile(DSOc,strcat('inst_data:::',indat.inst,indat.run_num,'.RAW'));
end

if indat.do_absolute==1
    %% Treat runfile
    cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
        'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
        'corr','abs',val_abs,'mass',indat.samp_mass,'RMM',indat.samp_rmm,'background',indat.background,'mon_map',mon_map);
    data_out= homer(DSOc,cellout{:});
    
else  
    %
    %% if NO absolute normalisation
    % Treat runfile
    cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
        'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
        'corr','background',indat.background,'mon_map',mon_map);
    data_out= homer(DSOc,cellout{:});
end
