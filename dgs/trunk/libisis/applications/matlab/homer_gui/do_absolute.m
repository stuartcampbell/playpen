function [val_abs] =  do_absolute(indat,wbrf,mask)



DSOc_mono=  setup_DSO_bank(indat.inst); % monochromatic vanadium data source
DSOc_mono=  add_chopinst(DSOc_mono);
DSOc_mono=  add_item(DSOc_mono,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',indat.choppertype);

mv_mapfile=strcat('inst_maps:::',indat.mv_mapfile);
if ~isempty(indat.mon_mapfile)
    mon_map=strcat('inst_maps:::',indat.mon_mapfile);
else
    mon_map='';
end



if indat.mv_sum_files == 1
    L=size(indat.run_num_mono,1);
    for i=1:L
    DSOc_mono=add_multrawfile(DSOc_mono,strcat('inst_data:::',indat.inst,indat.run_num(i,:),'.RAW'));
    end
else
    DSOc_mono=add_rawfile(DSOc_mono,strcat('inst_data:::',indat.inst,indat.run_num,'.RAW'));
end

%treat mono van data
% scale factor is multiplied by the binsize for integrating purposes
cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_int',indat.mono_van_int_lim,'det_map',mv_mapfile,'det_mask',mask,...
    'det_units',indat.mv_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
    'corr','abs',1,'mass',indat.vmass,'RMM',indat.vrmm,'background',indat.background,'mon_map',mon_map);
monvan=homer(DSOc_mono,cellout{:});

val_abs=monovan_abs(monvan);
disp(sprintf('Absolute normalisation scale from vanadium integration is %g ',val_abs));