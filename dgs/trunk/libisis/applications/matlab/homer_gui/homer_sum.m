function [data_out]=homer_sum(indat,mask,wbrf)
%%routine that creates a corrected, reduced data object
% requires indat data structure from the Homer GUI



DSOc_mono=  setup_DSO_bank(indat.inst); % monochromatic vanadium data source
DSOc=       setup_DSO_bank(indat.inst); % monochromatic data source sample
DSOc_mono=  add_chopinst(DSOc_mono);
DSOc=       add_chopinst(DSOc);

DSOc_mono=  add_item(DSOc_mono,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',indat.choppertype);
DSOc     =  add_item(DSOc,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',indat.choppertype);

mapfile=strcat('inst_maps:::',indat.map_file);



mv_mapfile=strcat('inst_maps:::',indat.mv_mapfile);
if indat.do_absolute ==1
    %% Mono vanadiaum normalisation section is a separate unit that calculates the val_abs parameter
    %Diag section diag on white_file and the mono vanadium run number

    %need to give this diag the whitebeam file for the mono run adjust indat
    %accordingly to indat2
    indat2=indat;   

    indat2.white_file=indat.white_file_mono;
    [wbrf1_mono,wbrf2_mono]=do_white(indat2);


    
    if    indat2.mv_mask ==1    
    indat2.run_num=indat.run_num_mono;
% i don't like this but it'll have to do for the moment until getei spec numbers
% for mid-tubes.map are resolved
    if strcmp(indat2.inst,'MAP')
        indat2.hardmask='';
    end
    
    [mask_mono]=do_diag2(indat2,wbrf1_mono,wbrf2_mono,false);
   
else
    mask_mono=mask;
end
    %treat mono van data    
    cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_int',indat.mono_van_int_lim,'det_map',mv_mapfile,'det_mask',mask_mono,...
        'det_units',indat.mv_units,'solid',wbrf1_mono,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
        'corr','abs',1,'mass',indat.vmass,'RMM',indat.vrmm,'background',indat.background);
    monvan=homer(DSOc_mono,indat.run_num_mono,cellout{:});
       
    val_abs=monovan_abs(monvan);
    disp(sprintf('Absolute normalisation scale from vanadium integration is %g ',val_abs));

    %% Treat runfile
    if indat.sum_files == 1
        L=size(indat.run_num);
        L=L(1);
        for i=1:L
            DSOc=add_multrawfile(DSOc,strcat('inst_data:::',indat.inst,indat.run_num(i,:),'.RAW'));
        end
        cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
            'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,'corr',...
            'abs',val_abs,'mass',indat.samp_mass,'RMM',indat.samp_rmm,'background',indat.background);
        data_out= homer(DSOc,cellout{:});
    end
else
    
    %% No absolute normalisation
    %% Treat runfile
    if indat.sum_files == 1
        L=size(indat.run_num);
        L=L(1);
        for i=1:L
            DSOc=add_multrawfile(DSOc,strcat('inst_data:::',indat.inst,indat.run_num(i,:),'.RAW'));
        end
        cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
            'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
            'corr','background',indat.background);
        data_out= homer(DSOc,cellout{:});
    else
        cellout=pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
            'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
            'corr','background',indat.background);
        data_out= homer(DSOc,indat.inst,indat.run_num,cellout{:});
    end

end


