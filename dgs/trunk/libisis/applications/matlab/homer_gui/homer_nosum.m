function [data_out]=homer_nosum(indat,mask,wbrf)
%%routine that creates a corrected, reduced data object
% requires indat data structure from the Homer GUI

DSOc_mono=  setup_DSO_bank(indat.inst); % monochromatic vanadium data source
DSOc=       setup_DSO_bank(indat.inst); % monochromatic data source sample

DSOc_mono=  add_chopinst(DSOc_mono);
DSOc_mono=  add_item(DSOc_mono,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',indat.choppertype);
DSOc=       add_chopinst(DSOc);
DSOc=       add_item(DSOc,'inst_nxs:::fermi_chopper.nxs','fermi_chopper',indat.choppertype);

mapfile=strcat('inst_maps:::',indat.map_file);
mv_mapfile=strcat('inst_maps:::',indat.mv_mapfile);



if indat.do_absolute==1
    %% Mono vanadium normalisation section is a separate unit that calculates the val_abs parameter

    %need to give this diag the whitebeam file for the mono run adjust indat
    %accordingly to indat2
    indat2=indat;

    indat2.white_file=indat.white_file_mono;
    [wbrf1_mono,wbrf2_mono]=do_white(indat2);

    if  indat2.mv_mask ==1
        indat2.run_num=indat.run_num_mono;
        % i don't like this but it'll have to do for the moment until spec numbers
        % for mid-tubes.map are resolved
        if strcmp(indat2.inst,'MAP')
            indat2.hardmask='';
        end

        [mask_mono]=do_diag2(indat2,wbrf1_mono,wbrf2_mono,false);

    else
        mask_mono=mask;
    end

    %treat mono van data
    % scale factor is multiplied by the binsize for integrating purposes
    cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_int',indat.mono_van_int_lim,'det_map',mv_mapfile,'det_mask',mask_mono,...
        'det_units',indat.mv_units,'solid',wbrf1_mono,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
        'corr','abs',1,'mass',indat.vmass,'RMM',indat.vrmm,'background',indat.background);
    monvan=homer(DSOc_mono,indat.run_num_mono,cellout{:});


    val_abs=monovan_abs(monvan);
    disp(sprintf('Absolute normalisation scale from vanadium integration is %g ',val_abs));
    %% Treat runfile
    cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
        'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
        'corr','abs',val_abs,'mass',indat.samp_mass,'RMM',indat.samp_rmm,'background',indat.background);
    data_out= homer(DSOc,indat.run_num,cellout{:});
else
    %
    %% if NO absolute normalisation
    % Treat runfile
    cellout = pre_parse(2,indat.energy_method,indat.ei_init,'d_rebin',indat.spe_rebin_lims,'det_map',mapfile,'det_mask',mask,...
        'det_units',indat.d_units,'solid',wbrf,'normalisation',indat.norm_method,'range',indat.range,'scale',indat.scalefactor,...
        'corr','background',indat.background);
    data_out= homer(DSOc,indat.run_num,cellout{:});
end
