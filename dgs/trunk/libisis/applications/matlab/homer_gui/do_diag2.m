function [mask]=do_diag2(indat,wbrf_1,wbrf_2,bank)
% run diag on a white beam file and a run number
% more than one wb file results in a stability check
% multiple sample runs results in the sum of the runs being sent to diag
% return the mask array and the white beam runfile, for homer to use
% need to switch on the number of whitebeam files

%first set up some objects irrespective of what will happen
 


if bank
    [dso,bank]=setup_DSO_bank(indat.inst); % monochromatic data source
else
    dso=setup_DSO_bank(indat.inst); % monochromatic data source
    bank=[];
end
        
if indat.sum_files == 1 %sum multiple run numbers
    L=size(indat.run_num,1);
    run_no=cell(1,L);
    for i=1:L
        run_no{i}=indat.run_num(i,:);
    end
else
    run_no{1}=indat.run_num;
end

if ~isempty(indat.hardmask)
    hardmask=strcat('inst_masks:::',indat.hardmask);
else
    hardmask=[];
end
cellout=pre_parse(1,'rf_v2',wbrf_2,'bank',bank,'vv_lo',indat.VLOW,'vv_hi',...
    indat.VHIGH,'sv_hi',indat.FACTOR,'r',indat.STAB,'s_zero',indat.ZERO_BKGD,'hardmask',hardmask,...
    'bmin',indat.background_default(1),'bmax',indat.background_default(2));

mask=diagnose(dso,run_no,wbrf_1,cellout{:});

