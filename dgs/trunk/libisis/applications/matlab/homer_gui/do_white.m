function [wbrf_1,wbrf_2]=do_white(indat)
% run diag on a white beam file and a run number
% more than one wb file results in a stability check
% multiple sample runs results in the sum of the runs being sent to diag
% return the mask array and the white beam runfile, for homer to use
% need to switch on the number of whitebeam files

%first set up some objects irrespective of what will happen
 


DSOw_1=setup_DSO_bank(indat.inst);  % primary whitebeam data source
    DSOw_2=setup_DSO_bank(indat.inst); % secondary white beam file
    DSOw_1 =add_diffinst(DSOw_1);
    DSOw_2=add_diffinst(DSOw_2);
    ei_wb='white';
  
    
    % white beam runfiles are created without any detector mapping
if ~isempty(indat.mon_mapfile)
    mon_map=strcat('inst_maps:::',indat.mon_mapfile);
else
    mon_map='';
end
%% check for multiple white beam files    
a=indat.white_file;
aa=size(a);
aaa=aa(1);
%% sort out the wb files 
if aaa >1; %for two wb files
    indat.white_file=a(1,:);
    indat.white_file2=a(2,:);
    DSOw_1=add_rawfile(DSOw_1,strcat('inst_data:::',indat.inst,indat.white_file,'.RAW'));
    DSOw_2=add_rawfile(DSOw_2,strcat('inst_data:::',indat.inst,indat.white_file2,'.RAW'));

    cellout = pre_parse(2,'ei',ei_wb,'det_units',indat.wb_units,'d_int',indat.wb_int,'normalisation',...
        indat.norm_method,'range',indat.range,'scale',1000,'background',indat.background,'mon_map',mon_map);
    wbrf_1= homer(DSOw_1,cellout{:});
    wbrf_2= homer(DSOw_2,cellout{:});
else
    DSOw_1=add_rawfile(DSOw_1,strcat('inst_data:::',indat.inst,indat.white_file,'.RAW'));
    cellout = pre_parse(2,'ei',ei_wb,'det_units',indat.wb_units,'d_int',indat.wb_int,'normalisation',indat.norm_method,...
        'range',indat.range,'scale',1000,'background',indat.background,'mon_map',mon_map);
    wbrf_1= homer(DSOw_1,cellout{:});
    wbrf_2=[];
end
