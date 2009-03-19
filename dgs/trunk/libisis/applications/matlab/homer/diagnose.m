function [mask,varargout]=diagnose(dso,run_no,rf_v1,varargin)
% function [mask,cause]=diagnose(dso,run_no,rf_v1,var_argin)
% run_no is the run_number, if multiple run_no are provided then they will
% be summed together
%  rf_v1: /V1  standard white beam vanadium run file
%  input for runfiles can be IXTrunfile object or nexus file ONLY
%  input for hmask and bank can be IXTmap object, ascii file or nexus file
%     'tiny',1e-10,...  %to reproduce VMS diag (check not made)
%     'huge',1e10,...   %to reproduce VMS diag (check not made)
%     'bmin',[],...     %/BMIN flag
%     'bmax',[],...     %/BMAX
%     's_zero',false,... %/ZERO_BKGD flag
%     's_out_lo',0.0,... %to reproduce VMS diag (check not made)
%     's_out_hi',100, ... %to reproduce VMS diag (check not made)
%     'sv_lo',0, ... %to reproduce VMS diag (check not made)
%     'sv_hi',2, ...  %/FACTOR flag
%     'sv_sig',3.3,... % to reproduce VMS diag (hard-wired)
%     'v_zero',true,... %to reproduce VMS diag (hard-wired)
%     'v_out_lo',0.01,... %to reproduce VMS diag (check not made)
%     'v_out_hi',100, ... %to reproduce VMS diag (check not made)
%     'vv_lo',0.1, ... % /VLOW flag
%     'vv_hi',1.5, ... % /VHIGH flag
%     'vv_sig',0.0,... %to reproduce VMS diag (check not made)
%     %the following v2 variables are not inputs to VMS diag as are
%     hard-wired, but
%     %%%%%%%%%
%     'r',10.0,... %/STABILITY for MAPS is 10, better default might be one
%     'r_sig',0.0,...% to reproduce VMS diag (no check on error value) , a default might be zero also
%     %%% if runfiles have been given as nexus files, then they can also be given
%     %%% an entry if the default value was not used, or if the file contains
%     %%% more than one runfile ...etc...
%     'rfile_entry','',...
%     'rf_v1_entry','',...
%     'rf_v2',IXTrunfile,... /V2
%     'rf_v2_entry','',...
%     'bank',IXTmap,...  removes hardwired info about instrument detector 'banks'
%     'bank_entry','',...
%     'hardmask',IXTmask,...   /HARD
%     'hmask_entry','',...
% arguments concerning output
%     'out_nex','out.nxs',...
%     'out_nex_entry','',...
%     'out_asc','out.msk',...
%     'summary',true,...
%     'messages',false

arglist = struct(... % argument names and default values
    'tiny',1e-10,...
    'huge',1e10,...
    'bmin',[],...
    'bmax',[],...
    's_zero',false,...
    's_out_lo',0.0,...
    's_out_hi',100, ...
    'sv_lo',0, ...
    'sv_hi',[], ...
    'sv_sig',3.3,...
    'v_zero',true,...
    'v_out_lo',0.01,...
    'v_out_hi',100, ...
    'vv_lo',[], ...
    'vv_hi',[], ...
    'vv_sig',0.0,...
    'r',[],...
    'r_sig',0.0,...
    'rfile_entry','',...
    'rf_v1_entry','',...
    'rf_v2',IXTrunfile,...
    'rf_v2_entry','',...
    'bank',[],...
    'bank_entry','',...
    'hardmask',[],...
    'hmask_entry','',...
    'out_nex','out.nxs',...
    'out_nex_entry','',...
    'out_asc','out.msk',...
    'summary',true,...
    'messages',false);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parsing of input arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flags = {'s_zero','v_zero','summary','messages'};
[par,arg,present] = parse_arguments(varargin,arglist,flags);

% get defaults if they exist
list=fieldnames(arg);
for i=1:numel(list)
        % get default variable if it exists
        tempvar=ixf_global_var('diag','get',list{i});
        % captures 'no' prefix for all values
        if isempty(arg.(list{i}))
            if present.(list{i})
                present.(list{i})=0;
            else
                if ~isempty(tempvar)
                    arg.(list{i})=tempvar;
                    present.(list{i})=1;
                end
            end
        end   
end
%%%% output object
mask=IXTmask;
%rfile can be file or object
disp('----------------------------------------------------------------------------------')
if ischar(run_no) || iscellstr(run_no)
    dso=add_diffinst(dso);   
    rf_ZC=homer(dso,run_no,'d_int',[NaN('double'),NaN('double')]);
    mono_sam=homer(dso,run_no,'d_int',[arg.bmin,arg.bmax]);
else
    error('bad run_no input to diag');
end


if ischar(rf_v1) % read from file
    disp(['First vanadium normalisation file is: ',rf_v1])
    if present.rf_v1_entry
        if ischar(arg.rf_v1_entry)
            rf_v1=read_nxs(rf_v1,IXTrunfile,arg.rf_v1_entry);
        else
            disp(' bad rf_v1_entry input to diag')
        end
    else
        rf_v1=read_nxs(rf_v1,IXTrunfile,'');
    end
else % make sure it is correct object
    if ~isa(rf_v1,'IXTrunfile')
        disp('bad rf_v1 input to diag');
        return
    end
end

if present.rf_v2
    if ischar(arg.rf_v2) % read from file
        disp(['Second vanadium normalisation file is: ',arg.rf_v2])
        if present.rf_v2_entry
            if ischar(arg.rf_v2_entry)
                arg.rf_v2=read_nxs(arg.rf_v2,IXTrunfile,arg.rf_v2_entry);
            else
                disp(' bad rf_v2_entry input to diag')
            end
        else
            arg.rf_v2=read_nxs(arg.rf_v2,IXTrunfile,'');
        end
    else % make sure it is correct object
        if ~isa(arg.rf_v2,'IXTrunfile')
            disp('bad rf_v2 input to diag');
            return
        end
    end
end


if present.bank
    if ischar(arg.bank) % read from file
        disp(['Spectrum bank description file is: ',arg.bank])
        if present.bank_entry
            if ischar(arg.bank_entry)
                arg.bank=read_nxs(arg.bank,IXTmap,arg.bank_entry);
            else
                disp(' bad bank_entry input to diag')
            end
        else
            if filetype(arg.bank) ==1 %ascii read of mapfile
                arg.bank=fileread(IXTmap,arg.bank);
            else
                arg.bank=read_nxs(arg.bank,IXTmap,'');
            end
        end
    else % make sure it is correct object
        if ~isa(arg.bank,'IXTmap')
            disp('bad bank input to diag');
            return
        end
    end
end
tot_hmask=0;
if present.hardmask
    if ischar(arg.hardmask) % read from file
        disp(['Spectrum hard-masking file is: ',arg.hardmask])
        if present.hmask_entry
            if ischar(arg.hmask_entry)
                arg.hardmask=read_nxs(arg.hardmask,IXTmask,arg.hmask_entry);
            else
                disp(' bad hmask_entry input to diag')
            end
        else
            if filetype(arg.hardmask) ==1 %ascii read of mapfile
                arg.hardmask=fileread(IXTmask,arg.hardmask);
            else
                arg.hardmask=read_nxs(arg.hardmask,IXTmap,'');
            end
        end
    else % make sure it is correct object
        if ~isa(arg.hardmask,'IXTmask')
            error('bad hardmask input to diag');            
        end
    end
    tot_hmask=length(arg.hardmask.mask_array);    
end
disp('----------------------------------------------------------------------------------')
[mask,cause_code,cause_value,cause_error,cause_file]=libisisexc('IXTrunfile','diag',IXTmask,mono_sam,rf_v1,arg.tiny,arg.huge,...
    arg.s_zero,arg.s_out_lo,arg.s_out_hi,arg.sv_lo,arg.sv_hi,arg.sv_sig,...
    arg.v_zero,arg.v_out_lo,arg.v_out_hi,arg.vv_lo,arg.vv_hi,arg.vv_sig,...
    arg.r,arg.r_sig,arg.rf_v2,arg.bank,arg.hardmask,rf_ZC);

cause.cause=cause_code;
cause.value=cause_value;
cause.error=cause_error;
cause.file=cause_file;

total_masked=length(mask.mask_array);
j=0;
if arg.messages
    %high integral
    for i=1:total_masked
        if cause.cause(i)==5
            if (cause.file(i) == 1 | cause.file(i) == 2)
                disp(['Low vanadium integral in spectrum ', num2str(mask.mask_array(i)),' : ',num2str(cause.value(i)),' +/- ',num2str(cause.error(i)),' v',num2str(cause.file(i))]);
            else
                disp(['Low background in spectrum ', num2str(mask.mask_array(i)),' : ',num2str(cause.value(i)),' +/- ',num2str(cause.error(i))]);
            end
            j=j+1;
        end
        if cause.cause(i)==6
            if (cause.file(i) == 1 | cause.file(i) == 2)
                disp(['High vanadium integral in spectrum ', num2str(mask.mask_array(i)),' : ',num2str(cause.value(i)),' +/- ',num2str(cause.error(i)),' v',num2str(cause.file(i))]);
            else
                disp(['High background in spectrum ', num2str(mask.mask_array(i)),' : ',num2str(cause.value(i)),' +/- ',num2str(cause.error(i))]);
            end
            j=j+1;
        end
        if cause.cause(i)==2
            if cause.file(i) == 1 | cause.file(i) == 2
                disp(['Zero vanadium integral in spectrum ', num2str(mask.mask_array(i)),' :  v',num2str(cause.file(i))]);
            else
                disp(['Zero background in spectrum ', num2str(mask.mask_array(i))]);
            end
            j=j+1;
        end
        if cause.cause(i)==8
            disp(['Instability in spectrum ', num2str(mask.mask_array(i)),' : ',num2str(cause.value(i))]);
            j=j+1;
        end
    end
    if j ~= total_masked - tot_hmask
        disp('ERROR: not all masked spectra have valid reason assigned')
    end
end

%high integral

if arg.summary
    index6_0=[];
    index6_1=[];
    index6_2=[];
    index5_0=[];
    index5_1=[];
    index5_2=[];
    index2_0=[];
    index2_1=[];
    index2_2=[];
    index8=[];
    index10_0=[];
    file0=(find(cause.file==0));
    file1=(find(cause.file==1));
    file2=(find(cause.file==2));

    count1=1;
    count2=1;
    count3=1;
    count4=1;
    for j=1:length(file0)
        if cause.cause(file0(j)) == 6
            index6_0(count1)=file0(j);
            count1=count1+1;
        end
        if cause.cause(file0(j)) == 5
            index5_0(count2)=file0(j);
            count2=count2+1;
        end
        if cause.cause(file0(j)) == 2
            index2_0(count3)=file0(j);
            count3=count3+1;
        end
        if cause.cause(file0(j)) == 10
            index10_0(count4)=file0(j);
            count4=count4+1;
        end
    end
    count1=1;
    count2=1;
    count3=1;
    for j=1:length(file1)
        if cause.cause(file1(j)) == 6
            index6_1(count1)=file1(j);
            count1=count1+1;
        end
        if cause.cause(file1(j)) == 5
            index5_1(count2)=file1(j);
            count2=count2+1;
        end
        if cause.cause(file1(j)) == 2
            index2_1(count3)=file1(j);
            count3=count3+1;
        end
    end
    count1=1;
    count2=1;
    count3=1;
    for j=1:length(file2)
        if cause.cause(file2(j)) == 6
            index6_2(count1)=file2(j);
            count1=count1+1;
        end
        if cause.cause(file2(j)) == 5
            index5_2(count2)=file2(j);
            count2=count2+1;
        end
        if cause.cause(file2(j)) == 2
            index2_2(count3)=file2(j);
            count3=count3+1;
        end
    end

    if present.rf_v2
        index8 = find(cause.cause == 8);
    end

    disp('----------------------------------------------------------------------------------')
    disp('Summary of masked spectra')
    disp('----------------------------------------------------------------------------------')

    disp(' ')
    disp('Vanadium file 1:')
    disp('----------------')
    if ~isempty(index2_1)
        if arg.v_zero
            disp(['Zero Integral : ',num2str(length(index2_1))])
            disp('---------------')
            disp(char(iarray_to_str(mask.mask_array(index2_1))))
            disp(' ')
        end
    end
    if ~isempty(index6_1)
        disp(['High Integral : ',num2str(length(index6_1))])
        disp ('---------------')
        disp(char(iarray_to_str(mask.mask_array(index6_1))))
        disp(' ')
    end
    if ~isempty(index5_1)
        disp(['Low Integral : ',num2str(length(index5_1))])
        disp('--------------')
        disp(char(iarray_to_str(mask.mask_array(index5_1))))
        disp(' ')
    end
    if present.rf_v2
        disp(' ')
        disp('Vanadium file 2:')
        disp('----------------')


        if arg.v_zero
            if ~isempty(index2_2)
                disp(['Zero Integral : ',num2str(length(index2_2))])
                disp('---------------')
                disp(char(iarray_to_str(mask.mask_array(index2_2))))
                disp(' ')
            end
        end
        if ~isempty(index6_2)
            disp(['High Integral : ',num2str(length(index6_2))])
            disp('---------------')
            disp(char(iarray_to_str(mask.mask_array(index6_2))))
            disp(' ')
        end

        if ~isempty(index5_2)
            disp(['Low Integral : ',num2str(length(index5_2))])
            disp('--------------')
            disp(char(iarray_to_str(mask.mask_array(index5_2))))
            disp(' ')
        end

        if ~isempty(index8)
            disp(' ')
            disp('Ratio of vanadium integrals : ')
            disp('-----------------------------')
            disp(['Unstable Spectra:',num2str(length(index8))])
            disp('----------------')

            disp(char(iarray_to_str(mask.mask_array(index8))))
            disp(' ')
        end
    end
    disp(' ')
    disp('Analysis of run:')
    disp('----------------')
    % Specification of TGP is that input to diag is precalculated integrals and
    % therefore zero total counts in spectrum is not tested for, one might
    % assume that this would be accounted for by zero vanadium integrals
    % input: v_zero
    % if it is specifically required, another strategy may be to load
    % monochromatic sample data from the rawfile into a dataset_2d and
    % calculate the integral during the diag process
    %     if length(index10_0) >0
    %         disp(['Zero total counts:',num2str(length(index10_0))])
    %         disp('-----------------')
    %         disp(char(iarray_to_str(mask.mask_array(index10_0))))
    %     end

    if arg.s_zero
        if ~isempty(index2_0)
            disp(['Zero background : ',num2str(length(index2_0))])
            disp('-----------------')
            disp(char(iarray_to_str(mask.mask_array(index2_0))))
            disp(' ')
        end
    end
    if ~isempty(index6_0)
        disp(['High background : ',num2str(length(index6_0))])
        disp('-----------------')
        disp(char(iarray_to_str(mask.mask_array(index6_0))))
        disp(' ')
    end
    % will unlikely ever be tested for, since a low run background is likely
    % and why s_zero is generally false
    if ~isempty(index5_0)
        disp(['Low background : ',num2str(length(index5_0))])
        disp('----------------')
        disp(char(iarray_to_str(mask.mask_array(index5_0))))
        disp(' ')
    end
    if ~isempty(index10_0)
        disp(['Zero Total Counts : ',num2str(length(index10_0))])
        disp('----------------')
        disp(char(iarray_to_str(mask.mask_array(index10_0))))
        disp(' ')
    end

    disp('----------------------------------------------------------------------------------')
    disp(['Initial number of spectra is : ',num2str(length(rf_v1.det_data.datasets(1).y))]);
    if present.hardmask
        
        if tot_hmask ~= 0
            disp(['Number of spectra in hard mask file is : ',num2str(tot_hmask)])
            disp(['Number of additional spectra masked is : ',num2str(total_masked-tot_hmask)]);
        else
            disp('Warning: No spectra defined in hard mask file')
        end
    else
        disp(['Number of spectra masked : ',num2str(total_masked)]);
    end
    disp('----------------------------------------------------------------------------------')
end

%%%%%%%%%%%%%%% file output
if present.out_asc
    disp(['Masked spectra stored in : ',arg.out_asc])
    fid=fopen(translate_write(arg.out_asc),'wt');
    write_ascii(iarray_to_str(mask.mask_array),fid);
    fclose(fid);
end

if present.out_nex || present.out_nex_entry
    disp(['Masked spectra stored in : ',arg.out_nex])
    write_nxs(arg.out_nex,mask,arg.out_nex_entry);
end
if nargout == 2
  varargout{1}={cause};
end

