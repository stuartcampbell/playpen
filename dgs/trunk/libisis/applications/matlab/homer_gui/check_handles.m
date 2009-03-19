function [white,absolute,indat2,use_mask]=check_handles(indat,handles)



if isfield(handles,{'white_file'})
    if(size(handles.white_file,1) == size(indat.white_file,1))
        if(handles.white_file == indat.white_file)
            white=false;
        else
            white=true;
        end
    else
        white=true;
    end
else
    white=true;
end

if isfield(handles,{'white_file_mono'}) & isfield(handles,{'run_num_mono'}) & ...
        isfield(handles,{'mv_mapfile'}) & isfield(handles,{'mv_mask'})
    if  (size(handles.white_file_mono,1) == size(indat.white_file_mono,1)) ...
            &  (size(handles.run_num_mono,1) == size(indat.run_num_mono,1))
        if (handles.white_file_mono == indat.white_file_mono) & (handles.run_num_mono == indat.run_num_mono) & ...
            strcmp(handles.mv_mapfile,indat.mv_mapfile) & (handles.mv_mask== indat.mv_mask)
        % everything is the same and can use stored monovanadium whitebeam
        % runfiles
            absolute=false;
            indat2=[];
            use_mask=false;
        else
            absolute=true;
            indat2=indat;
            indat2.white_file=indat.white_file_mono;
            indat2.run_num=indat.run_num_mono;
            if indat2.mv_mask==1
                indat2.hardmask='';
                use_mask=false;
            else
                use_mask=true;
            end
        end
    else
        absolute=true;
        indat2=indat;
        indat2.white_file=indat.white_file_mono;
        indat2.run_num=indat.run_num_mono;
        if indat2.mv_mask==1
            indat2.hardmask='';
            use_mask=false;
        else
            use_mask=true;
        end
    end
else
    absolute=true;
    indat2=indat;
    indat2.white_file=indat.white_file_mono;
    indat2.run_num=indat.run_num_mono;
    if indat2.mv_mask==1
        indat2.hardmask='';
        use_mask=false;
    else
        use_mask=true;
    end
end

if isempty(handles.indat.white_file)
    
end
if isempty(handles.indat.run_num)
    error('ERROR: fill in run numor')
end


if( handles.indat.do_absolute==1)
    if isempty(handles.indat.run_num_mono)
        error('ERROR: fill in mono vanadium run numor')
    end
    if isempty(handles.indat.white_file_mono')
        error('ERROR: fill in absolute units vanadium numor')
    end
    if isempty(handles.indat.samp_mass)
        error('ERROR: fill in Sample Mass')
    end
    if isempty(handles.indat.samp_rmm)
        error('ERROR: fill in Sample RMM')
    end
    a=str2num(get(handles.edit12,'string'));
    b=str2num(get(handles.edit14,'string'));
    if isempty(a) || isempty(b)
        error('ERROR: mono vanadium integration limits must be filled in')
    end
    indat2.mono_van_int_lim(1)=a;
    indat2.mono_van_int_lim(2)=b;
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% handles.run_num_mono=    indat.run_num_mono;
% handles.white_file_mono =   indat.white_file_mono;
% handles.white_file=    indat.white_file;
