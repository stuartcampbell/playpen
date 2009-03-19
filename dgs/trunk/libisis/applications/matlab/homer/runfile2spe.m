function runfile2spe(rfile,fname,varargin)
% Create spe file from runfile object, and optionally open in mslice

% Package data for the write routine
rf=rfile.det_data.datasets(1);

data.S=rf.signal;
data.ERR=rf.error;
data.en=rf.x';

% Send data to mslice, if requested
if nargin==3 && varargin{1}==1

    % Determine if mslice is running, and try to open if it is not
    if isempty(findobj('Tag','ms_ControlWindow')),
        disp('Mslice control window not active. Saving spe file anyway...');
        put_spe(data,translate_write(fname));
        return
    end
    
    % Get phx file data, and check compatibility with data
    h_dir=findobj('Tag','ms_PhxDir');
    h_file=findobj('Tag','ms_PhxFile');
    phx_file=strcat(get(h_dir,'String'),get(h_file,'String'));
    if isempty(h_dir)|| isempty(h_file) || ~ischar(phx_file)
        disp('Phx file not set in mslice. Saving spe file anyway...')
        put_spe(data,translate_write(fname));
        return
    end

    try
        det=get_phx(phx_file);
    catch
        disp('Problems reading .phx file - check it exists. Saving spe file anyway...')
        put_spe(data,translate_write(fname));
        return
    end
    
    if size(data.signal,2)~=size(det.phi,2)     % phx file inconsistent with spe file
        disp('Phx file not set in mslice. Saving spe file anyway...')
        put_spe(data,translate_write(fname));
        return
    end
    
    % Package the data for mslice
    data_ms.S  =rf.signal';
    data_ms.ERR=rf.error';
    data_ms.en =(rf.x(2:end)+rf.x(1:end-1))/2;
    data_ms.det_theta=det.phi'*(pi/180);
    data_ms.det_group=det.group';
    [filepath,filename,fileext]=fileparts(translate_write(fname));
    data_ms.filename=[filename,fileext];
    data_ms.filedir=[filepath,filesep];
    data_ms.total_ndet=size(det.phi,2);
    data_ms.det_psi=det.azim'*(pi/180);
    data_ms.det_dtheta=det.dphi'*(pi/180);
    data_ms.det_dpsi=det.danght'*(pi/180);
    [filepath,filename,fileext]=fileparts(phx_file);
    data_ms.detfilename=[filename,fileext];
    data_ms.detfiledir=[filepath,filesep];
    towindow(data_ms)
    
end

% Save data to file
tic;
put_spe(data,translate_write(fname));
toc;

    
