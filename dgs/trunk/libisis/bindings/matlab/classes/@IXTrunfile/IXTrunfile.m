function runfile = IXTrunfile( varargin )
% ! Create an ISISEXCchopper_instrument object
% ! REQUIRED INPUT PARAMETERS
% ! chopper_instrument  = ISISEXCchopper_instrument( [entry_name], [name], [source], 
%                                                    [moderator], [monochromator], 
%                                                    [aperture], [attenuator],
%                                                    [workspaces]);
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTrunfile
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
%     title   char
%     users  IXTuser
%     sample  IXTsample
%     inst  IXTchopper_instrument
%     det_data  IXTdata
%     mon_data  IXTdata
%     peaks  IXTpeaks
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	

    % if no input arguments, create a default object
    runfile.base = IXTbase;
    runfile.title = ' ';
    runfile.start_time= ' ';
    runfile.end_time = ' ';
    runfile.run_number = int32(0);
    runfile.total_charge= 0.0;
    runfile.total_raw_frames = int32(0);
    runfile.total_good_frames = int32(0);
    runfile.program_name= IXThistory;
    runfile.command_line = IXThistory;
    runfile.users = [IXTuser];
    runfile.sample = IXTsample;
    runfile.inst = IXTinstrument;
    runfile.det_data = IXTdata;
    runfile.mon_data = IXTdata;
    runfile.peaks = IXTpeaks;
    runfile = class(runfile,'IXTrunfile');
if (nargin > 0)
    runfile = libisisexc('IXTrunfile','create',runfile,varargin);
end
