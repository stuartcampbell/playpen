function workspace = IXTworkspace( varargin )
% ! Create an IXTworkspace object
% ! REQUIRED INPUT PARAMETERS
% ! workspace  = IXTworkspace(base, work_no, IXTeffdet_index, IXTdetector); 
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXTworkspace
% ! =========================
% ! NeXus class: NXinstrument
% !
% !	base			IXTbase				Name of entry in NeXus file
%   work_no         int
%   effdet_index    IXTeffdet_index
%   detector        IXTdetector
% ! Notes:
% ! ------
% !	Mainly follows the NeXus standard, except that we use type workspace to store info about the detectors that make
% ! up a single spectrum.
% !	
    % if no input arguments, create a default object
    workspace.base = IXTbase;
    workspace.work_no = [int32(1)];
    workspace.effdet_index = IXTeffdet_index;
    workspace.eff_det = IXTdetector;
    workspace = class(workspace,'IXTworkspace');
if (nargin > 0)
    workspace = libisisexc('IXTworkspace','create',workspace,varargin);
end
