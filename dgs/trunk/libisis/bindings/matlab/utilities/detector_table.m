function varargout = detector_table (det_generator_file, calibration_file, varargin)
% Generate ISIS detector.dat file from generator file and`calibration file
% 
% detector = detector_table (det_input_file,calibration_file,det_output_file)
%
% if a calibration file is not specified a blank string must be defined
%
% optional output:
% detector     IXTdetector object
% det_output_file  string defining output to a traditional detector.dat file
%
%   >> detector = detector_table ('det.txt','calib.txt', 'det.dat')
%
%
% it is not necessary to translate the input as this is done in the Fortran 
% DLL
if (isempty(det_generator_file))
    error ('No generator input file given')
end

if (nargout == 0) && (nargin ==2)
    error('no output file or IXTdetector object specified')
end
if nargout == 1
    if nargin ==3
        det_out=libisisexc('IXTdetector','filecreator',det_generator_file, calibration_file, '',varargin{1}, '',IXTdetector);
    else
        det_out=libisisexc('IXTdetector','filecreator',det_generator_file, calibration_file, '','', '',IXTdetector);
    end
    varargout(1) = {det_out};
else
    if nargin ==3
        libisisexc('IXTdetector','filecreator',det_generator_file, calibration_file, '',varargin{1}, '');
    else
        libisisexc('IXTdetector','filecreator',det_generator_file, calibration_file, '','', '');
    end
end