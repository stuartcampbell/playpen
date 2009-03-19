function varargout = ass( number )
% mgenie style ass command for libisis. Takes the instrument, extension, and 
% directory settings from set_dir, set_ext, set_inst and number uses them to open
% the corresponding RAW file (i.e. run). This can then be read into a rawfile object
% OR is stored internally. When functions are called without a rawfile
% object, it is this default object that will be used.
%
% Alternaltively, the full path and name of the rawfile can be given.
% 
% >> ass(number)
% >> rawfile = ass(number)
% >> ass(filename)
%
% inputs
%------------
%
%   number:       real          number of the rawfile to get (i.e. run number)
%   filename:     char          filename of RAW file to store as default.
%
% outputs
%------------
%
%   rawfile       IXTrawfile (optional)     rawfile object containing
%                                           information from the RAW file
% none
%
% example: 
%           >> ass('c:/maps_files/rawfiles/MAP11012.RAW')
%           >> myraw = ass(11012)

if ischar(number)
    rawfile_name = number;
else
    [IXG_dir, IXG_inst, IXG_ext] =  ixf_global_var('data_source','get','path','inst', 'ext');

    if isempty(IXG_ext)  % ext not so important, use a default
        warning('Extension not found, using RAW by default');
        IXG_ext = 'RAW';
    end

    if isempty(IXG_dir)  % checks
        error('directory not set, use set_dir to rectify this')
    elseif isempty(IXG_inst)
        error('instrument not set, use set_inst to rectify this')
    end

num_str = num2str(number);
xlen = 5 - length(num_str);

if xlen > 0
num_str = [repmat('0', xlen, 1) num_str];
end

    rawfile_name = [IXG_dir, filesep, IXG_inst, num_str, '.', IXG_ext];      % condense text

end
rawfile = IXTraw_file(rawfile_name);         % make rawfile


if nargout == 1
    varargout{1} = rawfile;
else
    ixf_global_var('data_source','set','rawfile', rawfile)
     ixf_global_var('data_source','set','rawfile_no', num_str)
end