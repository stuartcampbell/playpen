function [data, argout] = ixf_parse_plotdata(outtype, data, argin)
% parse arguments for plotdata, used with the plot commands to convert data
% and arguments into a format compatable with the underlying commands
%
% [dout, argout] = ixf_parse_plotdata(outtype, din, argin)
%
%
% inputs:
%
%       outtype:     STRING gives the output dataset type required - may be
%                   'IXTdataset_1d' or 'IXTdataset_2d'
%
%       din:        input dataset, may be IXTdataset_1d, IXTdataset_2d or
%                   IXTrunfile.
%
%       argin:      CELL ARRAY - other inputs to the plot functions
%                   (property-value pairs, xlimits etc.)
%
% outputs:
%
%       dout:       Output dataset of class outtype, converted if required
%
%       argout:     CELL ARRAY - Contains only property-value pairs.
%                   These will be consistent with the ui plot commands.
%
% This function assumes that inputs are CORRECT. For instance, if a user
% wishes to plot an IXTdataset_2d and gives the 'det' or 'mon' string after
% the dataset, then the plot will fail further down the line.
%
% NOTE: if the first arguments to argin are numbers then it assumes x and y
% limits but it does NOT assume z limits since this will be different for
% area, surface etc. (i.e. colour limits instead of z limits etc.)

% Dean Whittaker 04 april 2008


% sort data first
if ~isa(data, outtype)

    switch outtype          % look at the output type required
        case 'IXTdataset_1d'    % Convert where necessary
            if isa(data, 'IXTdataset_2d')
                data = ixf_twod_to_oned(data);
            elseif isa(data,'IXTrunfile')
                % note that when plotting IXTrunfile, an extra 'det' or 'mon'
                % command may be given.
                if ~isempty(argin)
                    if ischar(argin{1}) && strcmp(argin{1}, 'det')
                        data= data.det_data.datasets;
                        argin(1)=[];        % need to remove the text from the input arguments
                        data = ixf_twod_to_oned(data);
                    elseif ischar(argin{1})&&strcmp(argin{1},'mon')
                        data = data.mon_data.datasets;
                        argin(1)=[];
                        data = ixf_twod_to_oned(data);
                    else
                        data = data.det_data.datasets;
                        data = ixf_twod_to_oned(data);
                    end
                else
                    data = data.det_data.datasets;
                    data = ixf_twod_to_oned(data);
                end
            elseif isnumeric(data)
                data = getspectrum(data);
            else
                error(['Incorrect input dataset type'])
            end
        case 'IXTdataset_2d'        % convert where necessary
            if isa(data,'IXTdataset_1d')
                data=ixf_oned_to_twod(data);
            elseif isa(data, 'IXTrunfile')
                if ~isempty(argin)
                    if ischar(argin{1}) && strcmp(argin{1}, 'det')
                        data = data.det_data.datasets;
                        argin(1)=[];
                    elseif ischar(argin{1})&&strcmp(argin{1},'mon')
                        data = data.mon_data.datasets;
                        argin(1)=[];
                    else
                        data = data.det_data.datasets;
                    end
                else
                    data = data.det_data.datasets;
                end
            elseif isnumeric(data)
                data = getspectra(data);
            else
                error('Incorrect input dataset type')
            end
        otherwise
            error(['Can not output datasets of the type  ' outtype '!'])

    end
end

% Now sort the input arguments

tot = numel(argin); % total elements in input

if ( tot == 2) &&(isnumeric(argin{1}) && isnumeric(argin{2}))
    argout = {'xlim', [argin{1},argin{2}]};
elseif (tot == 4)&&(isnumeric(argin{1}) && isnumeric(argin{2}) && isnumeric(argin{3}) && isnumeric(argin{4}))
    argout = {'xlim', [argin{1},argin{2}] 'ylim', [argin{3},argin{4}]};
else
    argout = argin;
end