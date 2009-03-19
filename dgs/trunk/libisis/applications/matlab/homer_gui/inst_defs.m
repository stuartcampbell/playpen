function indat=inst_defs(instname)
% returns an object with default parameters defined for the input
% instrument
% 
% >> indat = inst_defs(instname)
%
% instname may be any instrument name for which there is a text file
% located in 
%
% instrument_setup/<instname>.txt
%
% that defines the defaults for that instrument
%
% short names for the following instruments are also accepted if their long
% names exist as a text file in the instrument_setup folder
%
% Instrument Name       Text File Name       Short Name
%---------------------------------------------------------
%  MARI                     mari.txt            mar
%  MAPS                     maps.txt            map
%  MERLIN                   merlin.txt          mer
%



switch instname
    case 'mar'
        instname = 'mari';
    case 'map'
        instname = 'maps';
    case 'mer'
        instname = 'merlin';
    otherwise
end


fid = fopen([ instname, '.txt']);

if fid < 0   % Check file exists
    error('Instrument setup file does not exist')
end

while 1
    tline = fgetl(fid); % get line of text

    if ~ischar(tline), break, end  % break at end of file (tline = -1)

    tline = strtrim(tline); % remove trailing blanks
    datfield = ''; % make datfield and value blank by default.
    value = '';

    if ~isempty(tline) && ~strcmp(tline(1),'%')  % a % indicates comments, blank indicates line break.

        for i = 1:length(tline)   % search for the = sign,
            % use a loop instead of vectors because this is designed to be compiled
            if strcmp(tline(i),'=')

                % every thing to the left of the equals sign is the field,
                % to the right is the value
                datfield = tline(1:(i-1));
                value = tline((i+1):end);

                % remove any trailing blanks form the fields.
                value = strtrim(value);
                datfield = strtrim(datfield);

                %check through the datfield for further subfields
                datasubs = {};
                k = 0;
                for j = 1:length(datfield)
                    if strcmp(datfield(j-k),'.')
                        subfield = datfield(1:(j-k-1));      % take the left field
                        datfield = datfield((j-k+1):end);     % remove from datafield
                        datasubs = {datasubs{:}, subfield};  % add field to the cell array
                        k = j;
                    end
                end
                datasubs = {datasubs{:}, datfield};        % final field to be added


                % user may have put a , or ; at the end of the line
                if strcmp(value(end),';') || strcmp(value(end),',')
                    value(end) = [];  %remove the last character
                end

                value_num = str2num(value);                 % have a numerical equivilent to value ready.

                break           % no need to continue loop after = is found
            end
        end
        if isempty(value_num) && ~strcmp(value, '[]')   % the value is neither blank nor a number
            % remove any quotes from the string. 
            if strcmp(value(1), '''')
                value(1) = [];
            end
            if strcmp(value(end),'''')
                value(end)=[];
            end
        else
            value = value_num;                           % the value should be the numerical equivilent if possible
        end

        indat_temp = [];

        if ~ isempty(datasubs) && ~isempty(datasubs{1})
            indat_temp.(datasubs{end}) = value;      % put value into final field

            for j = 1:(length(datasubs)-1)      % fill the next parts
                index = length(datasubs) - j;
                indat_temp.(datasubs{index}) = indat_temp;   % get the next level
                indat_temp = rmfield(indat_temp, datasubs{index+1}); % delete the previous level
            end
            indat.(datasubs{1})=indat_temp.(datasubs{1});
        end
    end
end


fclose(fid);

% Now run some checks that fields that should be there are there.

fid = fopen('required_fields.txt'); % open the required fields file

if fid < 0   % Check file exists
    error('Instrument setup file does not exist')
end

while 1
    tline = fgetl(fid);                             % get line of text

    if ~ischar(tline), break, end                   % break at end of file (tline = -1)
    tline = strtrim(tline);                         % remove blanks

    if ~isempty(tline) && ~strcmp(tline(1),'%')        % a % indicates comments, blank indicates line break.

                if strcmp(tline(end),';') || strcmp(tline(end),',')
                    tline(end) = [];                % remove the last character
                end
                                                    % check the required field is a field in
                                                    % indat.
        if ~isempty(tline) && ~isfield(indat, tline)
            error(['Required field, ''' tline ''', not found in setup file'])
        end

    end
end

fclose(fid);
