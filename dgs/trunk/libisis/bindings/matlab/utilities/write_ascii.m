function [ok, mess] = write_ascii (var, fid, var_name)
% Write a variable to a currently open text file
%   >> [ok, mess] = write_ascii (var, fid, var_name)
%
%   var         Variable to be written (numeric array, cell array of strings or
%              character array)
%   fid         File identifier of an open ascii file (mode 'wt')
%   var_name    [Optional] name of variable
%
%   ok          If no problems then ok = 1; otherwise ok=0
%   mess        Contains error message if ok=0
%
% Examples of output:
%   >> write_ascii ([34.3, 76], fid, 'range')
%       range = 34.3 76
%   >> write_ascii ({'first line'; 'second line'}, fid, 'title')
%       title = first line
%       title = second line
% Notes:
% - if var_name is '', then "var_name = " is omitted
% - arrays are written so that the first dimension is the row number, the
%   second dimension is the column number, and higher dimensions are written
%   in sequence, with the 3rd cycling most rapidly, 4th next most etc.

% written by T.G. Perring, Modified by: D.J. Whittaker 30/10/2006

ok = 1;
mess = [];

% Check input arguments
if nargin < 2
    ok = 0;
    mess = 'Insufficient parameters passed to write_ascii';
end

if ~isnumeric(fid)
    ok = 0;
    mess = 'File ID passed to write_ascii is non-numeric';
    return
else
    [file, mode] = fopen(fid);
    if length(mode)==1 % for unix 'wt' does not exist
        if isempty(file)||~strcmpi(mode,'w')
            ok = 0;
            mess = 'Check file is open and its mode is "w"';
            return
        end
    elseif  length(mode)==2
        if isempty(file)||~strcmpi(mode,'wt')
            ok = 0;
            mess = 'Check file is open and its mode is "wt"';
            return
        end
    end
end

if nargin==2
    named = 0;
else
    if ~isa_size(var_name,'row','char')
        ok = 0;
        mess = 'Argument name passed to write_ascii must be a character string';
        return
    else
        named = 1;
        name = [deblank(var_name),' = '];
    end
end

% Now write var to ascii file

if isempty(var) && named % write out label if present, otherwise do nothing
    fprintf (fid, '%s\n', name);

elseif iscellstr(var)
    var = reshape(var,prod(size(var)),1); % reshape to be column of elements
    if named
        for i=1:size(var)
            fprintf (fid, '%s%s\n', name, var{i});
        end
    else
        for i=1:size(var)
            fprintf (fid, '%s\n', var{i});
        end
    end

elseif isa(var,'char')
    dims = size(var);
    if length(dims)>2  % multi-dimensional character array; 2nd dimension is the string
        var = reshape(var,dims(1),dims(2),prod(dims(3:end)));   % reshape to be array of matricies
        if named
            for j=1:prod(dims(3:end))
                for i=1:dims(1)
                    fprintf (fid, '%s%s\n', name, var(i,:,j));
                end
            end
        else
            for j=1:prod(dims(3:end))
                for i=1:dims(1)
                    fprintf (fid, '%s\n', var(i,:,j));
                end
            end
        end
    else
        if named
            for i=1:dims(1)
                fprintf (fid, '%s%s\n', name, var(i,:));
            end
        else
            for i=1:dims(1)
                fprintf (fid, '%s\n', var(i,:));
            end
        end
    end

elseif isnumeric(var) || islogical(var)
    dims = size(var);
    fmt = [repmat('%-25.16g',1,dims(2)),'\n'];
    if length(dims)>2  % multi-dimensional array; layout as pages of matricies
        var = reshape(var,dims(1),dims(2),prod(dims(3:end)));   % reshape to be array of matricies
        if named
            fmt = ['%s',fmt];
            for j=1:prod(dims(3:end))
                for i=1:dims(1)
                    fprintf (fid, fmt, name, var(i,:,j));
                end
            end
        else
            for j=1:prod(dims(3:end))
                for i=1:dims(1)
                    fprintf (fid, fmt, var(i,:,j));
                end
            end
        end
    else
        if named
            fmt = ['%s',fmt];
            for i=1:dims(1)
                fprintf (fid, fmt, name, var(i,:));
            end
        else
            for i=1:dims(1)
                fprintf (fid, fmt, var(i,:));
            end
        end
    end

else
    ok = 0;
    mess = 'write_ascii can only write numeric or character arrays, or cell arrays of characters';
    return
end
