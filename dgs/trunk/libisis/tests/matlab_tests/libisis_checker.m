function out = libisis_checker(type, varargin)
%---libisis_checker - checks that tests are correctly running----
%
% >> outcome = libisis_checker('type', varargin)
%
% The input arguments depend on the type. A full list will be given as new
% types are added. Outcome will always be success or fail.
%
% NOTE: First argument must be a single dataset, subsiquent arguments can
%       not be cell arrays. The test script must allow for this
%
% type:     
%   'binary'
%       >> flag = libisis_checker('binary','operator',dataset1, operand, result)
%       Inputs:
%           operator  -    a string with the operator symbol in it
%           dataset1  -    First dataset for operation to be performed on
%           operand   -    Second operand for operation to be performed on.
%                          one should append *, ^ and / with a . i,e, .*,
%                          .^, ./ 
%           result    -    The result to be compared with.

warning off all

switch type
    case 'binary'
        if isa(varargin{2}, 'IXTdataset_1d') 
            
            if isa(varargin{3},'IXTdataset_1d') || isa(varargin{3},'IXTdataset_2d')
                diff = (abs(varargin{4}.signal - eval(['varargin{2}.signal ' varargin{1} ' varargin{3}.signal'])));
                result = ((abs(0.0002.*varargin{4}.signal) + 0.000002) > diff) | isnan(diff) | isinf(diff);
                out = all(result(:));
            
            elseif isnumeric(varargin{3})
                diff = (abs(varargin{4}.signal - eval(['varargin{2}.signal ' varargin{1} ' varargin{3}'])));
                result = ((abs(0.0002.*varargin{4}.signal) + 0.000002) > diff) | isnan(diff) | isinf(diff);
                out = all(result(:));
            end

        elseif isa(varargin{2},'IXTdataset_2d')
            if isa(varargin{3},'IXTdataset_1d')
                if length(varargin{3}) == 1
                    result = false(1,size(varargin{2}.signal,2));
                    for i = 1:size(varargin{2}.signal,2)
                        diff = abs(varargin{4}.signal(:,i) - eval(['varargin{2}.signal(:,i) ' varargin{1} ' varargin{3}.signal''']));
                        result(i) = all(((abs(0.0002.*varargin{4}.signal(:,i)) + 0.000002) > diff) | isnan(diff) | isinf(diff));
                    end
                    out = all(result(:));
                elseif length(varargin{3}) == length(varargin{2}.signal(1,:))
                    result = false(1,size(varargin{2}.signa(1,:)));
                    for i = 1:length(varargin{2}.y)
                        diff = abs(varargin{4}.signal(:,i) - eval(['varargin{2}.signal(:,i) ' varargin{1} ' varargin{3}(i).signal''']));
                        result(i) =  all(((abs(0.0002.*varargin{4}.signal(:,i)) + 0.000002) > diff) | isnan(diff) | isinf(diff));
                    end
                    out = all(result(:));
                end
            elseif isa(varargin{3}, 'IXTdataset_2d')
                    diff = abs(varargin{4}.signal - eval(['varargin{2}.signal' varargin{1} ' varargin{3}.signal']));
                    result = all((abs(0.0002*varargin{4}.signal) + 0.000002) > diff | isnan(diff) | isinf(diff));
                    out = all(result(:));
            elseif isnumeric(varargin{3})
                if size(varargin{3}) == [1, size(varargin{2}.signal,2)]
                    result = false(1,size(varargin{2}.signal,2));
                    for i = 1:size(varargin{2}.signal,2)
                        diff = abs(varargin{4}.signal(:,i) - eval(['varargin{2}.signal(:,i) ' varargin{1} ' varargin{3}']));
                        result(i) = all(((abs(0.0002.*varargin{4}.signal(:,i)) + 0.000002) > diff) | isnan(diff) | isinf(diff));
                    end
                    out = all(result(:));
                elseif size(varargin{3}) == size(varargin{2}.signal)
                   
                    diff = abs(varargin{4}.signal(:) - eval(['varargin{2}.signal(:) ' varargin{1} ' varargin{3}(:)']));
                    result = ((abs(0.0002.*varargin{4}.signal(:)) + 0.000002) > diff) | isnan(diff) | isinf(diff);
                    out = all(result(:));
                elseif size(varargin{3}) == 1
                    diff = abs(varargin{4}.signal(:) - eval(['varargin{2}.signal(:) ' varargin{1} ' varargin{3}'])); 
                    result = ((abs(0.0002.*varargin{4}.signal(:)) + 0.000002) > diff) | isnan(diff) | isinf(diff);
                    out = all(result(:));
                end
            end
        end    
    case 'trig'
        
        out = true;
        
        for i = 1:length(varargin{3})

            diff = abs(varargin{3}(i).signal - eval([varargin{1}, '(varargin{2}(i).signal)']));
            result = ((abs(0.0002.*varargin{3}(i).signal) + 0.000002) > diff) | isnan(diff) | isinf(diff);
            out = all(result(:)) & out;

        end
        
    otherwise
        warning('unrecognised type sent to libisis_checker')
        out = false;
end

warning on all 