function [lengthx, lengthy] = ixf_get_lengths(w, varargin)
%------------help for gtk ixf_get_lengths----------------------------------
%
% purpose: get the lengths for x and y for array of dataset_2d
%
% call syntax: [lengthx lengthy] = ixf_get_lengths(w,optional)
%
% inputs: w = dataset_2d, optional = list of options
%
% outputs: lengthx = length of x array to be used, lengthy = length of y
% array to be used
%
%---------------------updated 18/09/2006, Dean Whittaker-------------------

IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

totArg = length(varargin);
% set defaults
noxvalues=[];
noyvalues=[];
noxvalues_flag = false;
noyvalues_flag = false;

% look for passed values
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch varargin{iArgLoop}
        case 'noxvalues'
            noxvalues = varargin{iArgLoop+1};
            if (~isnumeric(noxvalues))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'noxvalues')
            end
        case 'noyvalues'
            noyvalues = varargin{iArgLoop+1};
            if (~isnumeric(noyvalues))
                 ixf_display_error(IXG_ST_ERROR.invalid_number,'noyvalues')
            end
    end
end

% set flags and values
if ~isempty(noxvalues)
    noxvalues_flag = true;
else
    noxvalues = 0;
end

if ~isempty(noyvalues)
    noyvalues_flag = true;
else
    noyvalues = 0;
end


% get automatic values if not given 

for i = 1:length(w)
    if ~noxvalues_flag
        temp1 = length(w(i).x);
        noxvalues = noxvalues + temp1;
    end
    
     
    if ~ noyvalues_flag
        temp1 = length(w(i).y);
        noyvalues = noyvalues + temp1;
    end
        
end

% check they're under the limit, if not, set to limit

if noxvalues > IXG_ST_STDVALUES.x_value_limit
    lengthx = IXG_ST_STDVALUES.x_value_limit;
else
    lengthx = noxvalues;
end

if noyvalues > IXG_ST_STDVALUES.y_value_limit
    lengthy = IXG_ST_STDVALUES.y_value_limit;
else
    lengthy = noyvalues;
end



    
   