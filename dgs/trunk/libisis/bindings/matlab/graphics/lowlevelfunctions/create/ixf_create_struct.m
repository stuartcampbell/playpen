

function ixf_create_struct(varargin)

[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

totArg = numel(varargin);
for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
    switch(lower(varargin{iArgLoop}))
        case 'appname'
            appname = varargin{iArgLoop + 1};
        case 'tag'
            tag = varargin{iArgLoop + 1};            
        case 'var'
            var = varargin{iArgLoop + 1};
        case 'struct'
            tmp = varargin{iArgLoop + 1};
        otherwise
            ixf_display_error(IXG_ST_ERROR.wrong_arg);
    end
end
%fill temporary structure
tmp.figure.name = appname;
tmp.figure.ftag = tag;
tmp.tag = tag;
tmp.name = appname;
%create the variable
assignin('base',var,tmp);