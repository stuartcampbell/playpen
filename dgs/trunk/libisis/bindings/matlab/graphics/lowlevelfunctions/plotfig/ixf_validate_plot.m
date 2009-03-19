

function ixf_validate_plot(varargin)
%--------------------------------------------------------------------------
%Function syntax: ixf_validate_plot(nameoid,name,tagoid,tag)
%Purpose: to validate plot
%Input:  nameoid,name,tagoid,tag 
%Output: none
%Example: ixf_validate_plot('name','tobie','tag','1d')
%The above example checks current figure properties with passed values
%--------------------------------------------------------------------------

%global structures
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

tot = numel(varargin);
data = '';
name = IXG_ST_STDVALUES.appname;
format = '';
%check total args  
for iArg = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:tot
    %parse, args must be character
    if (ischar(varargin{iArg}))
        switch(lower(varargin{iArg}))
            case 'tag'
                data = varargin{iArg + 1};    
            case 'name'
                name = varargin{iArg + 1};    
        end
    end
end
%create figure or change old figure 
ixf_check_figureprop(name,data);

