
function ixf_display_error(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ixf_display_warning(error_no)
%Purpose: to display error
%Input: error no 
%Output: none
%Example: ixf_display_error(IXG_ST_WARNING.no_figure)
%the above examples displays an error message of no figure
%NOTE: the figure if created by graphics will be deleted
%if any error occurs
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');


%do not show the figure if any error occurs
%check for figure
ind = get(0,'currentfigure');
if (~isempty(ind))
    %pass gcf
    idvalue = ixf_getfigureidentifier('hdl',gcf);
    %check my figure
    if ( strcmpi(idvalue,IXG_ST_STDVALUES.identifier) && (isempty(gca)) )
        %if my figure and anything is not drawn then delete it
        delete(gcf);
    end    
end

error_no = varargin{1};

%go for error parsing (integer comparision)
if (error_no == IXG_ST_ERROR.wrong_arg)
    error(['Error:-->Wrong no of arguments, Please see help ']);
elseif (error_no == IXG_ST_ERROR.wrong_value)
    error(['Error:-->Wrong values for field ' varargin{2}]);
elseif (error_no == IXG_ST_ERROR.wrong_function)
    error(['Error:-->Wrong function name passed']);
elseif (error_no == IXG_ST_ERROR.wrong_interface)
    error(['Error:-->Wrong interface or Interface does not exist']);
elseif (error_no == IXG_ST_ERROR.no_figure)
    error(['Error:-->There is No Graphics figure']);
elseif (error_no == IXG_ST_ERROR.invalid_number)
    error(['Error:-->Invalid number for field ' varargin{2}]);
elseif (error_no == IXG_ST_ERROR.invalid_character)
    error(['Error:-->Invalid string for field ' varargin{2}]);    
elseif (error_no == IXG_ST_ERROR.greater_value)
    error(['Error:-->' varargin{2} ' Value must be greater than ' varargin{3}]);    
elseif (error_no == IXG_ST_ERROR.cannot_plot)
    error(['Error:-->Cannot get plot handle']);    
elseif (error_no == IXG_ST_ERROR.cannot_axes)
    error(['Error:-->Cannot get axes handle']);    
elseif (error_no == IXG_ST_ERROR.cannot_figure)
    error(['Error:-->Cannot get figure handle']);  
elseif (error_no == IXG_ST_ERROR.no_figfound_at)
    error(['Error:-->No figure found with specified application name and tag']);
elseif (error_no == IXG_ST_ERROR.no_figfound_ht)    
    error(['Error:-->No figure found with specified handle and tag']);
elseif (error_no == IXG_ST_ERROR.no_figfound_h)    
    error(['Error:-->No figure found with specified handle ']);    
elseif (error_no == IXG_ST_ERROR.no_object)    
    error(['Error:-->Object is required ']);  
elseif (error_no == IXG_ST_ERROR.wrong_field)    
    error(['Error:-->Wrong field name passed'' ', varargin{2}, ' '' Please see help ']);  
elseif (error_no == IXG_ST_ERROR.already_exist)    
    error(['Error:-->' varargin{2} ' already exist ']);  
elseif (error_no == IXG_ST_ERROR.no_format)    
    error(['Error:-->No format specified for plot ']);  
elseif (error_no == IXG_ST_ERROR.single_objreq)    
    error(['Error:-->Multiple object values are not allowed ']);          
elseif (error_no == IXG_ST_ERROR.no_figfound_hta)    
    error(['Error:-->No figure found with specified handle or name and tag ']);      
elseif (error_no == IXG_ST_ERROR.plot_legend_wrong)    
    error(['Error:-->No of plot are less compare to Strings passed ']);         
elseif (error_no == IXG_ST_ERROR.not_valid_color)    
    error(['Error:-->Invalid color  ']);         
elseif (error_no == IXG_ST_ERROR.not_valid_markerstyle)    
    error(['Error:-->Invalid marker style ']);         
elseif (error_no == IXG_ST_ERROR.not_valid_linestyle)    
    error(['Error:-->Invalid line style ']);   
elseif (error_no == IXG_ST_ERROR.no_size)    
    error(['Error:-->Object has no size ']);   
end