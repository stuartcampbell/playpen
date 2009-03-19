function res = ixf_set_binning(bin_value)
%------------------------------------------------
%function syntax: res = ixf_set_binning
%purpose: to set binning
%input: none
%output: true or false
%example: res = ixf_set_binning(2)
%the above example sets the binning value to 2
%------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

small = 1.0e-10;
std = 0.5;

if (isnumeric(bin_value))
    
    if (isa(bin_value,'double')) %double
        n_temp = bin_value;
    elseif (isa(bin_value,'int32'))  %int
        n_temp = double(bin_value);
    else
         res = IXG_ST_STDVALUES.false;
         ixf_display_error(IXG_ST_ERROR.invalid_number);
    end
    
    min1 = abs(n_temp-round(n_temp));
    
    if ( min1 < small && n_temp > std)
        IXG_ST_STDVALUES.binning = n_temp;
        res = IXG_ST_STDVALUES.true;
    else
        res = IXG_ST_STDVALUES.false;
        ixf_display_error(IXG_ST_ERROR.greater_value,'bin','equal to 1');
    end

    
    
else
    
    res = IXG_ST_STDVALUES.false;
    ixf_display_error(IXG_ST_ERROR.invalid_number,'bin');
    
end

i