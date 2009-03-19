function sb(bin_value)
%-----help for gtk set binning, sb command --------------------------------
%Function Syntax: ret = SB(bin_value)
%Purpose: set binning value
%
%Output: success or error
%
%Input: none
%
%Examples: 
%ret = SB(2)
%the above example sets the binning value to 2
%-------------------------------------------------------------------------
ret = ui_setbinning(bin_value);

