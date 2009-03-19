
function value = ixf_get_arrayvalue(values,count_number)
%--------------help for GTK ixf_get_value----------------------------------
%
% syntax: ixf_get_arrayvalue(values, count_number)
%
% purpose: chooses a single value from a cell array by cycling around the values count_number
% times
%
% inputs: 
%
%       values = cell array of values, 
%       count_number = number of times to cycle around the values
%
% output: 
%
%       value = the value of the array which matches the count_number
%--------------------------------------------------------------------------
if count_number <1
    count_number = 1;
end

while count_number > length(values)
    count_number = count_number - length(values);
end

value = values{count_number};