function w2 = oned_to_twod(w)
%----------------Help for GTK oned_to_twod---------------------------------
% Call syntax: wout=oned_to_twod(win)
% purpose: convert dataset_1d array to dataset_2d array each with a y value
% coresponding to the index of the dataset in the array.
%
% Inputs: dataset1d array
% Outputs: dataset2d array
%
% ----------------updated: 30/08/2006, Dean Whittaker----------------------

if length(w) <1
    error('incorrect number of arguments')
end


% y_axis in this case must specifically be spectrum number
w2=IXTdataset_2d;
for i=1:numel(w)
    
    w2(i)=IXTdataset_2d(w(i).base, w(i).title, w(i).signal', w(i).error', w(i).s_axis,...
        w(i).x, w(i).x_axis, w(i).x_distribution,...
        i,IXTaxis('$spno'), false);
end

% try to combine into a single d2d rather than separate ones
try 
    w2 = contract_d2d(w2);
end

