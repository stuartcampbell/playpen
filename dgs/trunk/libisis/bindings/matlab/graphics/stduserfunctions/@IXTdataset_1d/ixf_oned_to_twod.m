function w2=ixf_oned_to_twod(w)
%----------------Help for GTK oned_to_twod---------------------------------
% Call syntax: wout=oned_to_twod(win)
% purpose: convert dataset_1d array to dataset_2d array each with a y value
% coresponding to the index of the dataset in the array.
%
% Inputs: dataset1d array
% Outputs: dataset2d array
%
% ----------------updated: 30/08/2006, Dean Whittaker----------------------

IXG_ST_ERROR=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR');

will_contract = true;       % set a flag which specifies if the array will contract

if length(w) <1
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
end


% y_units in this case must specifically be spectrum number
w2=IXTdataset_2d;
initial_size = size(w(1).signal);  % find the size of the first dataset

for i=1:length(w)
    
    w2(i)=IXTdataset_2d(w(i).base, w(i).title, w(i).signal', w(i).error', w(i).s_axis,...
        w(i).x, w(i).x_axis, w(i).x_distribution,...
        i,IXTaxis('$spno'), false);
    
    will_contract = will_contract && all(initial_size == size(w(i).signal)); 
                        % To contract all datasets must be the same size
                        % This means that all datasets must be same as
                        % initial_size.
    
end

% try to combine into a single d2d rather than separate ones
if will_contract
    w2 = contract_d2d(w2);
end

