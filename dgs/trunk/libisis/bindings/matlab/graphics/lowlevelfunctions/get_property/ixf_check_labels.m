function [res] = ixf_check_labels(w)
% result = ixf_check_labels(win)
%
% checks the labels of array data in IXTdataset_1d or IXTdataset_2d by
% comparing them. Will return true if the labels are consistent and false
% if they are not
%
% inputs: IXTdataset_1d or IXTdatset_2d
% output: logical true or false

res = true;

for i = 1:(length(w)-1) 
    [xlab, zlab] = make_label(w(i));
    [xlab2, zlab2] = make_label(w(i+1));
    
    if ~strcmp(char(xlab),char(xlab2))
        res=false;        %IXG flags. 1 = the label is created automatically 2 = label given by user
    end
    if ~strcmp(char(zlab), char(zlab2))
        res = false;
    end
    if w(i).x_distribution~=w(i+1).x_distribution
        res = false;
    end
    % try technique on ylabel if exists
    try
        if ~strcmp(char(w(i).y_axis.units),char(w(i+1).y_axis.units))
        res = false;
        end
    end
end

    