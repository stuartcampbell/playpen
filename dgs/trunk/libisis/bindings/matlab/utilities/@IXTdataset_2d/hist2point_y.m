function wout = hist2point_y(w)
% Converts histogram IXTdataset_2d to point data along the y dimension
wout = w;
for i=1:length(w)
    if length(w(i).y)~=size(w(i).signal,2) % histogram data
        wout(i).y = 0.5*(w(i).y(2:end)+w(i).y(1:end-1));
    end
end
