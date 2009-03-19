function wout = hist2point_x(w)
% Converts histogram IXTdataset_2d to point data along the x dimension
wout = w;
for i=1:length(w)
    if length(w(i).x)~=size(w(i).signal,1) % histogram data
        wout(i).x = 0.5*(w(i).x(2:end)+w(i).x(1:end-1));
    end
end
