function wout = hist2point(w)
% Converts histogram data to point data in IXTdataset_1d
%
%   >> wout = hist2point(win)

wout = w;
for i=1:length(w)
    if length(w(i).x)~=length(w(i).signal) % hisogram data
        wout(i).x = 0.5*(w(i).x(2:end)+w(i).x(1:end-1));
    end
end
