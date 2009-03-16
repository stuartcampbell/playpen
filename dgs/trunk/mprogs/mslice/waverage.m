function [xav,sigav]=waverage(x,sig)

% function [xav,sigav]=waverage(x,sig)

index=(sig>eps);	% points with non-zero errorbars
weights=0*x;
weights(index)=1./sig.^2;
weight=sum(weights(index));
xav=dot(x(index),weights(index))/weight;
sigav=1/sqrt(weight);
disp(sprintf('Weighted average %7.4g ± %7.4g',xav,sigav));
disp(sprintf('Simple average %7.4g',mean(x)));
