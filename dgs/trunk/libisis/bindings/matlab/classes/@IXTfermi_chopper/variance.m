function var = variance(fermi_chopper, varargin)
% Calculate variance of the pulse width of a fermi chopper in sec^2. The energy
% at which the variance is calculated can be either that in the fermi_chopper
% object, or a scalar or vector of energy values:
%
% Syntax
%   >> w = variance (chop)        % transmission at energy of chopper
%   >> w = variance (chop, ei)    % transmission at energy ei, where
%                                   ei is a 1xn or nx1 vector (including n=1)
%
switch length(varargin)
    case 0
        var = libisisexc('IXTfermi_chopper','variance',fermi_chopper);
    case 1
        ei = varargin{1};
        var = libisisexc('IXTfermi_chopper','variance',fermi_chopper,ei);
    otherwise
        error ('Check argumnents to fermi chopper variance')
end