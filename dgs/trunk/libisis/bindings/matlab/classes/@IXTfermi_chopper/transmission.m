function trans = transmission(fermi_chopper, varargin)
% Calculate transmission of a fermi chopper. The energy at which the transmission
% is calculated can be either that in the fermi_chopper object, or a scalar or
% vector of energy values:
%
% Syntax
%   >> w = transmission (chop)        % transmission at energy of chopper
%   >> w = transmission (chop, ei)    % transmission at energy ei, where
%                                       ei is a 1xn or nx1 vector (including n=1)
%
switch length(varargin)
    case 0
        trans = libisisexc('IXTfermi_chopper','transmission',fermi_chopper);
    case 1
        ei = varargin{1};
        trans = libisisexc('IXTfermi_chopper','transmission',fermi_chopper,ei);
    otherwise
        error ('Check argumnents to fermi chopper transmission')
end
