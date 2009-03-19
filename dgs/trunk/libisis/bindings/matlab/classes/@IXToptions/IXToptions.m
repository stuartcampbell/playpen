function options = IXToptions( varargin )
% ! Create an IXToptions object
% ! REQUIRED INPUT PARAMETERS
% ! options  = IXToptions(base, bgrd(logical), m_axis(logical), m_rebin(logical),d_axis(logical),d_rebin(logical),ei(logical)); 
%
% !! @author Dickon Champion, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !---------------------------------------------------------------------------------------------------------------------------
% ! IXToptions
% !
% !	base			IXTbase				
%    options.bgrd=logical(0);		detector background subtraction
%    options.m_axis=logical(0);     monitor units change
%    options.m_rebin=logical(0);	monitor rebinning
%    options.d_int=logical(0);		detector integration
%    options.d_axis=logical(0);     detector units change
%    options.d_rebin=logical(0);	detector rebinning
%    options.ei=logical(0);		incident energy 
% ! Notes:
% ! ------
% a set of logical flags which determine specific options for population of IXTrunfile in a homer operation

    % if no input arguments, create a default object
    options.base = IXTbase;
    options.bgrd=false;
    options.m_axis=false;
    options.m_rebin=false;
    options.d_int = false;
    options.d_axis=false;
    options.d_rebin=false;
    options.ei=false;
    options = class(options,'IXToptions');
if (nargin > 0)
    options = libisisexc('IXToptions','create',options,varargin);
end
