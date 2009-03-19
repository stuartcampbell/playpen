function axis = IXTaxis(varargin)
% Create IXTaxis object
%   >> w = IXTaxis (caption)
%   >> w = IXTaxis (caption, units)
%   >> w = IXTaxis (caption, units, code)
%
%  or with IXTbase object:
%   >> w = IXTaxis (base, caption, units, code)
%
%  Creates an IXTdataset_1d object with the following elements:
%
%   base        IXTbase 
% 	caption		char        Caption for axis
%                          (Caption can be multiline input in the form of a
%                           cell array or a character array)
%   units       char        Units for axis
%   code        char        Code for units (see documnetation for built-in units;
%                          can also be unser-defined unit code)

axis.base = IXTbase;
axis.caption = [ 'line1'; 'line2' ];
axis.units = ' ';
axis.code = ' ';
axis = class(axis,'IXTaxis');

% Now call the fortran constructor which will fill the object and also check its arguments

if (nargin == 1) 
    if ischar(varargin{1})
        if (varargin{1}(1)=='$')   
            axis = libisisexc('IXTaxis','create_code_varargin',axis,varargin);    
        else
            axis = libisisexc('IXTaxis','create_caption_varargin',axis,varargin);    
        end
    end
    if iscellstr(varargin{1})
        caption=char(varargin{1});        
        axis = libisisexc('IXTaxis','create_caption',axis,caption);
    end
    
elseif (nargin == 2)
    if iscellstr(varargin{1})
        caption=char(varargin{1});
    else
        caption=varargin{1};
    end 
    axis = libisisexc('IXTaxis','create_caption_units',axis,caption,varargin{2});
    
elseif (nargin == 3)
    if iscellstr(varargin{1})
        caption=char(varargin{1});
    else
        caption=varargin{1};
    end 
    axis = libisisexc('IXTaxis','create_caption_units_code',axis,caption,varargin{2},varargin{3});
    
elseif (nargin > 0) 
    axis = libisisexc('IXTaxis','create',axis,varargin);
end
