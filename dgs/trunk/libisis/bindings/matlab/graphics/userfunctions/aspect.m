function aspect(varargin)
%--- help for libisis gtk aspect set aspect ratio user function-----
%
% >> aspect(axesHandle, x_ulen, y_ulen, z_ulen)
% >> aspect(axesHandle, x_ulen, y_ulen)
% >> aspect(axesHandle, mode)
% >> aspect(axesHandle)
%
%
% or one can use name tags to identify which graphs to change the aspect
% ratio of
%
% >> aspect(name, tag, xulen, yulen, zulen)
%
% where xulen, yulen and zulen are optional 
%
% axesHandle / name tag can be omitted, in this case, the current axes will be used.
%
% inputs: 
%
%       axesHandle: handle of the axes which is to be scaled. Can be a
%                   vector array of axes handles - in which case the
%                   operation is performed on each handle in turn.
%       x_ulen:     defines the unit length along the x axis
%       y_ulen:     defines the unit length along the y axis
%       z_ulen:     defines the unit length along the z axis]
%       mode:       either 'auto' or 'manual'
%
% This sets the scale of axes so that one unit along the x axis
% is the same as one unit along the y axis and z axis.
%
% If just given values for x_ulen and y_ulen, the z_ulen retains the same
% proportion to x and y as it did before the aspect was set
%
% With just the axes handle input, the function is the same as
%
% >> aspect(axesHandle, 1, 1)
%
% when the mode is set to 'auto', matlab chooses the aspect ratio which
% fills the figure the best.
%
%
%--------------------------------------------------------------------------
% Dean Whittaker 2007

ui_set_aspect(varargin{:});