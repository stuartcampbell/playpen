function dataset = fromplot(varargin)
% - help for libisis graphics from plot user function
%
% syntax: dataset = fromplot(Handle, 'property','value',...)
%         dataset = fromplot('property','value',...)
%
% purpose: creates an IXTdataset_2d with as much information as possible 
% from the plot contained in it. This can be used to recreate the plot, 
% however does not contain information such as units codes etc.
%
% Works on plots that are not part of libisis graphics also.
%
% syntax: dataset = fromplot(inHandle_, property, value, ...)
%
% inputs: 
%       inHandle_:      Any figure, axes or plot handle. The related plots
%                       will be used
%       property:       Property such as "x_units", "x_distribution etc. to
%                       over-ride the automatically filled fields
%       value:          Value corresponding to the property, such as
%                       IXTunits(IXTbase('unknown', false, true),'null','mylabel') 
%                       or true.
%
% outputs:
%       dataset:    IXTdataset_2d with fields:
%                           x       - ALL x data used to make the plot (even
%                                     if not within the current limits)
%                           y       - ALL y data used to make the plot (even
%                                     if not within the current limits)
%                           signal  - ALL z data used to make the plot (even
%                                     if not within the current limits)
%                           error   - ALL error data will be 0 
%                           x_axis - caption will be xlabel, 
%                                     code will be '$' 
%                                     units will be ' '
%                           y_axis - caption will be ylabel, 
%                                     code will be '$' 
%                                     units will be ' '
%                           s_axis - caption will be slabel, 
%                                     code will be '$' 
%                                     units will be ' '
%                           title -   will 
%                           x_distribution - FALSE
%                           y_distribution - FALSE
%
%               on a 1 dimensional plot, signal data and units will
%               correspond to the y axis data and units, ydata will be "1" and 
%               units will be NULL.
%
%               on an errorbar plot, the errorbars themselves are stored as
%               x and y data.
%                           
% example:
%       >> dataset_2d = fromplot(gcf, 'y_axis',
%       IXTaxis('$spno'), 
%       'x_distribution',true)
%
%   will create a dataset from the data in the current figure, uses spno as
%   the units code for the y units and the units to 'spectrum number' 
%   and sets x distribution to true
% Dean Whittaker 2007
dataset = ui_get_dataset(varargin{:});