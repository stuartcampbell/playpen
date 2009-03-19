function no_errors = polygon_check(r)
% Function checks that all fields in an IXTpolygons object are consistent. 
%
% >> no_errors = polygon_check(r)
%
% inputs:
%   r               IXTpolygons object
%
% outputs:
%   no_errrors:     Number of errors encountered 
%
% note that a result of 0 means that the IXTpolygons object has been
% construted correctly and not that it has failed. 

verts = size(r.vertices);
faces = size(r.faces);
sigs = size(r.signal);
errs = size(r.error);

no_errors = 0;

if faces(1) ~= sigs(1)
    warning('Face and signal data dimensions do not match')
    no_errors = no_errors + 1;
end

if verts(2) ~= 2
    warning('Vertices dimensions are incorrect. Should contain x, y data only')
        no_errors = no_errors + 1;
end

if errs(1) ~= sigs(1) || errs(2) ~= sigs(2)
    warning('Error and signal data incompatable')
        no_errors = no_errors + 1;
end

if ~isa(r.s_axis, 'IXTaxis')
    warning('s_axis must be an IXTaxis object')
        no_errors = no_errors + 1;
end

if ~isa(r.x_axis, 'IXTaxis')
    warning('x_axis must be an IXTaxis object')
        no_errors = no_errors + 1;
end

if ~isa(r.y_axis, 'IXTaxis')
    warning('y_axis must be an IXTaxis object')
        no_errors = no_errors + 1;
end

if ~ islogical(r.x_distribution)
        warning('x_distribution must be logical')
            no_errors = no_errors + 1;
end

if ~ islogical(r.y_distribution)
        warning('y_distribution must be logical')
            no_errors = no_errors + 1;
end

if ~ (ischar(r.title) || cellstr(r.title))
    warning('title must be a string or cell array of strings')
        no_errors = no_errors + 1;
end