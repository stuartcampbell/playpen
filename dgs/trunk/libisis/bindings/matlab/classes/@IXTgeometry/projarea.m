function [px,py] = projarea(varargin)
% Returns the coordinates of a polygon that draws the projection of a geometry
% object in one of several coordinate frames.
%
%   >> [px,py] = projarea( geometry, view_orientation, view_translation, projection_type [,radius] [,axes])

[px,py] = libisisexc('IXTgeometry', 'projarea_vertices_varargin', varargin);
