function polygons = convert_to_polygons(dataset_2d)
% Converts an IXTdataset_2d object to an IXTpolygons object by using the
% data to create a series of squares.
%
% >> polygon_object = convert_to_polygons(dataset_object)
%
% inputs: 
%       IXTdataset_2d object
%
% outputs:
%       IXTpolygons object
%

% Dean Whittaker 2007


for j = 1:numel(dataset_2d) % use j because i will be used later
    
    signal = dataset_2d(j).signal;
    error = dataset_2d(j).error;
    xdata = dataset_2d(j).x;
    ydata = dataset_2d(j).y;
    

    
    if size(signal,1) == numel(xdata)
        xdata = points_to_boundaries(xdata);
    end
    if size(signal,2) == numel(ydata)
        ydata = points_to_boundaries(ydata);
    end
    
    len_x = length(xdata);
    len_y = length(ydata);
    
    [ydata, xdata] = meshgrid(ydata, xdata);
    
    vertices = [xdata(:), ydata(:)];
    
%    faces = ones((numel(ydata) - len_y), 4);
    
    
   faces = ones(length(signal(:)), 4);

   
   
    for k = 0:(len_y - 2)   % generate faces matrix (see easier to read code commented below)
        i = 1 + k*len_x;
        start_point = i - k;
        end_point = (k+1)*len_x -1 ;
        faces((start_point: end_point - k), :) = [(i:end_point)', ((i:end_point)' + 1),(len_x + 1 + (i:end_point)'), (len_x + (i:end_point)')];
    end

%   ABOVE CODE DOES THE SAME AS BELOW CODE BUT A LOT FASTER!
%     for i = 1:(numel(xdata) - len_x)
%         if i ~= k*len_x
%             ind = i+1-k;           
%             faces(ind,:) = [i, (i+1), (len_x + (i+1)), (len_x + i)];
%         else
%             k = k+1;
%         end
%     end
    
polygons = IXTpolygons(dataset_2d.title, signal(:), error(:), dataset_2d.s_axis, ...
    vertices, faces, dataset_2d.x_axis, dataset_2d.x_distribution, ...
    dataset_2d.y_axis, dataset_2d.y_distribution);
    
end