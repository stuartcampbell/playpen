function display(win)
% Display an IXTpolygons object

% set flags for long arrays
title_flag = false;
sig_flag = false;
vert_flag = false;
faces_flag = [false, false];

title_size = size(deblank(win.title));
vertices_size = size(win.vertices);
faces_size = size(win.faces);
sig_size = size(win.signal);
err_size = size(win.error);

% set max sizes
if title_size > 20
    title_flag = true;
end

if prod(sig_size) > 6 
    sig_flag = true;
end

if faces_size(1) > 4
    faces_flag(1) = true;
end
if faces_size(2) > 5
    faces_flag(2) = true;
end
if vertices_size(1) > 4 
    vert_flag = true;
end


% set the distribution strings

if win.x_distribution
    x_dist = 'TRUE';
else 
    x_dist = 'FALSE';
end

if win.y_distribution
    y_dist = 'TRUE';
else 
    y_dist = 'FALSE';
end



disp('IXTpolygons')
disp([' - title  [' num2str(title_size(1)) '] ']);


disp([' - signal  [' num2str(sig_size(1)) '] ']);
if sig_flag
    disp(['   ' num2str(win.signal(1:6)')]);
else
    disp(['   ' num2str(win.signal')]);
end

disp([' - error  [' num2str(err_size(1)) '] ']);
if sig_flag
    disp(['   ' num2str(win.error(1:6)')]);
else
    disp(['   ' num2str(win.error')]);
end

disp([' - s_axis =  ' class(win.s_axis) ' ']);

disp([' - vertices  [' num2str(vertices_size(1)) ',' num2str(vertices_size(2)) '] ']);

disp([' - faces  [' num2str(faces_size(1)) ',' num2str(faces_size(2))  '] ']);

disp([' - x_axis =  ' class(win.x_axis) ' ']);
disp([' - x_distribution =  ' x_dist ' ']);
disp([' - y_axis =  ' class(win.y_axis) ' ']);
disp([' - y_distribution =  ' y_dist ' ']);
