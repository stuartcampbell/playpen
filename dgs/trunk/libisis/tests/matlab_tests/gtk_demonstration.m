%% initialise
addpath ../../bindings/matlab/classes/
addpath ../../applications/matlab/homer/
addpath ../../applications/matlab/mgeniefuncs/
addpath ../../bindings/matlab/graphics/gtk_application
addpath ../../bindings/matlab/utilities
gtk_init
rawfile1 = IXTisis_raw_file('../HET15958.RAW');
rawfile2 = IXTisis_raw_file('../HET16538.RAW');
rawfile3 = IXTisis_raw_file('../MAP06220.RAW');
nsp = geti(rawfile1,'NSP1');
w = getspectrum(rawfile1,25);
w2 = getspectrum(rawfile2,233);
wd = getspectra(rawfile1,[1:nsp]);
wd = rebin_x(wd,[0,200,20000]);
wd2 = getspectra(rawfile2,[1:nsp]);
wd3=getspectra(rawfile1,[1:300]);
wd4=getspectra(rawfile3,[500:1000]);
wd3 = rebin_x(wd3,[3000,300,13000]);
wd4 = rebin_x(wd4,[0,300,10000]);
wd3.title='not same'
wdd = [wd3, wd4];
array_builder;
%%




% The user can do a multiplot of several IXTdataset_1d and change the x
% limits
mp(ww);
lx(100,1000)



% Now we see a line plot of w appears in a separate window
dl(w);
logx
lx(1000,10000);
ly



% The user can choose to have figure, axes and plot handles as output
[surface_figure, surf_axes, surf_plot] = ds(wd);



% Now we plot a histogram, this will replace the previous line plot,
% because it is of the same graph type 
dh(w2);
lx(400,1000);
ly



% An array of colours can be set, see what happens as we plot
acolor('red','green','blue');
pe(w2);



% We can also change the linestyle
aline('--');



% And keep the figure
pl(w);
lx(600,1000)
ly
kf


% This figure will take a window called "myplot", it has extra settings
[figHandle, axesHandle, plotHandle] = dm(w2);
dm(w2,'name','myplot','tag','example','marker', ...
    '+','markersize',6,'acolor','red','afontsize',15);



% Now we return to the surface plot using the figure handle
figure(surface_figure)
lx(1000,10000);
lz
colorbar off



% see how this replaces the dm plot, because we've given it the same name
% and tag.
da(wd,'name','myplot','tag','example');
lx(5000,10000)
ly(500,2500)
lz



% lets see a surface plot of a D1D array and a D2D
ds(ww);
ds(wdd);



% See how the area plot does not replace the last one, since it has a
% different name and tag
da(wdd);
lz(0,0.2);



% Let's rebin some data and plot it
demo=getspectra(rawfile1,[1001:2000]);
demo=rebin_x(demo,[5000,100,5500,20,6400,100,10000]);
ds(demo, 'title', 'demonstration');




% I know the figure_handle of my plot, but not the plot or axes handle..
% How do I change the plot colour? 

dl(w,'name','demo')

%We've just made a plot, how do we set attributes without knowing all the
%information about our plot

set_plot('demo','color','red')

% this is the same as

set_plot(7,'color','green')

% which is the same as

set_plot(gca,'color','blue')

% we can set the figure properties from the axes handle

set_figure(gca,'color','red')















% slightly more usable
% set(::myplot(double)/figno(integer)/name,tag:: 'attribute',value)

