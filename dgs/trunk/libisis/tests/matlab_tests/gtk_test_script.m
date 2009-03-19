%% GTK Test Script
%% Get Variables

progressbar(0, 4);

gtk_load_data_script

progressbar(0.05)

%% 1d Plotting Functions

if ~ extra_tag

    [fig, axes, plot] = dl(w);

    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dl(w,'color','red');

    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dl(ww);

    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 


    [fig, axes, plot] = dl(ww,100,200);

    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    progressbar(0.07)

    [fig, axes, plot] = dl(w,100,400);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 


    [fig, axes, plot] = dl(w,100,400,0,10);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    
    kf;

    [fig, axes, plot] = dl(w2);

    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    close(2);

    [fig, axes, plot] = dl(w,'color','red');

    progressbar(0.1)
    
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 


    [fig, axes, plot] = dl(w,'color','red','fcolor','blue','funits','point','name','testplot','tag','one','acolor','green','xgrid','on','ygrid','on',...
        'title','test_case_1','xlabel','testdata','ylabel','ydata','afontcolor','blue',...
        'afontname','arial','afontweight','bold','xlim',[100,400],'ylim',[0,10], ...
        'linestyle','--','marker','o','linewidth',5);

    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    if default_structure_tag
        dl(w,default_structure);
    end


    kf;

    [fig, axes, plot] = de(w);

    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    close(2);

    [fig, axes, plot] = de(w);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(w,'color','red');
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(ww);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(ww,100,200);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(w,100,400);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(w,100,400,0,10);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    kf;

    [fig, axes, plot] = de(w2);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end
    
    progressbar(0.13)
    
    close(2);

    [fig, axes, plot] = de(w,'color','red','fcolor','blue','funits','point','name','testplot','tag','one','acolor','green','xgrid','on','ygrid','on',...
        'title',{'test_case_1', 'with multiline'},'xlabel','testdata','ylabel','ydata','afontcolor','blue',...
        'afontname','arial','afontweight','bold','xlim',[100,400],'ylim',[0,10], ...
        'linestyle','--','marker','o','linewidth',5);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(w);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end

    if default_structure_tag
       de(w,default_structure);
    end

    kf;
 
    [fig, axes, plot] = dh(w);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 
    close(2);

    [fig, axes, plot] = dh(w);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dh(w,'color','red');
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dh(ww);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dh(ww,100,200);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dh(w,100,400);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end

    progressbar(0.17)
    
    [fig, axes, plot] = dh(w,100,400,0,10);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

    kf;

    [fig, axes, plot] = dh(w2);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    close(2);
 
    [fig, axes, plot] = dh(w,'color','red');
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dh(w,'color','red','fcolor','blue','funits','point','name','testplot','tag','one','acolor','green','xgrid','on','ygrid','on',...
        'title','test_case_1','xlabel','testdata','ylabel','ydata','afontcolor','blue',...
        'afontname','arial','afontweight','bold','xlim',[100,400],'ylim',[0,10], ...
        'linestyle','--','marker','o','linewidth',5);
    if fig~=6
        warning('test failed: graph appears in wrong figure')
    end 


    kf;

    [fig, axes, plot] = dm(w);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    close(2);

    [fig, axes, plot] = dm(w);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dm(w,'color','red');
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dm(ww);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dm(ww,100,200);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dm(w,100,400);
    
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dm(w,100,400,0,10);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    kf;

    [fig, axes, plot] = dm(w2);
    if fig~=7
        warning('test failed: graph appears in wrong figure')
    end 

    close(2);
    
    progressbar(0.21)
    
    [fig, axes, plot] = dm(w,'color','red');
    if fig~=7
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = dm(w,'color','red','fcolor','blue','funits','point','name','testplot','tag','one','acolor','green','xgrid','on','ygrid','on',...
        'title','test_case_1','xlabel','testdata','ylabel','ydata','afontcolor','blue',...
        'afontname','arial','afontweight','bold','xlim',[100,400],'ylim',[0,10], ...
        'linestyle','--','marker','o','linewidth',5);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    kf;

    [fig, axes, plot] = de(w);
    if fig~=7
        warning('test failed: graph appears in wrong figure')
    end 
    kf
    [fig, axes, plot] = dm(w);
    if fig~=8
        warning('test failed: graph appears in wrong figure')
    end 
    
    [fh axesH, plotH] = get_all_handles;
    close(fh(~(fh == 1)));
    
    display('1d plotting routines check complete')
    

%% 2d Plotting Functions
    
    [fig, axes, plot] = ds(wd);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = ds(wd,100,400);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = ds(wd,100,400,0,10);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    
    progressbar(0.24)
    
    [fig, axes, plot] = ds(wd,100,400,0,10,0,0.1);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    kf;

    [fig, axes, plot] = ds(wd2);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    close(2);

    [fig, axes, plot] = ds(wd);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = ds(wd,'fcolor','blue','tunits','point','name','testplot','tag','one','acolor','green','xgrid','on','ygrid','on',...
        'title','test_case_1','xlabel','testdata','ylabel','ydata','afontcolor','blue',...
        'afontname','arial','afontweight','bold','xlim',[100,400],'ylim',[0,10], ...
        'linestyle','--','marker','o','linewidth',5);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    kf;
    
    progressbar(0.26)

    [fig, axes, plot] = da(wd);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(2);
    [fig, axes, plot] = da(wd);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wdd);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wdd,100,200);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wd,100,400);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wd,100,400,0,10);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wd,100,400,0,10,0,0.1);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    

    
    kf;
    
    progressbar(0.31)

    [fig, axes, plot] = da(wd2);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    close(2);

    [fig, axes, plot] = da(wd);
   
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wd,'fcolor','blue','tunits','point','name','testplot','tag','one','acolor','green','xgrid','on','ygrid','on',...
        'title','test_case_1','xlabel','testdata','ylabel','ydata','afontcolor','blue',...
        'afontname','arial','afontweight','bold','xlim',[100,400],'ylim',[0,10], ...
        'linestyle','--','marker','o','linewidth',5);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 


    kf;

    close(2);

    kf;

    close(3);
    

    
    kf;

        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));
    
    progressbar(0.34)
    
    display('2d plotting routines check complete')
    
%% Window Keeping Tests
    
    [fig, axes, plot] = ds(wd); 
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    
    [fig, axes, plot] = dl(w);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 
    
    [fig, axes, plot] = mp(ww);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    progressbar(0.4)
    display('line 500 reached')
    
    [fig, axes, plot] = da(wd);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = de(w);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = mp(ww2);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(2);

    [fig, axes, plot] = dh(w);
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 
    close(3);
    
    progressbar(0.43)
    
    [fig, axes, plot] = dl(w);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = mp(ww);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 

    [fig, axes, plot] = da(wd);
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 

        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

    display('round 1 of keep figure check complete')
    
    %-------------round 2----------
    [fig, axes, plot] = mp(ww); 
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    [fig, axes, plot] = ds(wd); 
        
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 
    [fig, axes, plot] = dl(w);  
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(2)
    [fig, axes, plot] = de(w); 
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(3) 
    [fig, axes, plot] = da(wd);
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    close(2)
    close
    
    [fig, axes, plot] = dh(w);   
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    [fig, axes, plot] = da(wd); 
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 

        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

    progressbar(0.49)
    
    display('round 2 of keep figure check complete')
    
    %------------round 3 with keep figure-----
    [fig, axes, plot] = mp(ww); 
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    
    [fig, axes, plot] = ds(wd); 
    if fig~=3
        warning('test failed: graph appears in wrong figure')
    end 
    
    [fig, axes, plot] = da(wd); 
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    kf(2);
    
    [fig, axes, plot] = mp(ww2); 
       
    if fig~=5
        warning('test failed: graph appears in wrong figure')
    end 
    [fig, axes, plot] = dl(w);   
    if fig~=6
        warning('test failed: graph appears in wrong figure')
    end 
    kf
    close(4)
    
    [fig, axes, plot] = de(w); 
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    
    [fig, axes, plot] = dh(w);
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(2)    
    close(4)
    

    
    [fig, axes, plot] = dh(w);  
    if fig~=2
        warning('test failed: graph appears in wrong figure')
    end 
    kf
    
    progressbar(0.53)
    
    [fig, axes, plot] = dl(w); 
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(3);
    
    [fig, axes, plot] = dl(w);  
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    close(2)
    
    [fig, axes, plot] = dh(w);   
    if fig~=4
        warning('test failed: graph appears in wrong figure')
    end 
    
        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));
    
    display('round 3 of keep figure check complete')
    
%% Limit Setting Tests
   
    progressbar(0.56)

    [fig, axes, plot] = ds(wd);
    lx(100,500)
    lx
    lz
    ly(100,1000)
    ly
    lz(0,0.1)
    lz
    lx
    ly
    lx(1,1000)
    ly(500,1000)
    lx
    ly
    lx(500,1000)
    lz
    lc(0,0.1)
    lz(0,8000)
    lc
    lx

    lx(0,20000)
    ly(0,2500)
    lz(0,9000)
    lx
    ly
    lz
    lx(-5,30000)
    ly(-5,4000)
    lz(-5,20000)
    lx
    ly
    lz
    lc
    lx
    display('round 1 of limits check complete')
    
    progressbar(0.59)
    % ----------- round 2------------
    [fig, axes, plot] = da(wd);
    lx(100,500)
    lx
    lc
    ly(100,1000)
    ly
    lz(0,0.1)
    lc
    lx
    ly
    lx(1,1000)
    ly(500,1000)
    lx
    ly
    lx(500,1000)
    lc
    lc(0,0.1)
    lz(0,8000)
    lc
    lx

    lx(0,20000)
    ly(0,2500)
    lz(0,9000)
    lx
    ly
    lc
    lx(-5,30000)
    ly(-5,4000)
    lz(-5,20000)
    lx
    ly
    lc
    lc
    lx
    display('round 2 of limits check complete')
    
    %-----------------round 3----------------------

    ly
    lx(1,1000)
    ly(500,1000)
    lx
    ly
    lx(500,1000)
    lz
    lc(0,0.1)
    lz(0,8000)
    lc

    lx(0,20000)
    ly(0,2500)
    lz(0,9000)
    lx
    ly
    lz
    lx(-5,30000)
    ly(-5,4000)
    lz(-5,20000)
    lx
    ly
    lz
    lc
    display('round 3 of limits check complete')
   
    progressbar(0.6)
    
    % ---------------line data--------------
    [fig, axes, plot] = dl(w);
    lx(100,500)
    lx
    ly
    lx(1,1000)
    ly
    ly(0,40)
    ly
    ly(0,15)
    lx
    lx(1,2000)
    lx

    lx(-5,25000)
    ly(-5,100)
    lx
    
    [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

progressbar(0.62)
    display('limits on line data check complete')
    
%% Additional Options Tests

    acolor('red');
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    acolor('green');
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    figure(2);
    acolor('yellow');
    ph(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    
        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));
    
    progressbar(0.67)
    
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    figure(2)
    reset_default;
    [fig, axes, plot] = dl(w);
    reset_all;
    [fig, axes, plot] = dl(w);
        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

    display('acolor check complete')
    
    progressbar(0.69)
    
    %-------------amark--------------
    amark('+');
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    amark('*');
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    figure(2);
    amark('+',2.4);
    ph(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    close, close, close,
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    figure(2)
    
    reset_default;
    [fig, axes, plot] = dl(w);
    reset_all;
    [fig, axes, plot] = dl(w);
        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

    display('amark check complete')
    
    progressbar(0.72)
    
    %---------------aline-----------------
    aline('--');
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    aline('-',2.5);
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    figure(2);
    aline(':',2.0);
    ph(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    close, close, close,
    [fig, axes, plot] = dl(w);
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    [fig, axes, plot] = dm(w);
    [fig, axes, plot] = mp(ww);
    [fig, axes, plot] = ds(wd);
    [fig, axes, plot] = da(wd);
    figure(2)
    reset_default;
    [fig, axes, plot] = dl(w);
    reset_all;
    [fig, axes, plot] = dl(w);
        [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

    display('aline check complete')
    
    %-------------------log, lin-------------------
    
    progressbar(0.75)

    [fig, axes, plot] = dl(w);
    
    logx
    logy
    linx
    liny
    linx
    liny
    logx
    logy
    logx
    logy
    linlogbutton
    [fig, axes, plot] = de(w);
    [fig, axes, plot] = dh(w);
    logx
    logy
    linx
    liny
    logx
    [fig, axes, plot] = ds(wd);
    logx
    logy
    logz
    linlogbutton
    linx
    liny
    [fig, axes, plot] = da(wd2);
    logx
    logy
    linx
    liny

  [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

    display('linear / log check complete')

    %---------------check the dxy props-------------
    
    if ~continuous_flag
        gtk_dxyz_test_script;
    end
    
end % end of first parts, not extra only flag.

if extra_tag
    progressbar(0, 4)
end
%% Extra Functions Test

%% Get Data
rawfile1=IXTraw_file('../HET15870.RAW');
rawfile2=IXTraw_file('../HET16538.RAW');
rawfile3 = IXTraw_file('../MAP06220.RAW');
nsp=geti(rawfile1,'NSP1');
d2a=getspectra(rawfile1,[1:300]);
d2b=getspectra(rawfile2,[1:nsp]);
d2c=getspectra(rawfile3,[200:500]);
w1=getspectrum(rawfile1,25);
w2=getspectrum(rawfile2,233);
w = w1;
if lite_tag
    wd=rebin_x(d2a,[3000,300,13000]);
    wd2=rebin_x(d2c,[0,300,10000]);
end
wdd=[wd, wd2];
array_builder

wd.title = char({'multiline','test'});
w1.title = char({'multiline','test'});
display('loaded extra data');

progressbar(0.82, 4)

%% Test new functions

ds(wdd);

gtk_limits_check;

ds(wdd,'separation','on','acolor','green');

gtk_limits_check;

da(wdd);

da(wdd,0,1000);

gtk_limits_check;

ds(wdd,0,1000);

mp(wd,0,20000,0,30);

dl(ww);

ds(ww);

da(ww);

acolor('red','green','blue');

gtk_limits_check;

mp(ww);

progressbar(0.85)

ds(ww);
dl(ww);


de(ww);

pl(ww);

ph(ww);


acolor('yellow','red','blue');

dh(ww);
dl(w);
ph(w);

display(' ''new functions'' test completed ')

progressbar(0.9)

%% test default structures

mydefault = ixf_name_tag_properties('get','IXGDEFAULT', 'IXGDEFAULT');
mydefault.plot.color = 'red'; mydefault.figure.color = 'blue';
mydefault.axes.acolor = 'yellow';

dl(w1,'default',mydefault);
dl(w2);
ds(wd,'default',mydefault);
dl(w1);
ds(wd2);
ds(wd);

display('default structures test complete')

    [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

progressbar(0.93)

%% test dp, dd commands

[figure_, axes_, plot_] = dp(w1);
kf;
[figure_, axes_, plot_]=dd(w2);
acolor('red','green','blue');
dp(w2);
dd(w1);
kf
dp(w1);
close
close
dp(ww);
dd(ww);
dp(w2);
acolor('green');
dd(ww);
dp(ww);
dd(w1);
dd(ww,'color','blue','xlim',[0,500]);
dp(ww,'color','red','xlim',[0,500]);
dd(w1);
dp(w2);
acolor('red','blue','green');
dp(wd);
dd(wd);
dp(w2);

display('dp and de test complete')

    [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));

progressbar(0.95)

%% set and get handles tests

[figureH_, axesH_, plotH_] = dl(w1);
[fig, ax, plot] = get_all_handles('Default One Dimensional Plot','');
if figureH_ ~= fig || ax ~= axesH_ || plot ~= plotH_
    warning('figures and axes do not match in handles tests')
end

pl(w2);
pe(w1);
dl(w1,'name','plot2','tag','');
ph(w2,'name','plot2','tag','');

[fig, ax, plot] = get_all_handles('Default One Dimensional Plot','');

if figureH_ ~= fig || ax ~= axesH_
    warning('figures and axes do not match in handles tests')
end

if length(plot) ~= 3 
    warning('length of plot handle not correct in handles tests')
end

[fig2, ax2, plot2] = get_all_handles(2);

for i = 1:length(plot2)
    if fig2~=fig || ax2 ~= ax || plot2(i) ~= plot(i)
        warning('plot or figure handles do not match')
    end
end
set_plot(2,'color','yellow');
set_axes(plot2(1),'color','blue');
set_plot(ax(1), 'color','green');
set_plot(plot(1),'color','red');
set_figure(plot(2),'color','green');
set_plot('plot2','','color','green');

[fig2, ax2, plot2] = get_all_handles(plot(2));


if fig2~=fig || ax2 ~= ax || plot2 ~= plot(2)
    warning('plot or figure handles do not match')
end

ds(w1) 

da(w1)

progressbar(0.97)

%% Check Mgenie Style Commands

display('Checking mgenie style commands')

set_inst('MAP')
set_path('../')
ass(06220)
% assigned a map file

dl(34);
da(10:70);
dl(34:37);

w1 = getspectrum(34);
w2 = getspectrum(34:37);
wd1 = getspectra(10:70);

dl(w1)
da(wd1)
dl(w2)


%% check for import from file - using example with text in top line

w3 = read_ascii('../gtk_example.dat');
wread=read_ascii('../gtk_example.dat');
dl(w3);
dp(w3);
dl(w3)
if ~ continuous_flag

end
%% write to ascii

write_points(w1, '../gtk_example_write.dat');
write_hist(w1, '../gtk_example_write2.dat');
write_ascii(w1,'../gtk_example_write3.dat');

w4 = read_hist('../gtk_example_write.dat');
w5 = read_points('../gtk_example_write2.dat');
w6 = read_ascii('../gtk_example_write3.dat');

dl(w4);
pl(w5);
pm(w6);

if ~continuous_flag

    k = menu('gtk test script paused, please press continue','Continue','Keyboard');

    switch k
        case 1
        case 2
            keyboard;
        otherwise
            error('value entered on menu is not recognised')
    end
end
progressbar(0.99)

reset_all;

  [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));
 
%% complete and clear 
display('GTK test complete')
progressbar(1)
clear all
pack


% eroneous data
%dl(w,100,400,0,10,20,40)
%ds(wd,100,400,0,10,20,40,10,30)
%ds(wd,'separation','testing')
%ds(wdd,'separation','testing')

