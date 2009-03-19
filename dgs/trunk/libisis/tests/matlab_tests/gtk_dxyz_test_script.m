% gtk dxyz test script


        msgbox('begin dxyz test - please click on the figures (press return if multiple clicking has been allowed)','dxyz test');
        pause
        [fig, axes, plot] = dl(w);
        [x,y]=dxy('testing');
       [x,y] = dxy;
         dxyz('point')
        [x,y] = dxy;
        [x,y] = dxy('point','display_coords',true, 'no_points', 4);

            [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));


            ds(wd)
            [x,y,z] = dxyz;
            dxyz
            [x,y] = dxyz;
            [x,y,z] = dxyz('point');
            [x,y,z] = dxy('no_points',2);
            [x,y,z] = dxyz('point','display_coords',true, 'no_points', 4);
            
            da(wd)
            [x,y,z] = dxyz;
            [x,y,z,text] = dxyz('marker');
            dxyz
            dxyz('point ')
            [x,y] = dxyz;
            [x,y,z] = dxyz;
            [x,y,z] = dxyz('point');
            [x,y,z] = dxyc('no_points',2);
            [x,y,z] = dxyz('point','no_points',3, 'display_coords', true);
            
            ds(w)
            dxyz
            
            mp(ww)
            dxyz
            
            ds(wd)
            dxyz('closest_point',true,'display_coords',true);
            
            dl(w)
            dxy('closest_point',true,'display_coords',true);
            
            da(wd)
            dxyc('closest_point',true,'display_coords',true);
            
            [fh axesH, plotH] = get_all_handles;     close(fh(~(fh == 1)));
            
            progressbar(0.78)
    
        display('dxyz check complete')