function  rf = homer_solid (rf, dso, values)

[mappath,found]=findpath(dso,'detmapfile');

if(~found)
    disp('no mapping file found for runfile to be normalised, only 1:1 white beam normalisation possible')
 
end
rf=solidangle(rf,dso,values.solid);