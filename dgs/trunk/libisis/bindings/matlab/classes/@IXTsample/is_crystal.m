function out = is_crystal( sample )


if (max(sample.unit_cell)==0),  
        out=logical(0); %sample is powder 
else    
        out=logical(1); %sample is single crystal  
end   

