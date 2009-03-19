function mask_out = mask(list)
% IXTmask=mask(list)
% command line constructor for an IXTmask object, list is an array of
% spectra to be masked
if(isnumeric(list))
    if(size(list,1)==1)
        list=int32(sort(list));
        mask_out = IXTmask(IXTbase('entry',false,true),list);
    else
        disp('bad input to map constructor');
        return
    end
else
    disp('bad input to map constructor');
    return
end

