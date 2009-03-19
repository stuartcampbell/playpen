function runfile = plus(a,b)
%--- Help for IXTrunfile/plus.m---
% call syntax: a+b or plus(a,b)
%
% adds an 1XTrunfile object to another 1XTrunfile
% object element-by-element or scaler
%
% inputs: a = 1XTrunfile object, b = 1Xtrunfile object or
% scalar
%
% output: 1XTrunfile object.. runfile = a+b

len_a = length(a);
len_b = length(b);


if iscell(b) && len_a == len_b           % need to use cell syntax if either is a cell.
        runfile = a;
        for i = 1:length(a)                  % they must be equal length though
            runfile(i) = plus_runfile(a(i),b{i});
        end
        
    elseif iscell(a) && len_a == len_b
        runfile = b;
        for i = 1:length(a)
            runfile(i) = plus_runfile(a{i},b(i));
        end
        
    else 
        runfile = plus_runfile(a,b);
end
    



%--------function deals with numbers and datasets only------------

   function runfile = plus_runfile(a, b)
    
len_a = length(a);
len_b = length(b);

% if single object and array of runfiles, then expand the single object
% into an array of identical objects.
if len_a == 1 && isa(b,'IXTrunfile')
    a(1:len_b) = a;
    len_a = len_b;
elseif len_b == 1 && isa(a,'IXTrunfile')
    b(1:len_a) = b;
    len_b = len_a;
end

if len_a == len_b 
        
    if isa(b,'IXTdataset_1d')
        error('plus is not defined for IXTdataset_1d with IXTrunfile operations')
%       runfile = libisisexc('IXTrunfile','dataset_1d_plus',IXTrunfile,a,b);
    else
        runfile = libisisexc('IXTrunfile','plus',IXTrunfile,a,b);
    end
 
 elseif  (len_a == 1 && isnumeric(b)) || (len_b == 1 && isnumeric(a))    % single dataset, array of numbers 
        error('plus is not defined for IXTrunfile with an array of numbers')
%        runfile = libisisexc('IXTrunfile','array_plus',IXTrunfile,a,b);

elseif isa(b,'IXTdataset_1d')        % looking at array of d1d added to the d2d or single d1d added to array d2d
        error('plus is not defined for IXTrunfile with an array of IXTdataset_1d')
%   len_d2d = length(a);
%    len_d1d = length(b);
%    
%     for i = 1:len_d2d
%         
%         len_y = length(a(i).y);
%         
%         if len_d1d>=len_y
%             runfile(i) = libisisexc('IXTrunfile','dataset_1d_power',IXTrunfile,a(i),b(1:len_y));
%             len_d1d = len_d1d - len_y;
%             
%             if len_d1d ~= 0
%                 b=b((len_y+1):end);
%             end
%            
%        else 
%            error('dataset_1d dimensions inconsistent with runfile dimensions')
%        end
%        
%    end    

else

    error('a and b have incompatable column dimensions')
    
end