function map_out = map(varargin)
% IXTmap=map([workspace_number],cell)
% where cell is a 1-D cell array containing lists of spectra to group
% cell = {[spectrum list 1],[spectrum list 2], ...,  [spectrum list N]}
% cell may be prefilled or created implicitly in the argument call.
% The array of workspace numbers is optional, if they are not provided they
% are given numbers 1:no_of_workspaces
if(nargin > 2)
        disp('bad input to spec');
        return
end
if (nargin < 2)
    if(isa(varargin{1},'cell'))
        cell=varargin{1};
    else
        disp('bad input to spec');
        return
    end
else
    if(isa(varargin{2},'cell'))
        cell=varargin{2};
    else
        disp('bad input to spec');
        return
    end
end
n=size(cell,2); %essentially number of workspaces
if(size(cell,1)>1)
    disp('bad input to spec, one dimensional cell arrays only');
    return
end
if (nargin == 2)
    work_no=int32(varargin{1});
    if(size(work_no,2) ~= n )
        disp('workspace numbers incommensurate with spectra');
        return
    end
else
    work_no(1:n)=int32(1:n);
end
%%%preallocate some arrays

total_spec(1:n)=int32(1);
spec_ind(1:n)=int32(1);
%%%%
spec_ind(1)=int32(1);
for i=1:n
    if(size(cell{i},1)>1)
        disp('bad input to spec, one dimensional spectrum arrays only');
        return
    end
    n1=size(cell{i},2); %number of spectra in each workspace
    total_spec(i)=int32(n1);
    if(i ~= n)
        spec_ind(i+1)=int32(spec_ind(i) + total_spec(i));
    end
    spec_no(spec_ind(i):spec_ind(i)+total_spec(i)-1)=int32(sort(cell{i}));
end


map_out = IXTmap(IXTbase('entry',false,true),work_no,total_spec,spec_ind,spec_no);

