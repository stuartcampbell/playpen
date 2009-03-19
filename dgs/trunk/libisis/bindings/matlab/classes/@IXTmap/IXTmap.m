function map = IXTmap(varargin)
% IXTmap(IXTbase,[work_no],[total_spec],[spec_ind],[spec_no]);
% or
% IXTmap([workspace_number],cell)
% where cell is a 1-D cell array containing numerical lists of spectra to group
% cell = {[spectrum list 1],[spectrum list 2], ...,  [spectrum list N]}
% cell may be prefilled or created implicitly in the argument call.
% The array of workspace numbers is optional, if they are not provided they
% are given numbers 1:no_of_workspaces
map.base = IXTbase;
map.work_no=[int32(1)];
map.total_spec=[int32(1)];
map.spec_ind=[int32(1)];
map.spec_no=[int32(1)];
map = class(map,'IXTmap');

if nargin ==2 || nargin==1
    if (nargin ==1) && isa(varargin{1},'cell')
        cell=varargin{1};
        n=size(cell,2); %essentially number of workspaces
        work_no(1:n)=int32(1:n);
    elseif isa(varargin{2},'cell') && isnumeric(varargin{1})
        cell=varargin{2};
        n=size(cell,2); %essentially number of workspaces
        work_no=int32(varargin{1});
        if(size(work_no,2) ~= n )
            disp('workspace numbers incommensurate with spectrum mapping');
            return
        end
    else
       disp('bad input to IXTmap, help IXTmap for proper syntax');
        return        
    end
    if iscellnum(cell)
        % then only numeric arrays supplied no workspace numbers
        if(size(cell,1)>1)
            disp('bad input to IXTmap, one dimensional cell arrays only');
            return
        end
        %%%preallocate some arrays
        total_spec(1:n)=int32(1);
        spec_ind(1:n)=int32(1);
        %%%%
        spec_ind(1)=int32(1);
        for i=1:n
            if(size(cell{i},1)>1)
                disp('bad input to IXTmap, one dimensional spectrum arrays only');
                return
            end
            n1=size(cell{i},2); %number of spectra in each workspace
            total_spec(i)=int32(n1);
            if(i ~= n)
                spec_ind(i+1)=int32(spec_ind(i) + total_spec(i));
            end
            spec_no(spec_ind(i):spec_ind(i)+total_spec(i)-1)=int32(sort(cell{i}));
        end
    else
        disp('bad input to IXTmap, cell array must be numeric');
        return

    end
    map = libisisexc('IXTmap','create',map,{IXTbase('entry',false,true),work_no,total_spec,spec_ind,spec_no});
elseif (nargin > 0)
    map = libisisexc('IXTmap','create',map,varargin);
end