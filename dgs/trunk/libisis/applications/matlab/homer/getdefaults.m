function [present,argout,varargout] = getdefaults(present,argout)
% if default rebin flag requested then its default value is true except
% when 'nod_rebin' is specified
if nargout==3
    varargout(1)={true};
end
list=fieldnames(argout);
for i=1:numel(list)
    %special case of range default only being determined if
    %normalisation is numeric
    if ~strcmp(list{i},'range')
        % get default variable if it exists
        tempvar=ixf_global_var('homer','get',list{i});
        % captures 'no' prefix for all values
        if isempty(argout.(list{i}));
            if  present.(list{i})
                if nargout==3 %fills default rebinning parameters for homer
                    %special case for nod_rebin
                    if strcmp(list{i},'d_rebin')
                        varargout(1)={false};
                    end
                end
                present.(list{i})=0;
            else
                if ~isempty(tempvar)
                    argout.(list{i})=tempvar;
                    present.(list{i})=1;
                end
            end
        end
    end
end