function [rpath,varargout] = make_path_from_par(par)

parlen=size(par,2);
inst=ixf_global_var('data_source','get','INST');
ext=ixf_global_var('data_source','get','ext');
path=ixf_global_var('data_source','get','path');

if (isempty(inst) || isempty(ext) || isempty(path))
    error('inst, ext or path undefined, run specific startup script for instrument')
end
% have to grow path by indexing since do not know length by initial input
% length
jj=0;
for i=1:parlen
    if(iscellstr(par{i}))
        % cell array of strings
        for j=1:size(par{i},2)
            [a,b,c]=fileparts(par{i}{j});
            if (strcmp(a ,'') && strcmp(c,''))
                rstring=par{i}{j};
                lstring=size(rstring,2);
                switch lstring
                    case 1
                        rstring=strcat('0000',rstring);
                    case 2
                        rstring=strcat('000',rstring);
                    case 3
                        rstring=strcat('00',rstring);
                    case 4
                        rstring=strcat('0',rstring);
                end
                %                rpath{jj+1}=strcat(path,inst,par{i}{j},'.',ext);
                rpath{jj+1}=strcat(path,inst,rstring,'.',ext);
            else
                rpath{jj+1}=par{i}{j};
            end
            jj=jj+1;
        end
    end
    if(isnumeric(par{i}))
        %% numeric array (length 1 or many)
        for j=1:size(par{i},2)
            rstring=num2str(int32(par{i}(j)));
            lstring=size(rstring,2);
            switch lstring
                case 1
                    rstring=strcat('0000',rstring);
                case 2
                    rstring=strcat('000',rstring);
                case 3
                    rstring=strcat('00',rstring);
                case 4
                    rstring=strcat('0',rstring);
            end
            rpath{jj+1}=strcat(path,inst,rstring,'.',ext);
            jj=jj+1;
        end
    end
    if(ischar(par{i}))
        % a single char or array of chars
        for j=1:size(par{i},1)
            [a,b,c]=fileparts(par{i}(j,:));
            if (strcmp(a ,'') && strcmp(c,''))
                rstring=par{i}(j,:);
                lstring=size(rstring,2);
                switch lstring
                    case 1
                        rstring=strcat('0000',rstring);
                    case 2
                        rstring=strcat('000',rstring);
                    case 3
                        rstring=strcat('00',rstring);
                    case 4
                        rstring=strcat('0',rstring);
                end
                %                rpath{jj+1}=strcat(path,inst, par{i}(j,:)  ,'.',ext);
                rpath{jj+1}=strcat(path,inst, rstring ,'.',ext);
            else
                rpath{jj+1}=par{i}(j,:);
            end
            jj=jj+1;
        end
    end
    % a cell of numerics
    if(iscellnum(par{i}))
        for j=1:size(par{i},2)
            rstring=num2str(int32(par{i}{j}));
            lstring=size(rstring,2);
            switch lstring
                case 1
                    rstring=strcat('0000',rstring);
                case 2
                    rstring=strcat('000',rstring);
                case 3
                    rstring=strcat('00',rstring);
                case 4
                    rstring=strcat('0',rstring);
            end
            rpath{jj+1}=strcat(path,inst,rstring,'.',ext);
            jj=jj+1;
        end
    end
end
varargout(1)={jj};