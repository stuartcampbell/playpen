% Script used by Dickon when testing Libisis
%
% Now redundant, and out of date. Should be replaced by calling the function:
%   >> libisis_init
%
% T.G.Perring

ipath=pwd;
if(isdir(ipath))
    STRING = [ipath '/bindings/matlab/classes'];
    if(isdir(STRING))
        addpath (STRING);
    end
    STRING = [ipath '/bindings/matlab/utilities'];
    if(isdir(STRING))
        addpath (STRING);
    end
    STRING = [ipath '/applications/matlab/homer'];
    if(isdir(STRING))
        addpath (STRING);
    end
    STRING = [ipath '/applications/matlab/homer_gui'];
    if(isdir(STRING))
        addpath (STRING);
    end    
    STRING = [ipath '/applications/matlab/MARI'];
    if(isdir(STRING))
        addpath (STRING);
    end
    STRING = [ipath '/applications/matlab/MAPS'];
    if(isdir(STRING))
        addpath (STRING);
    end
    STRING = [ipath '/applications/matlab/HET'];
    if(isdir(STRING))
        addpath (STRING);
    end
    STRING = [ipath '/applications/matlab/MERLIN'];
    if(isdir(STRING))
        addpath (STRING);
    end

    STRING = [ipath '/applications/matlab/mgeniefuncs'];
    if(isdir(STRING))
        addpath (STRING);
    end

    STRING = [ipath '/bindings/matlab/graphics'];
    if(isdir(STRING))
        addpath (STRING);
    end
end
clear STRING ipath;
cd tests;
gtk_init;