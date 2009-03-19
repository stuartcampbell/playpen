function [DSO,varargout]=setup_DSO_bank(inst)

if strcmp(inst,'MAP')
    DSO=setup_maps_data_source;
    %%% this is the standard banking for MAPS
    if nargout == 2
        varargout(1)={IXTmap({1:17280,17281:18432,18433:32256,32257:41472})};
    end
end
if strcmp(inst,'MAR')
    DSO=setup_mari_data_source;
    %%% all detectors are equal in MARI
    if nargout == 2
        varargout(1)={[]};
    end
end
if strcmp(inst,'MER')
    DSO=setup_merlin_data_source;
    %%% all detectors are equal in MERLIN (for the moment)
    if nargout == 2
        varargout(1)={[]};
    end
end
if strcmp(inst,'HET')
    DSO=setup_het_data_source;
    %%% this is the standard banking for HET
    if nargout == 2
        varargout(1)={IXTmap({[6:49,51:72,75:96,101:356],401:2584})};
    end
end