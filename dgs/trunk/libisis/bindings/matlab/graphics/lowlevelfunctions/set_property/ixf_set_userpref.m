

function ret = ixf_set_userpref(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ret = ixf_set_userpref()
%Purpose: to set the default properties of plots,figure or axes
%Input: name value pair control property
%Output: true or false
%Example: ixf_set_userpref('acolor','black')
%the above examples will display each figure with axes color black
%(default)
%--------------------------------------------------------------------------

%global structure
IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');


%tot args
totArg = numel(varargin);
ret = IXG_ST_STDVALUES.true;

st_local_default = IXG_ST_DEFAULT;
%parse
st_local_default = ixf_set_prop(st_local_default,totArg, varargin{:});

st_local_default.figure.name = 'IXGDEFAULT';
st_local_default.figure.tag = 'IXGDEFAULT';

%at the end change the default global strucutre
IXG_ST_DEFAULT = st_local_default;

ixf_name_tag_properties('set','IXGDEFAULT', 'IXGDEFAULT', IXG_ST_DEFAULT);