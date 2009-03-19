function n_axes = get_n_axes( proj_tofndgs )
%GET Summary of this function goes here
%  Detailed explanation goes here

if(get_n_axes( proj_tofndgs.proj_info )== get_n_axes(proj_tofndgs.proj_data)),
    n_axes=get_n_axes( proj_tofndgs.proj_info );
else
    n_axes=int8(-1);
end

    