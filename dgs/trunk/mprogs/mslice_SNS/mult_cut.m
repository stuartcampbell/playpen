function cut_out=mult_cut(cut_in,number)

% function mult_cut(cut,number)
% multiply intensities in the cut by the given number

cut_out=cut_in;
cut_out.y=cut_out.y*number;
if isfield(cut_out,'e'),
   cut_out.e=cut_out.e*number;
end