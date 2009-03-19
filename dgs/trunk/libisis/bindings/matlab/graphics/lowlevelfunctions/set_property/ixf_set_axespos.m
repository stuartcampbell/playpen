
function ixf_set_axespos(oid,ahdl,field1,tt,field2,tx,field3,ty)
%--------------------------------------------------------------------------
% Function Syntax: ixf_set_axespos(oid,ahdl,field1,tt,field2,tx,field3,ty)
% Purpose: to set axes position for the title
% Input: axes oid, axes handle, title oid, value, xlabel oid,value,
% ylabel oid,value
% Output: none
% Example: ixf_set_axespos('hdl',102.333,'title',title,'xlabel',xlabel,
% 'ylabel',ylabel)
% the above examples sets the position of axes
%--------------------------------------------------------------------------

% calculate space for titles:
% no cells in titles:
if iscell(tt)
    nt = numel(tt);
elseif size(tt,1) > 1
    nt = size(tt,1);
else
    nt = 2;
end

if iscell(tx)
    nx = numel(tx);
elseif size(tx,1) > 1
    nt = size(tx,1);
else
    nx = 2;
end
if iscell(ty)
    ny = numel(ty);
elseif size(ty,1) > 1
    nt = size(ty,1);

else
    ny = 2;
end
% units per single height of line (quick fix assuming default aspect ratio and font size)
h = 0.03833;
% allow for up to 4 lines in tx and ty, and 5 lines in tt:
xplo=min(0.13+(ny-1)*h,0.245);  yplo=min(0.11+(nx-1)*h,0.225);  xphi=0.905;   yphi=max(0.925-(nt-1)*h,0.772);
pos = [xplo,yplo,xphi-xplo,yphi-yplo];
set(ahdl,'position',pos);
%