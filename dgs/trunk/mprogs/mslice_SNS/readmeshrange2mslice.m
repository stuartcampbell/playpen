function datout=readmeshrange2mslice(base,idxlst)
%datout=cell(length(idxlst),1);
datout.en=zeros(1,length(idxlst));
for idx=1:length(idxlst)
    fidx=idxlst(idx)
    switch floor(log10(fidx));
        case 0
           basecomb=strcat(base,'000');
        case 1
            basecomb=strcat(base,'00');
        case 2
            basecomb=strcat(base,'0');
        case 3
            basecomb=base;
    end
    filestr=strcat(basecomb,num2str(idxlst(idx)),'.in');
    datrd=readmesh2avg(filestr);
    if idx==1
        datout.I=zeros(length(datrd.I),length(idxlst));
        datout.e=zeros(length(datrd.err),length(idxlst));
        datout.Q=zeros([length(idxlst),size(datrd.Q)]);
    end
    datout.Q(idx,:,:)=datrd.Q;
    datout.S(:,idx)=datrd.I;
    datout.ERR(:,idx)=datrd.err;
    datout.en(idx)=datrd.E;
end
datout.Q=permute(datout.Q,[3 1 2]);
datout.det_group=(1:length(datout.Q))';
datout.filename=base;
