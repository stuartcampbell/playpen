function runfile_out = populate(runfile_in,dso,period,nchunk,ei,monei_info,m_axis,m_rebin,i_lim,d_axis,d_rebin,bgrd,opt)
%varargin{:}
%nargin
if size(ei) > 0 
    new_ei(1)=ei;
    new_ei(2)=0.0;
else
    new_ei=zeros(2);
end
runfile_out = libisisexc('IXTrunfile','populate_dso',IXTrunfile,runfile_in,dso,period,new_ei,monei_info,m_axis,m_rebin,d_axis,d_rebin,i_lim,bgrd,opt,nchunk);
%else
%runfile_out = libisisexc('IXTrunfile','populatenomap',IXTrunfile,runfile_in,varargin{1});
%else disp(['no valid arguments to the populate function']);
%end
