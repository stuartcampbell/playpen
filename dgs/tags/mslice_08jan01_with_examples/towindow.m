function towindow(data)

% put structure <data> previously obtained using data=fromwindow;
% back into the Mslice: ControlWindow

h_cw=findobj('Tag','ms_ControlWindow');
if ~isempty(h_cw),
   % === update UserData field in the ControlWindow 
   set(h_cw,'UserData',data);
   % === update data file : ms_DataFile
   if isfield(data,'filename')&~isempty(data.filename)&...
         ischar(data.filename),
      h=findobj(h_cw,'Tag','ms_DataFile');
      if ~isempty(h),
         set(h,'String',deblank(data.filename));
      end
   end
   % === update data file directory : ms_DataDir
   if isfield(data,'filedir')&~isempty(data.filedir)&...
         ischar(data.filedir),
      h=findobj(h_cw,'Tag','ms_DataDir');
      if ~isempty(h),
         set(h,'String',deblank(data.filedir));
      end
   end
   % === update detector file : ms_PhxFile
   if isfield(data,'detfilename')&~isempty(data.detfilename)&...
         ischar(data.detfilename),
      h=findobj(h_cw,'Tag','ms_PhxFile');
      if ~isempty(h),
         set(h,'String',deblank(data.detfilename));
      end
   end   
   % === update detector file directory : ms_PhxDir
   if isfield(data,'detfiledir')&~isempty(data.detfiledir)&...
         ischar(data.detfiledir),
      h=findobj(h_cw,'Tag','ms_PhxDir');
      if ~isempty(h),
         set(h,'String',deblank(data.detfiledir));
      end
   end   
   % === update TitleLabel : ms_TitleLabel
   if isfield(data,'title_label')&~isempty(data.title_label)&...
         ischar(data.title_label),
      h=findobj(h_cw,'Tag','ms_TitleLabel');
      if ~isempty(h),
         set(h,'String',data.title_label);
      end
   end
   % === update IntensityLabel : ms_IntensityLabel
   if isfield(data,'axis_unitlabel')&~isempty(data.axis_unitlabel)&...
         ischar(data.axis_unitlabel),
      h=findobj(h_cw,'Tag','ms_IntensityLabel');
      if ~isempty(h),
         temp=deblank(data.axis_unitlabel(end,:));
         set(h,'String',temp);
      end
   end
   % === update efixed : ms_efixed
   if isfield(data,'efixed')&~isempty(data.efixed)&...
         isnumeric(data.efixed)&(prod(size(data.efixed))==1),
      h=findobj(h_cw,'Tag','ms_efixed');
      if ~isempty(h),
         set(h,'String',num2str(data.efixed));
      end
   end
   % === update emode : ms_emode
   if isfield(data,'emode')&~isempty(data.emode)&...
         isnumeric(data.emode)&(prod(size(data.emode))==1)&...
         (sum(([1 2]-data.emode)==0)==1),
      h=findobj(h_cw,'Tag','ms_emode');
      if ~isempty(h),
         set(h,'Value',data.emode);
      end
   end
   %=== if in single crystal psd mode clear stored slice if any (assume new data in)
   h=findobj(h_cw,'Tag','ms_det_type');
   if ~isempty(h)&ishandle(h),
      if get(h,'Value')==1,
         ms_slice('clear');
         %disp('Clearing stored slice data.');
      end
   end
else
   disp('No Control Window opened.');
end
