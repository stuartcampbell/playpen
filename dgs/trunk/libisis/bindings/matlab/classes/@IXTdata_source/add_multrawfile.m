function self = add_multrawfile(self,varargin)
%dso=add_multrawfile(dso,'raw_file1','rawfile2'...'rawfileN')
%dso is an IXTdata_source object 
%'raw_file1'->'raw_fileN' are strings defining the location of the raw data
%files to be loaded andc ombined together
for i=1:nargin-1
   self=add_item(self,varargin{i},'rawfile_mult');
end