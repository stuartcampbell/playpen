function alias(varargin)
%-------------------------------------------------------------------------
%function syntax: alias('function_old','function_new','Comments to add')
%purpose: alias for making similar functions with different names
%input :  function (old) name, function (new) name, comments
%output:none
%example: alias('uinv_setdefaultprop','sdp','For setting default plot')
%-------------------------------------------------------------------------
totArg = numel(varargin);
std = 2;
ixf_validate_arg(std,totArg,'greater');
ps = matlabroot;
%open old file and copy the func and comments
ostr = varargin{1};
ostr = [ostr '.m'];
oh = fopen(ostr,'r');
if (isempty(oh) || oh < 0)
    error('1: Cannot open file');
end
cstr = '';
while (~feof(oh))
    str = fgetl(oh);    
    ind = strfind(str,'global');
    if (isempty(ind))
        cstr = [cstr str];
    else
        break;
    end    
end
str = cstr;
nstr = strrep(str,varargin{1},varargin{2});
str = nstr;
fclose(oh);

%open new file and copy the help and func name
nstr = varargin{2};
%use global path
 IXG_UF_STRING = ixf_global_var('libisis_graphics','get', 'IXG_UF_STRING');
nstr = [IXG_UF_STRING '/' nstr '.m'];
chkflag = fopen(nstr,'r');
nh = fopen(nstr,'w+t');
if (isempty(nh) || nh < 0)
    error('1: nh Cannot open file');
end
std = 1;
n = strfind(str,'%');
tot = numel(n);
fprintf(nh,'%s\n',str(std:n(1)-1));
std = n(1);
for i = 2:tot
    str(std:n(i)-1);
    fprintf(nh,'%s\n',str(std:n(i)-1));
    std = n(i);
end
fprintf(nh,'%s\n',str(std:end));
fclose(nh);



%open old file and copy function name
ostr = varargin{1};
ostr = [ostr '.m'];
oh = fopen(ostr,'r');
if (isempty(oh) || oh < 0)
    error('2: Cannot open file');
end
while (~feof(oh))
    str = fgetl(oh);
    n = strfind(str,'function');
    if (~isempty(n))
        break;
    end
end
n = strfind(str,' ');
rstr = str(n(1)+1:end);
% n = strfind(rstr,'%');
std = 1;
fclose(oh);

%rstr is the final string to be manipulated
rstr = strrep(rstr,'varargin','varargin{:}');


%final write
nh = fopen(nstr,'a');
if (isempty(nh) || nh < 0)
    error('3: Cannot open file');
end
fprintf(nh,'%s;\n',rstr);
fclose(nh);


%----if you want to add comments-----
if (totArg == 3)
    comm = varargin{3};
    
    %global structure
    IXG_GTK_INSTALLATION_PATH = ixf_global_var('libisis_graphics','get','IXG_GTK_INSTALLATION_PATH');
    %additional help file prepared for this
    name = 'addhelp.txt';
    filename = [IXG_GTK_INSTALLATION_PATH '\' name];
    
    fid=fopen(filename,'a+');
    if (isempty(fid))        
        error('Cannot open file addhelp.txt');
    end
    chkstr = fscanf(fid,'%s');
    chkindex = findstr(chkstr,varargin{2});
    if (isempty(chkindex))
        fprintf(fid,'%s- %s\n',varargin{2},varargin{3});
    end
    fclose(fid);
end