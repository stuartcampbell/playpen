file_name='fileio_test.nxs';
disp(strcat('creating ',file_name,' and testing read/write'));
fio=open(IXTfileio,file_name,4);
% write instance of testclass
test=IXTtestclass;
test.label='hello';
write(test,fio,'test');
% write instance of moderator
mod=IXTmoderator;
mod.distance=7.1;
%mod.name='hello';
write(mod,fio,'mod');
% write instance of runfile
rf=IXTrunfile;
write(rf,fio,'rf');
%
fio=close(fio);
% now read back and check
fio=open(IXTfileio,file_name,1);
% read testclass and check
test_check=read(IXTtestclass,fio,'test');
if isequal(test_check,test)
    disp('read back OK of testclass');
else
    disp('ERROR in read back of testclass');
end
% read moderator and check
mod_check=read(IXTmoderator,fio,'mod');
if isequal(mod_check,mod)
    disp('read back OK of moderator');
else
    disp('ERROR in read back of moderator');
end
% read runfile and check
rf_check=read(IXTrunfile,fio,'rf');
if isequal(rf_check,rf)
    disp('read back OK of runfile');
else
    disp('ERROR in read back of runfile');
end
%
fio=close(fio);
%
a=strcat('/home/faa59/nexus/applications/nxconvert -x "',file_name,'" test.xml');
system(a);
fio=open(IXTfileio,'test.xml',1);
mod_check=read(IXTmoderator,fio,'mod');
if isequal(mod_check,mod)
    disp('read back OK of moderator');
else
    disp('ERROR in read back of moderator');
end
fio=close(fio);
