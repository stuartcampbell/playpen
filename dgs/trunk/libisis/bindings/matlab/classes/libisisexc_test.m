% $Id: libisisexc_test.m 63 2004-06-11 08:33:12Z faa59 $
% example program for calling MATLAb/F90 interface
% uses the ixtestclass class
w1=IXTtestclass(IXTbase('junk'),1,1,[ 1 2 3 4], [1,2,3,4]);
w2=IXTtestclass(IXTbase('junk'),1,1,[ 1 2 3 4], [1,2,3,4]);
w3 = w1 + w2
%moments.area.val=2.0
%moments.area.err=1.0
%dataset.name='test'
%dataset.title='test'
%dataset.nx=5
%dataset.x = [ 1 2 3 4 5 ]
% now loads 
%libisisexc(dataset, moments)

% Now unload libisisexc.dll (Windows) or libisisexc.mexglx (Linux) from memory
% We do this in case we are recompiling and testing to make sure
% we pick up any new library next time, but normally you will not need to
%clear libisisexc
