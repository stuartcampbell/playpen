
function testclass = IXTtestclass(varargin)

testclass.base = IXTbase;
testclass.val = 0;
testclass.nx = int32(1);
testclass.val_static = ones(3);
testclass.int_static =  ones(4);
testclass.val_array = zeros(2);
testclass.int_arr = int32(zeros(2,2));
testclass.spectra = IXTspectra;
testclass.xhist = true;
testclass.label='test';
testclass.cell_string = [ 'one' ];
testclass.d2d = [ IXTdataset_2d ];
testclass = class(testclass,'IXTtestclass');

if (nargin > 0) 
    testclass = libisisexc('IXTtestclass','create',testclass,varargin);
end
