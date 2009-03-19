! this is a test program to test the reading of a maps raw file in chunks
! and reduce it to a dataset_1d state
program readinmapsfile
use IXMdataset_1d
use IXMdataset_2d
use IXMraw_file
use IXMlibcore

implicit none

type(IXTdataset_2d)::d2din
type(IXTdataset_1d)::cat1da,cat1db,cat1dc
type(IXTdataset_1d),allocatable::array1a(:),array1b(:),array1c(:)

type(IXTstatus)::status
integer(i4b)::i,ichunk,nchunk,piece,piece_end,ispec,nspec
integer(i4b)::start
type(IXTraw_file) :: raw_file
integer,allocatable :: spec_nums(:)

write(6,*) 'Starting dataset_1d test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

nchunk=50

call IXFopen_raw('map06215.RAW', raw_file, status)

! determine number of spectra in raw file
call IXFget_raw(raw_file, 'NSP1', nspec, status)

! determine number of arrays to split dataset_2d into, 
! according to nspec and nchunk

! some integer arithmetic with remainders
piece=nspec/nchunk

piece_end=mod(nspec,nchunk)
! if there is a remainder piece then there is another chunk
if (piece_end /= 0)then
  nchunk=nchunk+1
else
  piece_end=piece
endif

! allocate memory for array of dataset_1d objects to be catenated later on
! interim results are loaded into this array
call IXFalloc(array1a,nchunk,status)
call IXFalloc(array1b,nchunk,status)
call IXFalloc(array1c,nchunk,status)

! start loop over nchunks
do ichunk=1,nchunk

! ispec is the number of spectra to be read in at any one time in a chunk
! if total no of spectra is purely divisible by nchunk then it will always be the smae
! otherwise for the last piece on the end it will be different
  if (ichunk == nchunk)then
     ispec=piece_end
  else
     ispec=piece
  endif
  
  allocate (spec_nums(ispec))

! determines the spectrum numbers of raw file to read in,
! the range depends on how many chunks there are 
  start=((ichunk-1)*piece)+1
  spec_nums=(/(i,i=start,start+ispec-1)/) 

! this reads the raw file into a dataset_2d with y-dimension spec_nums(nspec)

  call IXFget_raw(raw_file, spec_nums,d2din,1, status)
  deallocate (spec_nums)
  
  ! this is where you do monitor specific integration to make a value
  
  
  
  ! for example integrates the d2din object and puts the output into array1d(ichunk)
  ! which is itself a dataset_1d
  call treatdata(array1a(ichunk),array1b(ichunk),array1c(ichunk),d2din,status)

! destroys piece of raw file in an IXTdataset_2d object 
! this routine contains an implicit memory leak, but will work provided there is 1Gb of RAM
! tested on my PC  
  call IXFdestroy(d2din,status)

enddo

call IXFcatarray_dataset_1d(array1a,cat1da,status)
call IXFcatarray_dataset_1d(array1b,cat1db,status)
call IXFcatarray_dataset_1d(array1c,cat1dc,status)

call IXFdealloc(array1a,status)
call IXFdealloc(array1b,status)
call IXFdealloc(array1c,status)


end




!*************
subroutine treatdata(d1da,d1db,d1dc,d2din,status)
use IXMdataset_2d
use IXMdataset_1d
type (IXTdataset_2d),intent(in)::d2din
type (IXTdataset_1d),intent(out)::d1da,d1db,d1dc
type (IXTstatus),intent(inout)::status
real(dp)::xmin,xmax


! so for example the components of the diag routine would be put in this 
! subroutine to give various dataset_1d outputs

! d1dout has come from the array of dataset_1d objects (array1d in main subroutine)
! you can therefore have more than one of these objects

xmin=2000
xmax=10000

call IXFintegrate_x_dataset_2d(d1da,d2din,xmin,xmax,status)
xmin=10000
xmax=15000

call IXFintegrate_x_dataset_2d(d1db,d2din,xmin,xmax,status)
xmin=15000
xmax=20000

call IXFintegrate_x_dataset_2d(d1dc,d2din,xmin,xmax,status)

end subroutine treatdata

