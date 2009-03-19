 subroutine IXBdiag_testclass(nlhs, plhs, nrhs, prhs, status)
    use IXMlibcore
    use IXMm_dataset_1d
    use IXMm_dataset_2d
    use IXMm_datum
	use IXMmatlab_interface
	use IXMm_testclass
    use IXMm_isis_raw_file
    
    implicit none
 	
 	integer :: nlhs, nrhs, ntimechan, nspec, ntimechan2, nspec2, i
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	!integer :: spec_nums(6) = (/ 9,10,15,20,25,30 /)
	integer, allocatable :: spec_nums(:)
	integer, allocatable :: counts(:,:), counts2(:,:)
	
	
	real(dp), allocatable :: time_chans(:), time_chans2(:)
	real(dp) :: xmin, xmax, ymin, ymax
	
	type(IXTstatus) :: status
    type(IXTisis_raw_file) :: raw_file,raw_file2
    type(IXTdataset_2d) :: w2d, w2d2, w2d3
    type(IXTdataset_1d) :: w1d, w1d2
    
    logical:: xdist,xhist,ydist,yhist
    
    character(len=long_len) :: x_label, y_label, title
    character(len=short_len) :: x_units, y_units
    
    

! Open and read a RAW file into a dataset_2d
! spec_nums is an array specifying the spectra to be
! included in the output w2d    
call IXFopen_raw('mar09948.raw', raw_file, status)
call IXFopen_raw('mar10432.raw', raw_file2, status)
! call IXFget_raw(raw_file, 'NSP1', nspec, status)

! Calls to routines to extract no. of time channels 
! and spectra from raw_file
call IXFget_raw(raw_file, 'NTC1', ntimechan, status)
call IXFget_raw(raw_file2, 'NTC1', ntimechan2, status)
call IXFget_raw(raw_file, 'NSP1', nspec, status)
call IXFget_raw(raw_file2, 'NSP1', nspec2, status)
! Select the number of spectra to be included in w2d
! NB Matlab struggles plotting the full nspec
! using ixf_multiplot2d
allocate(spec_nums(20)) !nspec))
! Select the spectra to included in w2d
spec_nums=(/(i,i=4,23)/)
! Finally write the chosen spectra from raw_file to w2d
call IXFget_raw(raw_file, spec_nums, w2d, status)
call IXFget_raw(raw_file2, spec_nums, w2d2, status)

! Need to redefine time channels because time zero is
! used as a dump for spurious signals
allocate(time_chans(ntimechan+1))
call IXFget_raw(raw_file, 'TIM1', time_chans, status)
allocate(time_chans2(ntimechan2+1))
call IXFget_raw(raw_file2, 'TIM1', time_chans, status)
! Need to match the counts, a 2d array, with the number of time
! channels and spectra
allocate(counts(ntimechan+1, 20))
allocate(counts2(ntimechan2+1, 20))
! Finally write the counts in to w2d
call IXFget_raw(raw_file, spec_nums, counts, status)
call IXFget_raw(raw_file2, spec_nums, counts2, status)


! Now carry out some operations on the dataset_2d
title = 'w1d'
x_label = 'tof'
x_units = 'usec'
xhist=.true.
xdist=.true.
y_label = 'spectrum'
y_units = 'spec no'
xmin=50
xmax=2000

call IXFset_dataset_2d(w2d,status,title=title)
title = 'w1d'
call IXFintegrate_x_dataset_2d(w1d,w2d,xmin,xmax,status)
call IXFset_dataset_1d(w1d,status,title=title,x_units=x_units)
title = 'w1d2'
call IXFintegrate_x_dataset_2d(w1d2,w2d2,xmin,xmax,status)
call IXFset_dataset_1d(w1d2,status,title=title,x_units=x_units)
!title = 'ratio'
call IXFdivide_dataset_2d(w2d3,w2d,w2d2,status)


! Send w2d and w1d to Matlab
call IXBsendToBinding(plhs(1),prhs(1),' ', 1, 0, w2d,status)
call IXBsendToBinding(plhs(2),prhs(2),' ', 1, 0, w1d,status)
call IXBsendToBinding(plhs(3),prhs(3),' ', 1, 0, w2d2,status)
call IXBsendToBinding(plhs(4),prhs(4),' ', 1, 0, w1d2,status)
call IXBsendToBinding(plhs(5),prhs(5),' ', 1, 0, w2d3,status)    
! Try and eliminate any confusion and memory leakage    
deallocate(spec_nums,time_chans,counts,counts2)

end subroutine IXBdiag_testclass


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine IXBdiag_testclass_XYZ(nlhs, plhs, nrhs, prhs, status)
   
! Include the modules required 
    use IXMlibcore
    use IXMm_dataset_1d
    use IXMm_dataset_2d


    use IXMm_datum
	use IXMmatlab_interface
	use IXMm_testclass
    use IXMm_isis_raw_file
!	use IXMisis_raw_file

!    use IXMtestclass_m
   	implicit none
   
! Define the number of inputs and outputs + their pointers for the matlab interface
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
    type(IXTisis_raw_file) :: raw,raw_file
!real(dp) :: s2(3,5),e2(3,5)

! Define the types of inputs and outputs and argument types
    real(dp) :: xmin,xmax,ymin,ymax
    integer(i4b) :: spec_lo,spec_hi
    integer :: spec_nums(4) = (/ 1, 2, 3, 4 /)
    integer :: vali(1), ntimechan, nspec
    real(dp), allocatable :: time_chans(:)
    integer, allocatable :: counts(:,:)
    type (IXTdatum)::iresA,iresB
    real(dp),allocatable :: x(:),y(:),signal(:,:),error(:,:),x2(:),y2(:),signal2(:,:),error2(:,:)
    real(dp),allocatable :: size1d(:)
    integer :: i,j,k,nx,bin,npt
    character(len=120):: line
    type(IXTdataset_1d) :: w1d
    character(len=256) :: w1dout
    type(IXTdataset_2d) :: w2d
    logical:: xdist,xhist,ydist,yhist
    character(len=100) :: files(1) = (/ 'w8.dat' /)!,files2(2) = (/ 'w7.dat','w7.dat' /) 
    character(len=long_len) :: x_label,y_label,title,title2
    character(len=name_len) :: array_name,array_name2
    character(len=short_len) :: x_units,y_units

! Start of the program proper
    call IXBwriteline( 'Starting testclas_diag2 test program thank you please sir', status)
    


! Extract data from .dat files and insert into a 2d array with dimensions tof,
! spectrum no., signal and signal error  
    do j = 1,size(files)
        k = len_trim(files(j))
        open(1,file=files(j)(:k))
        read(1,'(71x,i)') npt
        npt = npt - 1
         read(1,'(a80)') line
        read(1,'(a80)') line
        read(1,'(a80)') line
            if (.not. allocated(signal)) then
                allocate(signal(npt,size(files)))
                allocate(error(npt,size(files)))
                allocate(x(npt+1),y(size(files)))
            endif
        y(j) = j
            do i = 1,npt
                read(1,*) bin,x(i),signal(i,j),error(i,j)
            enddo
        read(1,*) bin,x(npt+1)
        close(1)
    enddo




! Construct the 2D array w2d from 'files'
    array_name = 'working'
    title = 'w2d'
    x_label = 'tof'
    x_units = 'usec'
    xmin = 100
    xmax = 800
    xhist=.true.
    xdist=.true.
    y_label = 'spectrum'
    y_units = 'spec no'
    
    call IXFSet_dataset_2d(w2d,status,name=array_name,signal=signal,error=error,x=x,  &
              x_distribution=xdist,x_label=x_label,x_units=x_units, &
              y=y,y_distribution=ydist,y_label=y_label,y_units=y_units,title=title)
    

    call IXFintegrate_x_dataset_2d(w1d,w2d,xmin,xmax,status)
    call IXFset_dataset_1d(w1d,status,title='arse')



! Print the 2D array and results of the manipulations to screen

! This is the matlab binding link to view arrays
    !call IXBmatlabWriteDataset_2d(w2d,plhs(1),prhs(1),status)
    
    call IXBsendToBinding(plhs(1),prhs(1),' ', 1, 0, w2d,status)
    
    !call IXBmatlabWriteDataset_2d(w2d,plhs(2),prhs(2),status)
    !call IXBmatlabWriteDataset_1d(w1d,plhs(2),prhs(2),status)
    
    call IXBsendToBinding(plhs(2),prhs(2),' ', 1, 0, w1d,status)
    
    !call IXBgetFromBinding(prhs(1),' ', 1, 0, w1d, status)






!type(IXTisis_raw_file) :: 'D:\MariFTP\MAR09948.RAW'
!call IXFopen_raw(raw,status)


!type(IXTdataset_2d) :: d2
!integer :: ispec(:)
!call IXFget_spectra(raw,ispec,d2,status)
!type(IXTisis_raw_file) :: 'D:\MariFTP\MAR09948.RAW'


!call IXBsendtobinding(plhs(1),prhs(1),' ', 1, 0, d2,status)




  end subroutine



