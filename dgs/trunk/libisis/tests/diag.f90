module IXMdiag
    use IXMlibcore
    use IXMdataset_1d
    use IXMdataset_2d
    use IXMdatum
    use IXMisis_raw_file
public:: IXFdiag
contains
 subroutine IXFdiag(bad_det,status)
  
    implicit none
 	
 	integer(i4b) :: i,run_no
 	integer :: ntimechan, nspec, ntimechan2, nspec2, ntimechan3, nspec3, ntimechan4, nspec4
 	integer :: ndet, nmon, nspec_hard 
 	real*4 c, L1, rvpb(64), t_delay, delt_sum, L2_sum, L2_mean, e_i, e_trans_min, e_trans_max, e_min, e_max 
 	real(dp) :: ratio_mean, ratio_sum_sum
 	real,allocatable :: ratio_sum(:)
 	real(dp) :: t_min, t_max
 	real,allocatable :: L2(:), TTHE(:), DELT(:)
 	
	! Input a hard mask
    integer :: hard(1) = (/ -10 /)
	!integer :: spec_nums(6) = (/ 9,10,15,20,25,30 /)
	integer, allocatable :: spec_nums(:), spec_nums2(:), spec_nums3(:), spec_nums4(:)
	integer, allocatable :: counts(:,:), counts2(:,:), counts3(:,:), counts4(:,:)
	integer :: ngood_stab, nbad_stab, ngood_back, nbad_back, ngood_zero, nbad_zero
	integer :: ngood_v1lo, nbad_v1lo, ngood_v1hi, nbad_v1hi, ngood_v1zi, nbad_v1zi
	integer :: ngood_v2lo, nbad_v2lo, ngood_v2zi, nbad_v2hi, ngood_v2hi, nbad_v2zi
	integer :: ngood_hard, nbad_hard, ngood, nbad
	integer,allocatable :: good_det_stab(:), bad_det_stab(:), good_det_back(:), bad_det_back(:)
	integer,allocatable :: good_det_zero(:), bad_det_zero(:), good_det_hard(:), bad_det_hard(:)
	integer,allocatable :: good_det_v1lo(:), bad_det_v1lo(:), good_det_v1hi(:), bad_det_v1hi(:)
	integer,allocatable :: good_det_v1zi(:), bad_det_v1zi(:)
	integer,allocatable :: good_det_v2lo(:), bad_det_v2lo(:), good_det_v2hi(:), bad_det_v2hi(:)
	integer,allocatable :: good_det_v2zi(:), bad_det_v2zi(:)
	integer,allocatable :: good_det(:)
	integer(i4b),allocatable:: bad_det(:) 
	
	real(dp),allocatable :: ratio_hl_err(:), ratio_ll_err(:)
	
	real(dp), allocatable :: time_chans(:), time_chans2(:), time_chans3(:), time_chans4(:)
	real(dp) :: xmin, xmax, ymin, ymax
	real(dp) :: bmin, bmax
	real(dp) :: z, stability, ratio_ll, ratio_hl, ratio_median, factor, median_back
	real(dp) :: vlow, vhigh, s_sig, v_sig, median_v1, median_v2
	real(dp), allocatable :: van_ratio(:), van_ratio_err(:)
	
	real(dp),allocatable :: s(:), s_back(:), s_fullint(:), sv1(:), sv2(:) ! signal array pointer
    real(dp),allocatable :: e(:), e_back(:), e_fullint(:), ev1(:), ev2(:) ! error array pointer
    real(dp),allocatable :: x(:), x_back(:), x_fullint(:), xv1(:), xv2(:) ! x-array pointer
	
	
	type(IXTstatus),intent(inout) :: status
    type(IXTisis_raw_file) :: raw_file, raw_file2, raw_file3, raw_file4
    type(IXTdataset_2d) :: w2d, w2dv1, w2dv2, w2dmv1, w2dvn1, w2d3
    type(IXTdataset_1d) :: w1d, w1dv1, w1dv2, w1dmv1, w1dVrat, w1d_back
    type(IXTdataset_1d) :: w1dv1_back, w1d_back_norm
    
    logical:: xdist, xhist, ydist, yhist
    logical,allocatable :: mask_stab(:), not_mask_stab(:), mask_back(:), not_mask_back(:)
    logical,allocatable :: mask_zero(:), not_mask_zero(:), mask_hard(:), not_mask_hard(:)
    logical,allocatable :: mask_v1lo(:), not_mask_v1lo(:), mask_v1hi(:), not_mask_v1hi(:)
    logical,allocatable :: mask_v1zi(:), not_mask_v1zi(:)
    logical,allocatable :: mask_v2lo(:), not_mask_v2lo(:), mask_v2hi(:), not_mask_v2hi(:)
    logical,allocatable :: mask_v2zi(:), not_mask_v2zi(:)
    logical,allocatable :: mask_total(:), not_mask_total(:)
    
    character(len=long_len) :: x_label, y_label, s_label, title
    character(len=short_len) :: x_units, y_units, s_units
    character*256 :: message1, message2, message3, message4, message5, &
    message6, message7, message8, message9, message10, message11, &
    message12, message13, message14, message15, message16, message17, &
    message18, message19, message20,message21,message22
    

! Open and read a RAW file into a dataset_2d, spec_nums is an array specifying
! the spectra to be included in the output w2d (=> Ei=17meV 5C, T=4K)    
call IXFopen_raw('mar10443.raw', raw_file, status)
!call IXFopen_raw('map06215.raw', raw_file, status)

! Open and read 2 white beam V runs into w2dv1, w2dv2 
call IXFopen_raw('mar10497.raw', raw_file2, status)
call IXFopen_raw('mar10432.raw', raw_file3, status)
!call IXFopen_raw('map06246.raw', raw_file2, status)
!call IXFopen_raw('map06247.raw', raw_file3, status)

! Open and read a mono. V run into w2dmv1 (=> Ei=17meV 5C)
call IXFopen_raw('mar10467.raw', raw_file4, status)
!call IXFopen_raw('map06220.raw', raw_file4, status)

! Calls to routines to extract run and instrument parameters from raw_file
call IXFget_raw(raw_file, 'NTC1', ntimechan, status)
call IXFget_raw(raw_file, 'NDET', ndet, status)
call IXFget_raw(raw_file, 'NMON', nmon, status)
call IXFget_raw(raw_file, 'NSP1', nspec, status)
call IXFget_raw(raw_file, 'RVPB', rvpb, status)
L1 = rvpb(23)
allocate(L2(ndet))
call IXFget_raw(raw_file, 'LEN2', L2, status)
allocate(TTHE(ndet))
call IXFget_raw(raw_file, 'TTHE', TTHE, status)
allocate(DELT(ndet))
call IXFget_raw(raw_file, 'DELT', DELT, status)

! Repeat for V1 raw file
call IXFget_raw(raw_file2, 'NTC1', ntimechan2, status)
call IXFget_raw(raw_file2, 'NSP1', nspec2, status)

! Repeat for V2 file
call IXFget_raw(raw_file3, 'NTC1', ntimechan3, status)
call IXFget_raw(raw_file3, 'NSP1', nspec3, status)

! Repeat for the mono V file
call IXFget_raw(raw_file4, 'NTC1', ntimechan4, status)
call IXFget_raw(raw_file4, 'NSP1', nspec4, status)


! Select the spectra to be included in w2d
! Keep ALL spectra, including monitors for now
! NB Matlab struggles plotting the full nspec using ixf_multiplot2d

!nspec=20

allocate(spec_nums(nspec)) !(nspec-4))
spec_nums=(/(i,i=1,nspec)/)

! Repeat for the other data sets
!allocate(spec_nums2(nspec2))
!spec_nums2=(/(i,i=1,nspec2)/)
!allocate(spec_nums3(nspec3))
!spec_nums3=(/(i,i=1,nspec3)/)
!allocate(spec_nums4(nspec4))
!spec_nums4=(/(i,i=1,nspec4)/)

! Write the chosen spectra from raw_file to w2d, w2dv1, w2dv2 and w2dmv1
call IXFget_raw(raw_file, spec_nums, w2d, status)
call IXFget_raw(raw_file2, spec_nums, w2dv1, status)
call IXFget_raw(raw_file3, spec_nums, w2dv2, status)
call IXFget_raw(raw_file4, spec_nums, w2dmv1, status)

! Redefine time channels -> time zero is used as a dump for spurious signals
allocate(time_chans(ntimechan+1))
call IXFget_raw(raw_file, 'TIM1', time_chans, status)
allocate(time_chans2((ntimechan2+1)))
call IXFget_raw(raw_file2, 'TIM1', time_chans2, status)
allocate(time_chans3((ntimechan3)+1))
call IXFget_raw(raw_file3, 'TIM1', time_chans3, status)
allocate(time_chans4((ntimechan4)+1))
call IXFget_raw(raw_file4, 'TIM1', time_chans4, status)

! Assign the counts in each 2d array with the number of time
! channels and spectra
allocate(counts(ntimechan+1, nspec))
allocate(counts2((ntimechan2)+1, nspec))
allocate(counts3((ntimechan3)+1, nspec))
allocate(counts4((ntimechan4)+1, nspec))

! Finally write the counts in to w2d, w2dv1, w2dv2, w2dmv1
call IXFget_raw(raw_file, spec_nums, counts, status)
call IXFget_raw(raw_file2, spec_nums, counts2, status)
call IXFget_raw(raw_file3, spec_nums, counts3, status)
call IXFget_raw(raw_file4, spec_nums, counts4, status)

! Now carry out some operations on the dataset_2d's
x_label = 'tof'
x_units = 'usec'
xhist=.true.
xdist=.true.
y_label = 'spectrum'
y_units = 'spec no'

! RAW data
title = 'w2d'
call IXFset_dataset_2d(w2d,status,title=title,x_label=x_label,x_units=x_units, &
y_label=y_label,y_units=y_units)
! wb V1
title = 'w2dv1'
call IXFset_dataset_2d(w2dv1,status,title=title,x_label=x_label,x_units=x_units, &
y_label=y_label,y_units=y_units)
! wb V2
title = 'w2dv2'
call IXFset_dataset_2d(w2dv2,status,title=title,x_label=x_label,x_units=x_units, &
y_label=y_label,y_units=y_units)
! mono V
title = 'w2dmv1'
call IXFset_dataset_2d(w2dmv1,status,title=title,x_label=x_label,x_units=x_units, &
y_label=y_label,y_units=y_units)

! Ratio of wb V1 and V2
title = 'Vratio'
call IXFdivide_dataset_2d(w2d3,w2dv1,w2dv2,status)
call IXFset_dataset_2d(w2d3,status,title=title,x_label=x_label,x_units=x_units, &
y_label=y_label,y_units=y_units)

! Ratio of RAW data, w2d, to mono V, w2dmv1
title = 'Vnorm'
call IXFdivide_dataset_2d(w2dvn1,w2d,w2dmv1,status)
call IXFset_dataset_2d(w2dvn1,status,title=title,x_label=x_label,x_units=x_units, &
y_label=y_label,y_units=y_units)


! Integrals of dataset_2d's w.r.t. TOF between xmin and xmax 
x_label = 'spectrum'
x_units = 'spectrum no.'
s_label = 'int int'
s_units = 'counts'

! Define the upper and lower bounds of the integral w.r.t. TOF
xmin=5
xmax=20000

! RAW data, w2d, integral wrt TOF between xmin and xmax
title = 'w1d'
call IXFintegrate_x_dataset_2d(w1d,w2d,xmin,xmax,status)
call IXFset_dataset_1d(w1d,status,title=title,x_label=x_label,x_units=x_units)

! mono V, w2dmv1, integral wrt TOF between xmin and xmax
title = 'w1dmv1'
call IXFintegrate_x_dataset_2d(w1dmv1,w2dmv1,xmin,xmax,status)
call IXFset_dataset_1d(w1dmv1,status,title=title,x_label=x_label,x_units=x_units)


! Send w2d's and w1d's to Matlab
!call IXBsendToBinding(plhs(1),prhs(1),' ', 1, 0, w2d,status)
!call IXBsendToBinding(plhs(2),prhs(2),' ', 1, 0, w2dv1,status)
!call IXBsendToBinding(plhs(3),prhs(3),' ', 1, 0, w2dv2,status)
!call IXBsendToBinding(plhs(4),prhs(4),' ', 1, 0, w2dmv1,status)
!call IXBsendToBinding(plhs(5),prhs(5),' ', 1, 0, w2d3,status)
!call IXBsendToBinding(plhs(6),prhs(6),' ', 1, 0, w2dvn1,status)
!call IXBsendToBinding(plhs(7),prhs(7),' ', 1, 0, w1d,status)
!call IXBsendToBinding(plhs(10),prhs(10),' ', 1, 0, w1dmv1,status)


! Calculate the average electronics delay, t_delay(usec) and secondary flight path, L2_mean
! WARNING: only valid for MARI at the moment, where monitors are excluded
delt_sum = 0.0
L2_sum = 0.0
do i=5,nspec
    delt_sum = delt_sum + DELT(i)
    L2_sum = L2_sum + L2(i)
end do

t_delay = delt_sum/(nspec-nmon)
L2_mean = L2_sum/(nspec-nmon)


! Write some parameters to the Matlab command window
write(message1, *) 'No. of spectra:', nspec, ' No. of monitors:', nmon, ' Number of detectors: ',ndet
call IXFwrite_line(message1,status)
write(message19, *) 'L1(m) :', L1, ' Mean L2(m): ', L2_mean, 'Mean t_delay(usec): ', t_delay
call IXFwrite_line(message19,status)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Remove spectra specified in a hard mask file

! Extract the signal(s), error(e) and spectrum no.(x) from w1d
call IXFget_alloc_dataset_1d(w1d,x,s,e) 

nspec_hard = size(hard)

! Define the hard mask
allocate(mask_hard(nspec))
mask_hard = .true.

! Identify those spectra specified in the hard mask input array
do i = 1,nspec_hard
where(mask_hard .eqv. .true. .and. (x == hard(i)))
mask_hard = .false.
end where
if (hard(i) .le. 0) then
    write(message9, *) 'At least one spectrum specified in the hard mask does not exist: ', hard(i)
mask_hard = .true.
endif
end do

ngood_hard = count(mask_hard)
allocate(good_det_hard(ngood_hard))
good_det_hard = pack(spec_nums,mask_hard)

allocate(not_mask_hard(size(mask_hard)))
not_mask_hard = .not.(mask_hard)
nbad_hard = count(not_mask_hard)
allocate(bad_det_hard(nbad_hard))
bad_det_hard = pack(spec_nums,not_mask_hard)

if (nspec_hard == 0) then
    write(message8, *) 'No spectra have been included in a hard mask'
else
    write(message8, *) 'Number and spectra in the hard mask: ', nbad_hard, bad_det_hard 
endif
call IXFwrite_line(message9,status)
call IXFwrite_line(message8,status)

deallocate(s,e,x)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Determining unstable detectors using ratio of integrals V1:V2

! Calculate the TOF integral limits
c = 2286.26
e_i = 17.0
e_trans_min = -10.0
e_trans_max = 16.0
e_min = e_i - e_trans_max
e_max = e_i - e_trans_min

t_min = c*(L1+L2_mean)/sqrt(e_max) + t_delay
t_max = c*(L1+L2_mean)/sqrt(e_min) + t_delay
! Ensure there's no frame overlap
if (t_max > time_chans(ntimechan+1)) then
    t_max = time_chans(ntimechan+1)
endif

! wb V1 integral wrt TOF between t_min and t_max
x_label = 'spectrum'
x_units = 'spectrum no'
s_label = 'int int'
s_units = 'counts'
title = 'w1dv1'
call IXFintegrate_x_dataset_2d(w1dv1,w2dv1,t_min,t_max,status)
call IXFset_dataset_1d(w1dv1,status,title=title,x_label=x_label,x_units=x_units)

! wb V2 integral wrt TOF between t_min and t_max
title = 'w1dv2'
call IXFintegrate_x_dataset_2d(w1dv2,w2dv2,t_min,t_max,status)
call IXFset_dataset_1d(w1dv2,status,title=title,x_label=x_label,x_units=x_units)

! Allocate (xv1,sv1,ev1) from the integrals over all TOF
call IXFget_alloc_dataset_1d(w1dv1,xv1,sv1,ev1)
! Allocate (xv2,sv2,ev2) from the integrals over all TOF
call IXFget_alloc_dataset_1d(w1dv2,xv2,sv2,ev2)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compare the V1 and V2 spectral integrals w.r.t. median values to remove spectra
! Use thDe arrays w1dv1, w1dv2 from above for the integrals over all TOF

! Define the low and high factor limits -> MARI defaults 
vlow = 0.1_dp
vhigh = 3.0_dp
! For now define v_sig as factor
factor = 16.0_dp
v_sig = factor

! Start off with V1 and repeat for V2 if present


allocate(mask_v1lo(nspec))
mask_v1lo = .true.
allocate(mask_v1hi(nspec))
mask_v1hi = .true.
allocate(mask_v1zi(nspec))
mask_v1zi = .true.

if (modulo(nspec,2) == 1) then
    median_v1 = sv1(4+(nspec/2))
else
    median_v1 = 0.5*(sv1(4+(nspec/2)) + sv1(5+(nspec/2)))
endif

where(mask_v1lo .eqv. .true. .and. (sv1 < (vlow*median_v1)) .and. (abs(sv1-median_v1) > v_sig*ev1))
mask_v1lo = .false.
end where
where(mask_v1hi .eqv. .true. .and. (sv1 > (vhigh*median_v1)))
mask_v1hi = .false.
end where
where(mask_v1zi .eqv. .true. .and. (sv1 == 0.0))
mask_v1zi = .false.
end where

ngood_v1lo = count(mask_v1lo)
allocate(good_det_v1lo(ngood_v1lo))
good_det_v1lo = pack(spec_nums,mask_v1lo)

ngood_v1hi = count(mask_v1hi)
allocate(good_det_v1hi(ngood_v1hi))
good_det_v1hi = pack(spec_nums,mask_v1hi)

ngood_v1zi = count(mask_v1zi)
allocate(good_det_v1zi(ngood_v1zi))
good_det_v1zi = pack(spec_nums,mask_v1zi)

allocate(not_mask_v1lo(size(mask_v1lo)))
not_mask_v1lo = .not.(mask_v1lo)
nbad_v1lo = count(not_mask_v1lo)
allocate(bad_det_v1lo(nbad_v1lo))
bad_det_v1lo = pack(spec_nums,not_mask_v1lo)

allocate(not_mask_v1hi(size(mask_v1hi)))
not_mask_v1hi = .not.(mask_v1hi)
nbad_v1hi = count(not_mask_v1hi)
allocate(bad_det_v1hi(nbad_v1hi))
bad_det_v1hi = pack(spec_nums,not_mask_v1hi)

allocate(not_mask_v1zi(size(mask_v1zi)))
not_mask_v1zi = .not.(mask_v1zi)
nbad_v1zi = count(not_mask_v1zi)
allocate(bad_det_v1zi(nbad_v1zi))
bad_det_v1zi = pack(spec_nums,not_mask_v1zi)

write(message21, *) 'Median V1 integral: ', median_v1
call IXFwrite_line(message21,status)
write(message11, *) 'No. and spectra with low V1 integral: ', nbad_v1lo, bad_det_v1lo
call IXFwrite_line(message11,status)
write(message16, *) 'No. and spectra with high V1 integral: ', nbad_v1hi, bad_det_v1hi
call IXFwrite_line(message16,status)
write(message17, *) 'No. and spectra with zero V1 integral: ', nbad_v1zi, bad_det_v1zi
call IXFwrite_line(message17,status)



! Repeat for V2 if present
allocate(mask_v2lo(nspec))
mask_v2lo = .true.
allocate(mask_v2hi(nspec))
mask_v2hi = .true.
allocate(mask_v2zi(nspec))
mask_v2zi = .true.

if (modulo(nspec,2) == 1) then
    median_v2 = sv2(4+(nspec/2))
else
    median_v2 = 0.5*(sv2(4+(nspec/2)) + sv2(5+(nspec/2)))
endif

where(mask_v2lo .eqv. .true. .and. (sv2 < (vlow*median_v2)) .and. (abs(sv2-median_v2) > v_sig*ev2))
mask_v2lo = .false.
end where
where(mask_v2hi .eqv. .true. .and. (sv2 > (vhigh*median_v2)))
mask_v2hi = .false.
end where
where(mask_v2zi .eqv. .true. .and. (sv2 == 0.0))
mask_v2zi = .false.
end where

ngood_v2lo = count(mask_v2lo)
allocate(good_det_v2lo(ngood_v2lo))
good_det_v2lo = pack(spec_nums,mask_v2lo)

ngood_v2hi = count(mask_v2hi)
allocate(good_det_v2hi(ngood_v2hi))
good_det_v2hi = pack(spec_nums,mask_v2hi)

ngood_v2zi = count(mask_v2zi)
allocate(good_det_v2zi(ngood_v2zi))
good_det_v2zi = pack(spec_nums,mask_v2zi)

allocate(not_mask_v2lo(size(mask_v2lo)))
not_mask_v2lo = .not.(mask_v2lo)
nbad_v2lo = count(not_mask_v2lo)
allocate(bad_det_v2lo(nbad_v2lo))
bad_det_v2lo = pack(spec_nums,not_mask_v2lo)

allocate(not_mask_v2hi(size(mask_v2hi)))
not_mask_v2hi = .not.(mask_v2hi)
nbad_v2hi = count(not_mask_v2hi)
allocate(bad_det_v2hi(nbad_v2hi))
bad_det_v2hi = pack(spec_nums,not_mask_v2hi)

allocate(not_mask_v2zi(size(mask_v2zi)))
not_mask_v2zi = .not.(mask_v2zi)
nbad_v2zi = count(not_mask_v2zi)
allocate(bad_det_v2zi(nbad_v2zi))
bad_det_v2zi = pack(spec_nums,not_mask_v2zi)

write(message22, *) 'Median V2 integral: ', median_v2
call IXFwrite_line(message22,status)
write(message12, *) 'No. and spectra with low V2 integral: ', nbad_v2lo, bad_det_v2lo
call IXFwrite_line(message12,status)
write(message14, *) 'No. and spectra with high V2 integral: ', nbad_v2hi, bad_det_v2hi
call IXFwrite_line(message14,status)
write(message15, *) 'No. and spectra with zero V2 integral: ', nbad_v2zi, bad_det_v2zi
call IXFwrite_line(message15,status)




! Calculate the ratio of wb V1:wb V2 integrals
s_label = 'ratio'
s_units = 'normalised'
title = 'wbV1V2ratio'
call IXFdivide_dataset_1d(w1dVrat,w1dv1,w1dv2,status)
call IXFset_dataset_1d(w1dVrat,status,title=title,x_label=x_label,x_units=x_units)

! Extract the signal(s), error(e) and spec.(x)
call IXFget_alloc_dataset_1d(w1dVrat,x,s,e) 

! Define some required input parameters ensuring symmetric treatment of V1,V2
stability = 10.0_dp
! Use s_sig = factor for now......
s_sig = factor
ratio_hl = 1.0_dp + 0.01_dp*stability
allocate(ratio_hl_err(nspec),ratio_ll_err(nspec))
ratio_hl_err = 1.0_dp + s_sig*sqrt((ev1/sv1)**2 +(ev2/sv2)**2)
ratio_ll = 1.0_dp/ratio_hl
ratio_ll_err = 1.0_dp/ratio_hl_err

! Old DIAG high/low ratio limits
!ratio_ll = 1/(1+z)
!ratio_hl = 1+z



! Warning, this assumes s1->s4 are monitors....not always true.
! Need to discriminate between monitors and detectors more intelligently
if (modulo((nspec-nmon),2) == 1) then
    ratio_median = s(4+((nspec-nmon)/2))
else
    ratio_median = sqrt(s(4+((nspec-nmon)/2))*s(5+((nspec-nmon)/2)))
endif

allocate(mask_stab(nspec))
mask_stab = .true.

! Old DIAG criterion
!where(mask_stab .eqv. .true. .and. ((s > (ratio_median*ratio_hl)) .or.  (s < ratio_median*ratio_ll)))
!mask_stab = .false.
!end where

! New DIAG criterion
where(mask_stab .eqv. .true. .and. & 
       ((s/ratio_median) < max(ratio_ll,ratio_ll_err) .or. (s/ratio_median) > min(ratio_hl,ratio_hl_err)))
mask_stab = .false.
end where

ngood_stab = count(mask_stab)
allocate(good_det_stab(ngood_stab))
good_det_stab = pack(spec_nums,mask_stab)

allocate(not_mask_stab(size(mask_stab)))
not_mask_stab = .not.(mask_stab)
nbad_stab = count(not_mask_stab)
allocate(bad_det_stab(nbad_stab))
bad_det_stab = pack(spec_nums,not_mask_stab)

! Output the V1,V2 integrals and their ratio to Matlab
!call IXBsendToBinding(plhs(8),prhs(8),' ', 1, 0, w1dv1,status)
!call IXBsendToBinding(plhs(9),prhs(9),' ', 1, 0, w1dv2,status)
!call IXBsendToBinding(plhs(11),prhs(11),' ', 1, 0, w1dVrat,status)

! Calculate the mean V2:V1 integral ratio
allocate(ratio_sum(nspec))
do i=1,nspec
    ratio_sum(i) = 0.0_dp
end do

    where (mask_stab .eqv. .true.)
    ratio_sum = s
    end where
    !where (mask_stab .eqv. .false.)
    !ratio_sum = 0.0_dp
    !end where
!end do

ratio_mean = 0.0_dp
do i=1,nspec
    ratio_mean = ratio_mean + ratio_sum(i)
    end do 
!ratio_mean = sum(ratio_sum)!/(ngood_stab)


write(message20, *) 'Lower and upper time channel boundaries for white beam integrals(usec): ', t_min, t_max
call IXFwrite_line(message20,status)
write(message4, *) 'median and mean V2:V1 ratios:', ratio_median, ratio_mean
call IXFwrite_line(message4,status)
write(message2, *) 'No. of stable spectra: ', ngood_stab, ' No. of unstable spectra: ', nbad_stab 
call IXFwrite_line(message2,status)
write(message3, *) 'The first 10 bad detectors from the V1:V2 ratio are:', bad_det_stab(2:nbad_stab)
call IXFwrite_line(message3,status)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Determining those spectra with high background and zero total counts

! Input parameters, defining the maximum deviation of a spectrums background
! away from the median background integral evaluated over the specified time 
! channel boundaries bmin -> bmax
!factor = 5.0_dp
bmin = 16000
bmax = 19500

! Calculate the background integral in each spectrum
title = 'w1d_back'
x_label = 'spectrum'
x_units = 'spectrum no'
s_label = 'int int'
s_units = 'counts'
call IXFintegrate_x_dataset_2d(w1d_back,w2d,bmin,bmax,status)
call IXFset_dataset_1d(w1d_back,status,title=title,x_label=x_label,x_units=x_units)

! Calculate the background integral from the V1 run
title = 'w1dv1_back'
call IXFintegrate_x_dataset_2d(w1dv1_back,w2dv1,bmin,bmax,status)
call IXFset_dataset_1d(w1dv1_back,status,title=title,x_label=x_label,x_units=x_units)

! Normalise the background integral w.r.t. the V1 run
title = 'w1d_back_norm'
s_label = 'ratio'
s_units = 'normalised'
call IXFdivide_dataset_1d(w1d_back_norm,w1d_back,w1dv1_back,status)
call IXFset_dataset_1d(w1d_back_norm,status,title=title,x_label=x_label,x_units=x_units)

! Allocate (x,s,e)_fullint from the integrals over all TOF
call IXFget_alloc_dataset_1d(w1d,x_fullint,s_fullint,e_fullint)
! Allocate (x,s,e)_back_norm from the normalised background integrals
call IXFget_alloc_dataset_1d(w1d_back_norm,x_back,s_back,e_back)

! Construct masks for high backgrounds and zero total counts
allocate(mask_back(nspec))
mask_back = .true.
allocate(mask_zero(nspec))
mask_zero = .true.

if (modulo(nspec,2) == 1) then
    median_back = s_back(nspec/2)
else
    median_back = 0.5*(s_back(nspec/2) + s_back((nspec/2)+1))
endif

where(mask_back .eqv. .true. .and. ( ((s_back - median_back) > 3.3*e_back) & 
         .and. (s_back > (factor*median_back))  .or. (s_fullint == 0.0_dp)) )
mask_back = .false.
end where

where(mask_zero .eqv. .true. .and. (s_fullint == 0.0_dp))
mask_zero = .false.
end where

! good_det_back defines good spectra via background inspection
ngood_back = count(mask_back)
allocate(good_det_back(ngood_back))
good_det_back = pack(spec_nums,mask_back)

! bad_det_back defines bad spectra via background inspection
allocate(not_mask_back(size(mask_back)))
not_mask_back = .not.(mask_back)
nbad_back = count(not_mask_back)
allocate(bad_det_back(nbad_back))
bad_det_back = pack(spec_nums,not_mask_back)    

! good_det_zero defines good spectra via integral over all TOF
ngood_zero = count(mask_zero)
allocate(good_det_zero(ngood_zero))
good_det_zero = pack(spec_nums,mask_zero)

! bad_det_zero defines bad spectra via zero intergral over all TOF
allocate(not_mask_zero(size(mask_zero)))
not_mask_zero = .not.(mask_zero)
nbad_zero = count(not_mask_zero)
allocate(bad_det_zero(nbad_zero))
bad_det_zero = pack(spec_nums,not_mask_zero)

write(message5, *) 'No. of spectra with abnormal bkgds: ', nbad_back, ' Median bckgd value: ', median_back 
call IXFwrite_line(message5,status)

write(message6, *) 'Spectra with abnormal bkgds: ', bad_det_back(1:(nbad_back/2)) 
call IXFwrite_line(message6,status)
write(message18, *) 'Spectra with abnormal bkgds: ', bad_det_back(((nbad_back/2)+1):nbad_back) 
call IXFwrite_line(message18,status)
write(message7, *) 'No. and spectra with zero total counts: ', nbad_zero, bad_det_zero 
call IXFwrite_line(message7,status)


! Output the normalised backgrounds from the raw data and V1    
!call IXBsendToBinding(plhs(12),prhs(12),' ', 1, 0, w1d_back_norm,status)
!call IXBsendToBinding(plhs(13),prhs(13),' ', 1, 0, w1dv1_back,status)

    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Combine the individual masks to generate a final mask array
allocate(mask_total(nspec))
mask_total = .false.

where(mask_stab .eqv. .true. .and. mask_back .eqv. .true. .and. mask_zero .eqv. .true. .and. mask_hard .eqv. .true. &
.and. mask_v1lo .eqv. .true. .and. mask_v1hi .eqv. .true. .and. mask_v1zi .eqv. .true. .and. mask_v2lo .eqv. .true. &
.and. mask_v2hi .eqv. .true. .and. mask_v2zi .eqv. .true.)
mask_total = .true.
end where

! good_det defines total good spectra
ngood = count(mask_total)
allocate(good_det(ngood))
good_det = pack(spec_nums,mask_total)

! bad_det defines total bad spectra
allocate(not_mask_total(size(mask_total)))
not_mask_total = .not.(mask_total)
nbad = count(not_mask_total)
allocate(bad_det(nbad))
bad_det = pack(spec_nums,not_mask_total)  

write(message10, *) 'Total no. of good spectra: ', ngood, ' Total no. of bad spectra: ', nbad   
call IXFwrite_line(message10,status)   
write(message13, *) 'Masked spectra: ', bad_det(1:(nbad/2))
call IXFwrite_line(message13,status)
write(message14, *) 'Masked spectra: ', bad_det(((nbad/2)+1):(nbad))
call IXFwrite_line(message14,status)


! Try and eliminate any confusion and memory leakage    
deallocate(spec_nums,time_chans,time_chans2,time_chans3,time_chans4,&
            counts,counts2,counts3,counts4,s_back,e_back,x_back)

end subroutine IXFdiag
end module IXMdiag

