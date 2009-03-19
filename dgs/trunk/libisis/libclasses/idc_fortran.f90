module IDFroutines
    implicit none
    public :: IDFgetpari, IDFgetparr, IDFgetparc, IDFopen, IDFclose,IDFgetdat
    private :: getpari, getpari1, getparr, getparr1, getparc, getparc1, getparr2, getpari2,getdat1d, getdat2d
    interface IDFgetpari
        module procedure getpari, getpari1, getpari2
    end interface IDFgetpari
    interface IDFgetparr
        module procedure getparr, getparr1, getparr2
    end interface IDFgetparr
    interface IDFgetparc
        module procedure getparc, getparc1
    end interface IDFgetparc
    interface IDFgetdat
        module procedure getdat1d, getdat2d
    end interface IDFgetdat
    
contains
	subroutine IDFopen(file, mode, opts, fh, errcode)
	implicit none
	integer fh(2), mode, opts, errcode
	character*(*) file
	call IDCFopen(trim(file), mode, opts, fh, errcode)
	end subroutine

	subroutine IDFclose(fh, errcode)
	implicit none
	integer fh(2), errcode
	call IDCFclose(fh, errcode)
	end subroutine

	subroutine getpari(fh, name, value, errcode)
	implicit none
	integer stat, fh(2), dims_array(1), ndims, errcode
	integer value
	character*(*) name
	dims_array(1) = 1
	ndims = 1
	call IDCFgetpari(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine getpari1(fh, name, value, dims_array, ndims, errcode)
	implicit none
	integer stat, fh(2), dims_array(*), ndims, errcode
	integer value(:)
	character*(*) name
	call IDCFgetpari(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine getpari2(fh, name, value, dims_array, ndims, errcode)
	implicit none
	integer stat, fh(2), dims_array(*), ndims, errcode
	integer value(:,:)
	character*(*) name
	call IDCFgetpari(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine getparr(fh, name, value, errcode)
	implicit none
	integer stat, fh(2), dims_array(1), ndims, errcode
	real value
	character*(*) name
        dims_array(1) = 1
        ndims = 1
	call IDCFgetparr(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine getparr1(fh, name, value, dims_array, ndims, errcode)
	implicit none
	integer stat, fh(2), dims_array(*), ndims, errcode
	real value(:)
	character*(*) name
	call IDCFgetparr(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine getparr2(fh, name, value, dims_array, ndims, errcode)
	implicit none
	integer stat, fh(2), dims_array(*), ndims, errcode
	real value(:,:)
	character*(*) name
	call IDCFgetparr(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine IDFgetpard(fh, name, value, dims_array, ndims, errcode)
	implicit none
	integer stat, fh(2), dims_array(*), ndims, errcode
	double precision value(:)
	character*(*) name
	call IDCFgetpard(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine

	subroutine getparc(fh, name, value, errcode)
	implicit none
	integer :: i, stat, fh(2), dims_array(1), ndims, errcode
	character(len=*) :: value
	character(len=*) :: name
!        integer*1 , allocatable :: byte_array(:)
!	value = ' '
        dims_array(1) = len(value)
        ndims = 1
!        allocate(byte_array(dims_array(1)))
!	call IDCFgetparc(fh, trim(name), byte_array, dims_array, ndims, errcode)
	call IDCFgetparc(fh, trim(name), value, dims_array, ndims, errcode)
!        do i=1,dims_array(1)
!             value(i:i) = char(byte_array(i))
!        enddo
!        deallocate(byte_array)
	end subroutine

	subroutine getparc1(fh, name, value, dims_array, ndims, errcode)
	implicit none
	integer :: stat, fh(2), dims_array(*), ndims, errcode
	character(len=*) :: value(*)
	character(len=*) :: name
	call IDCFgetparc(fh, trim(name), value, dims_array, ndims, errcode)
	end subroutine


! for some reason the following two subroutines don't get recognised as being different
! inthe module procedure....
	subroutine getdat1d(fh,ifsn,nos,value,dims_array,ndims,errcode)
	implicit none
	integer stat, fh(2), ifsn, nos, errcode
	integer value(:), dims_array(*), ndims
	call IDCFgetdat(fh, ifsn, nos, value, dims_array, ndims, errcode)
	end subroutine 

	subroutine getdat2d(fh,ifsn,nos,value,dims_array,ndims,errcode)
	implicit none
	integer stat, fh(2), ifsn, nos, errcode
	integer value(:,:), dims_array(*), ndims
	call IDCFgetdat(fh, ifsn, nos, value, dims_array, ndims, errcode)
	end subroutine 
end module IDFroutines
