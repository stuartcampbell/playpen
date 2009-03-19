!------------------------------
! MODULE: IXMspectra
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1417 $ ($Date: 2008-07-04 13:04:09 -0400 (Fri, 04 Jul 2008) $)
!!
!! FORTRAN definition of IXMspectra class
! spectra module
module IXMspectra
  use IXMtype_definitions
  use IXMbase
  implicit none	
  public :: IXTspectra
  type IXTspectra
     private
     type(IXTbase):: base
     !! lookup to spectra in the spec_no array
     !! ie: spec_lookup(spectrum_no)=index of spectrum_no in spec_no(:) array
     integer(i4b),pointer:: spec_lookup(:) 
     !! Spectrum numbers for the workspaces are contained in spec_no(1:nspec_tot).
     !! The spectrum numbers corresponding to workspace work_no(i) are
     !! spec_no(spec_ind(i)) ... spec_no(spec_ind(i) + nspec(i) - 1).
     integer(i4b), pointer :: spec_no(:)=>NULL()
     !! Number of detectors contributing to each spectrum
     !! Array runs over ndet(1:nspec_tot).
     integer(i4b), pointer :: ndet(:)=>NULL()
     !! det_index(i) contains the index to the first element in the det_no (a detector number) array 
     !! contributing to the spec_no (i)
     !! The array runs over det_ind(1:nspec_tot). If there are no detectors contributing to the spectrum
     !! then this value is 0
     integer(i4b),pointer:: det_index(:)=>NULL()
     !!detector no's array. det_no(1:[total # of detectors])
     integer(i4b),pointer :: det_no(:)=>NULL() 
  end type IXTspectra

#define IXD_TYPE spectra
#include "class_header.f90"

  private:: populate_list_dso_isis
  
  interface IXFpopulate_spectra
     module procedure  populate_list_dso_isis
  end interface


contains

#define IXD_DESCRIPTION	"IXTspectra class"
#define IXD_TYPE spectra
#define IXD_SQTYPE 'spectra'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_spectra(op, field, arg, status)
    implicit none
    type(IXTspectra) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTspectra', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'spec_lookup', arg%spec_lookup, status)
    call IXFoperation_run_ptr(op,'spec_no', arg%spec_no, status)
    call IXFoperation_run_ptr(op,'ndet',arg%ndet,status)
    call IXFoperation_run_ptr(op,'det_index', arg%det_index, status)
    call IXFoperation_run_ptr(op,'det_no', arg%det_no, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_spectra(spectra,spec_no,spec_lookup,ndet,det_index,det_no,status)
    implicit none
    type(IXTspectra) :: spectra
    type(IXTstatus) :: status
    integer(i4b),intent(in)::spec_no(:),det_index(:),ndet(:),det_no(:),spec_lookup(:)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(spectra)
    call IXFset_spectra(spectra,status,spec_lookup,spec_no,ndet,det_index,det_no)

  end subroutine IXFcreate_spectra

  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_spectra(arg, status)
    implicit none
    type(IXTspectra) :: arg
    type(IXTstatus) :: status

    call IXFcheck_base(arg%base,status)

    if(size(arg%spec_lookup) /= maxval(arg%spec_no))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'spec_lookup table invalid (IXFcheck_spectra)')
    endif
    ! a quick check to see if correspondance b/w spec_lookup and spec_no

    if(.not.(size(arg%spec_no).eq.size(arg%ndet)).and.(size(arg%ndet).eq.size(arg%det_index)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'spec_no, ndet and det_index array lengths incompatible  (IXFcheck_spectra)')
    endif

    if(.not. (sum(arg%ndet) .eq. size(arg%det_no)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'spec_no, ndet and det_no array lengths incompatible  (IXFcheck_spectra)')
    endif

  end subroutine IXFcheck_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_spectra(spectra,status)
    implicit none
    type(IXTspectra) :: spectra
    type(IXTstatus) :: status

    call IXFdestroy(spectra%base,status)
    call IXFdealloc(spectra%spec_no,status)
    call IXFdealloc(spectra%spec_lookup,status)    
    call IXFdealloc(spectra%ndet,status)
    call IXFdealloc(spectra%det_index,status)
    call IXFdealloc(spectra%det_no,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(spectra)

  end subroutine IXFdestroy_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_spectra(spectra,status,spec_lookup,spec_no,ndet,det_index,det_no,ref)
    implicit none
    type(IXTspectra),intent(inout) :: spectra
    type(IXTspectra),intent(in),optional:: ref
    integer(i4b),optional,intent(in)::spec_no(:)
    integer(i4b),optional,intent(in)::spec_lookup(:)
    integer(i4b),optional,intent(in)::ndet(:)
    integer(i4b),optional,intent(in)::det_index(:)
    integer(i4b),optional,intent(in)::det_no(:)
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_spectra)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(spectra)
    else    
       if(IXFvalid(spectra) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_spectra)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_spectra(spectra,status,ref%spec_lookup,ref%spec_no,ref%ndet,ref%det_index,ref%det_no)

    call IXFset_integer_array(spectra%spec_lookup,status,spec_lookup) 
    call IXFset_integer_array(spectra%spec_no,status,spec_no) 
    call IXFset_integer_array(spectra%ndet,status,ndet) 
    call IXFset_integer_array(spectra%det_index,status,det_index)
    call IXFset_integer_array(spectra%det_no,status,det_no)
    call IXFcheck_spectra(spectra,status)

  end subroutine IXFset_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_spectra(spectra, status,spec_lookup,spec_no,ndet,det_index,det_no,wout)
    implicit none
    type(IXTspectra),intent(in) :: spectra
    type(IXTspectra),optional,intent(out)::wout
    type(IXTstatus) :: status
    integer(i4b),optional,intent(out)::spec_lookup(:)
    integer(i4b),optional,intent(out)::spec_no(:)
    integer(i4b),optional,intent(out)::ndet(:)
    integer(i4b),optional,intent(out)::det_index(:)
    integer(i4b),optional,intent(out)::det_no(:)

    if (present(wout))call IXFcopy(spectra,wout,status)
    
    call IXFget_integer_array(spectra%spec_lookup,status,spec_lookup)
    call IXFget_integer_array(spectra%spec_no,status,spec_no)
    call IXFget_integer_array(spectra%ndet,status,ndet)
    call IXFget_integer_array(spectra%det_index,status,det_index)
    call IXFget_integer_array(spectra%det_no,status,det_no)

  end subroutine IXFget_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_spectra(spectra,spec_lookup,spec_no,ndet,det_index,det_no)
    implicit none
    type(IXTspectra) :: spectra
    integer(i4b),optional,pointer::spec_no(:),ndet(:),det_index(:),det_no(:),spec_lookup(:)

    if (present(spec_lookup))spec_lookup=>spectra%spec_lookup
    if (present(spec_no))spec_no=>spectra%spec_no
    if (present(ndet))ndet=>spectra%ndet
    if (present(det_index))det_index=>spectra%det_index
    if (present(det_no))det_no=>spectra%det_no

  end subroutine IXFget_ptr_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_spectra(spectra,status,spec_lookup, spec_no,ndet,det_index,det_no,wout)
    implicit none
    type(IXTspectra),intent(in) :: spectra
    type(IXTspectra),intent(out) :: wout
    type(IXTstatus):: status
    integer(i4b),optional,allocatable::spec_lookup(:)
    integer(i4b),optional,allocatable::spec_no(:)
    integer(i4b),optional,allocatable::ndet(:)
    integer(i4b),optional,allocatable::det_index(:)
    integer(i4b),optional,allocatable::det_no(:)


    if (present(spec_lookup))then
       call IXFreallocdimsFortran(spec_lookup,shape(spectra%spec_lookup),.false.,status)
    endif
    
    if (present(spec_no))then
       call IXFreallocdimsFortran(spec_no,shape(spectra%spec_no),.false.,status)
    endif
    
    if (present(ndet))then
       call IXFreallocdimsFortran(ndet,shape(spectra%ndet),.false.,status)
    endif

    if (present(det_index))then
       call IXFreallocdimsFortran(det_index,shape(spectra%det_index),.false.,status)
    endif

    if (present(det_no))then
       call IXFreallocdimsFortran(det_no,shape(spectra%det_no),.false.,status)
    endif
    
    call IXFget_spectra(spectra,status,spec_lookup, spec_no,ndet,det_index,det_no,wout)

  end subroutine IXFget_alloc_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFgetdets_spectra will return an allocatable array of the list of detectors contributing to a 
  !! given a list of spectrum numbers
  subroutine IXFgetdets_spectra(spectra,specs_in,dets_out,status)
    implicit none
    type(IXTspectra) :: spectra
    integer(i4b),allocatable::dets_out(:),indices(:)
    integer(i4b),intent(in)::specs_in(:)
    type(IXTstatus)::status
    integer(i4b)::i,spa,spb,d1,d2,no_spec

    if(allocated(dets_out))deallocate(dets_out)
! specs_in is a list of spectrum numbers, we need to determine indices of spectra in object
! use spec_lookup    
    no_spec=size(specs_in)
    allocate(indices(no_spec))
    if(no_spec > size(spectra%spec_lookup))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Spectrum in mapping array is not defined in IXTspectra object (IXFgetdets_spectra),check your reference file')
       return
    endif    
    ! dereference spec_lookup array to give 
    indices=spectra%spec_lookup(specs_in)

    !dereference ndet array using indices
    allocate (dets_out(sum(spectra%ndet(indices))))

!in all aspects in this routine the indices(:) array is used to dereference internal arrays of IXTspectra

    d1=1
    d2=0

    do i=1,no_spec
       if( spectra%ndet(indices(i)) < 0)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Spectrum in mapping array is not used (IXFgetdets_spectra),check your file')
          !mapping array refers to a bad spectra, so RETURN
          return    
       endif

       ! d1 and d2 step thru' sections of det_out array according to how many have been filled so far
       ! which it is NOT
       d1=d2+1
       d2=d1+spectra%ndet(indices(i))-1
       
       spa=spectra%det_index(indices(i))
       spb=spectra%det_index(indices(i))+spectra%ndet(indices(i))-1
       !need to reference section of det_no array using det_ind for each spectrum 
       dets_out(d1:d2)= spectra%det_no(spa:spb)
    enddo
    deallocate(indices)

  end subroutine IXFgetdets_spectra

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which will populate a spectra object from an isis rawfile
  !! the length of spec_no MUST equal NSPEC from the raw file
  !! list out is the index for the detector which is extracted from the raw file arrays
  subroutine populate_list_dso_isis(spectra,inputsource,spec_list,list_out, status)
    use IXMraw_file
    implicit none
    type(IXTspectra) :: spectra
    type(IXTstatus) :: status
    type(IXTbase)::base
    integer(i4b),intent(in)::spec_list(:) !! list of spectrum numbers used by the map files (if they have been provided)
    integer(i4b),allocatable::list_out(:)
    type(IXTraw_file),intent(in) :: inputsource  
    integer(i4b)::ndets,nspec,i,start,last,nstart,nlast,tdet
    integer(i4b),allocatable::spec_no(:),det_index(:),ndet(:),det_no(:),spec(:),temp1(:)


    call IXFget_raw(inputsource, 'NDET', ndets, status)
    call IXFget_raw(inputsource, 'NSP1', nspec, status)
    allocate(spec(ndets))
    call IXFget_raw(inputsource, 'SPEC', spec, status)

    ! this routine uses allocatable arrays which are then copied in the set command 
    ! and then deallocated it could be improved by writing routine to set the length
    ! of the arrays and then getting the pointers and filling them implicitly

    allocate (ndet(nspec),spec_no(nspec),det_index(nspec),temp1(nspec))
    ! so instead 
    ! call IXFmake_length_spectra(spectra,nspec,ndet, ..) etc...

    ndet=0
    det_index=0

    do i=1,ndets
       if(spec(i) > 0) ndet(spec(i))=ndet(spec(i))+1
    enddo

    ! this is vitally necessary
    forall(i=1:nspec) spec_no(i)=i

    ! find first non-zero detector spectrum
    i=1
    do while(i <= nspec )
       if ( ndet(i) /= 0) then
          ! leave loop
          det_index(i)=1
          start=i+1
          i=nspec+1 
       else
          i=i+1
       endif
    enddo

    ! set first value and step through
    last=start-1
    do i=start,nspec
       if (ndet(i) /= 0)then
          det_index(i)=det_index(last)+ndet(last)
          last=i
       endif
    enddo

    ! length of det_no is the total number of detectors used
    allocate (det_no(count(spec /=0)))
    det_no=0

    !use temp1 as an incremnting counter to fill group of detector in list
    !when detector is used the put it in the det_no list
    temp1=0
    do i=1,ndets
       if(spec(i) > 0 )then
          if( ndet(spec(i)) >0  ) then
             det_no(det_index(spec(i))+temp1(spec(i)))= i
             temp1(spec(i))=temp1(spec(i))+1
          endif
       endif
    enddo

    !determine new nspec length
    nspec=size(spec_list)
    
    call IXFalloc(spectra%spec_lookup,maxval(spec_list),status)
    spectra%spec_lookup=0
    call IXFalloc(spectra%spec_no,nspec,status)
    call IXFalloc(spectra%ndet,nspec,status)
    call IXFalloc(spectra%det_index,nspec,status)
    spectra%det_index=0        
    ! spec_list is a list of spectra used in the map file, can be used to index 
    ! previously filled allocatable arrays
    spectra%spec_no=spec_list
    spectra%ndet=ndet(spec_list)
    
    tdet=sum(spectra%ndet)
    call IXFalloc(spectra%det_no,tdet,status)
    call IXFallocFortran(list_out,tdet,status)


    ! find first non-zero detector spectrum (again)
    i=1
    do while(i <= nspec )
       if ( spectra%ndet(i) /= 0) then
          ! leave loop
          spectra%det_index(i)=1
          start=i+1
          i=nspec+1 
       else
          i=i+1
       endif
    enddo
    
    ! set first value and step through (again)
    last=start-1
    do i=start,nspec
       if (spectra%ndet(i) /= 0)then
          spectra%det_index(i)=spectra%det_index(last)+spectra%ndet(last)
          last=i
       endif
    enddo

    call IXFreallocFortran(temp1,nspec,.false.,status)
    temp1=0
    
    do i=1,nspec
        spectra%spec_lookup(spectra%spec_no(i))=i       
        if(spectra%ndet(i) /= 0)then
        !fill spec_lookup array
        !array indices in compressed object
          nstart=spectra%det_index(i)
          nlast=nstart+spectra%ndet(i)-1
        !array indices in complete file
          start=det_index(spec_list(i))
          last=start+spectra%ndet(i)-1
          list_out(nstart:nlast)=det_no(start:last)
        endif
    enddo
    ! now indices in detector object will be contiguous
    ! list_out will dereference the raw file arrays to populate it
    spectra%det_no=(/ (i, i=1,tdet) /)
    deallocate(ndet,spec_no,det_index,temp1,spec,det_no)
    call IXFmark_valid(spectra)

  end subroutine populate_list_dso_isis


  pure logical function IXFcompare_spectra(spec1,spec2)result(ident)
    implicit none
    type(IXTspectra),intent(in)::spec1,spec2
! for the purposes of this test we do not need to check spec_lookup array
! since if spec_no's are the same then lookup will be the same
    ident=.true.
!    if(size(spec1%spec_lookup) /= size(spec2%spec_lookup))ident=.false.
    if(size(spec1%spec_no) /= size(spec2%spec_no))ident=.false.
    if(size(spec1%ndet) /= size(spec2%ndet))ident=.false.
    if(size(spec1%det_index) /= size(spec2%det_index))ident=.false.
    if(size(spec1%det_no) /= size(spec2%det_no))ident=.false.
    if(.not. ident)return    
!    if (sum(abs(spec1%spec_lookup - spec2%spec_lookup))/=0  ) ident=.false.        
    if (sum(abs(spec1%spec_no - spec2%spec_no))/=0  ) ident=.false.            
    if (sum(abs(spec1%ndet - spec2%ndet))/=0) ident=.false.
    if (sum(abs(spec1%det_index - spec2%det_index))/=0) ident=.false.
    if (sum(abs(spec1%det_no - spec2%det_no))/=0) ident=.false.    
  end function

  pure logical function IXFwhitecompare_spectra(sample,samdet,whitebeam,whidet)result(ident)
    implicit none
    type(IXTspectra),intent(in)::sample,whitebeam
    integer(i4b),intent(in)::samdet(:),whidet(:)
    integer(i4b)::i,sam_st,sam_end,wb_st,wb_end
    integer(i4b),allocatable::list(:)
    ident=.true.
    !length of spec_lookup array is defined maxval(IXTspectra%spec_no)
    if(size(sample%spec_lookup) > size(whitebeam%spec_lookup))then
      ident=.false.
      return
    endif
    ! we need to check that all spectra in the sample mapping are included
    ! in the whitebeam spectra list and that the detector mapping is the same 
    
    ! find indices of sample spectra in whitebeam lookup table
    allocate(list(size(sample%spec_no)))
    list=whitebeam%spec_lookup(sample%spec_no)
    
    ! check if all spectra contained in whitebeam mapping, if no indices are zero then all spectra
    ! defined in whitebeam
    if (minval(list) == 0)then
      ident=.false.
      return
    endif
    !check number of detectors per spectrum
    if (sum(abs(sample%ndet - whitebeam%ndet(list))) /=0  ) ident=.false.
    
    ! check actual detector numbers for each contributing spectrum are the same in the whitebeam
    do i=1,size(sample%spec_no)
      sam_st=sample%det_index(i)
      sam_end=sample%det_index(i)+sample%ndet(i)-1
      wb_st=whitebeam%det_index(list(i)) 
      wb_end=whitebeam%det_index(list(i))+whitebeam%ndet(list(i))-1
      if(sum(abs(samdet(sample%det_no(sam_st:sam_end)) - whidet(whitebeam%det_no(wb_st:wb_end)))) /=0)then
        ident=.false.
        return
      endif
    enddo
    
  end function


	subroutine spectrum_table_main (data_file, spectrum_file,status)!,ierr)
	use IXMtools
	use IXMfileio
	use IXMsort
!	include 'tools.par'
!	include 'tables.par'

	integer, parameter ::nmax=7,ndet_totmax=1200000

!=======================================================================================================================
!
!  Reads a data file that gives detector-spectrum mapping in a codified fashion
!  Lines containing the detector-to-spectrum entries can have one of several formats, listed
! below. A single file can contain lines with any or all of these formats. Blank lines are ignored,
! as are lines starting with an exclamation mark are ignored (which can therefore be used as comment lines)
!
! Mapping of single detector to single spectrum:
! ----------------------------------------------
!     <det. no.>   <spectrum_no.>
!
!
! Mapping of N consecutive detectors to N consecutive spectra:
! -------------------------------------------------------------
!     <N>   <starting_detector_no.>   <starting_spectrum_no.>
!
!
! Mapping of N consecutive detectors in blocks of M_GANG to consecutive spectra:
!(if N is not divisible by M, then the last spectrum holds the remainder)
! ------------------------------------------------------------------------
!     <N>   <starting_detector_no.>   <starting_spectrum_no.>   <M_GANG>
!
!
! Mapping of N consecutive detectors to N consecutive spectra, to be repeated R times with the initial detector
! and spectrum numbers to be incremented by DEL_D and DEL_S between repeats:
! --------------------------------------------------------------------------
!     <N>   <starting_detector_no.>   <starting_spectrum_no.>   <R>   <DEL_D>   <DEL_S>
!
!
! Similarly, but with ganging of M_GANG consecutive detectors into one spectrum
!(again, if N is not divisible by M, then the last spectrum holds the remainder)
! --------------------------------------------------------------------------------------
!     <N>   <starting_detector_no.>   <starting_spectrum_no.>   <M_GANG>   <R>   <DEL_D>   <DEL_S>
!
!
!=======================================================================================================================
	integer:: l_data_file, l_op_file,  iu_data, iout,  istatus, ndet, l, n, i, j, k, ierr
    integer::	        iarray(nmax), detno(ndet_totmax), specno(ndet_totmax), nd, ns, id, is, dd, nrepeat, del_d, del_s
    integer::    n_zero
    integer::      is_lo, is_hi, del_s_lo, del_s_hi, direction_s, id_lo, id_hi, del_d_lo, del_d_hi, direction_d
	integer,allocatable::indx(:)
	character(len=20):: ch
	character(len=long_len)::data_file, spectrum_file, trans_name, line
!	external getlin, newfl, getis, shutfl, itoc, sys_trans_filename, lenstr
    type(IXTstatus)::status
    istatus=0
	ierr=OK

! Open data file
	call IXFunitno (iu_data)
!	istatus = sys_trans_filename (data_file, trans_name)


trans_name=IXFtranslate_read(data_file)
	if (istatus .ne. 0) then
		call remark ('ERROR: Unable to translate data file name.')
		ierr=ERR
		return
	endif
	istatus = oldfl (iu_data, trans_name, READ)
	if (istatus .ne. 0) then
		call remark ('ERROR: Unable to open data file.')
		ierr=ERR
		return
	endif


! Generate spectra.dat entries
! ----------------------------------
	ndet = 0
 1000	continue
	call read_line (iu_data, line, l)
	if (l .gt. 0) then
!  Default values:
		nd = 1
		dd = 1
		nrepeat = 1
		del_d = 0
		del_s = 0

!  Read line
		i = 1
		n = getis (line, i, iarray, nmax)		
		if (n .eq. 2) then			! single detector - spectrum mapping
			id = iarray(1)
			is = iarray(2)
		else if (n .eq. 3) then		! single run of sequential detector and spectrum numbers
			nd = iarray(1)
			id = iarray(2)
			is = iarray(3)
		else if (n .eq. 4) then		! single run of sequential detector and spectrum number, not 1:1 mapping
			nd = iarray(1)
			id = iarray(2)
			is = iarray(3)
			dd = iarray(4)
		else if (n .eq. 6) then		! multiple sets of sequential detector and spectrum number
			nd = iarray(1)
			id = iarray(2)
			is = iarray(3)
			nrepeat = iarray(4)
			del_d = iarray(5)
			del_s = iarray(6)
		else if (n .eq. 7) then		! multiple sets of sequential detector and spectrum number, not 1:1 mapping
			nd = iarray(1)
			id = iarray(2)
			is = iarray(3)
			dd = iarray(4)
			nrepeat = iarray(5)
			del_d = iarray(6)
			del_s = iarray(7)
		else
			call remark ('ERROR: Uninterpretable line in data file:')
			call remark ('      '//line(1:l))
			istatus = shutfl(iu_data)
			ierr = ERR
			return
		endif

!  Check parameter values:
		if (nd .eq. 0) then
			call remark ('ERROR: No. detector elements cannot be zero:')
			call remark ('      '//line(1:l))
			istatus = shutfl(iu_data)
			ierr = ERR
			return
		endif
		if (id .eq. -1) then
			if (ndet .eq. 0) then
				call remark ('ERROR: Initial detector index cannot be -1 for first entry:')
				call remark ('      '//line(1:l))
				istatus = shutfl(iu_data)
				ierr = ERR
				return
			endif
		endif
		if (is .eq. -1) then
			if (ndet .eq. 0) then
				call remark ('ERROR: Initial spectrum index cannot be -1 for first entry:')
				call remark ('      '//line(1:l))
				istatus = shutfl(iu_data)
				ierr = ERR
				return
			endif
		endif
		if (dd .eq. 0) then
			call remark ('ERROR: No. detector elements mapped into each spectrum cannot be zero:')
			call remark ('      '//line(1:l))
			istatus = shutfl(iu_data)
			ierr = ERR
			return
		endif
		if (nrepeat .le. 0) then
			call remark ('ERROR: No. repetitions of detectors-to-spectra mapping must greater than zero:')
			call remark ('      '//line(1:l))
			istatus = shutfl(iu_data)
			ierr = ERR
			return
		endif

!  Fill up detector and spectra arrays:
		ns = sign(((abs(nd)-1)/abs(dd)) + 1, dd)

!    Get starting points if ID, IS = -1:
		del_d_lo = min (0, nd-sign(1,nd), (nrepeat-1)*del_d, nd-sign(1,nd)+(nrepeat-1)*del_d)
		del_d_hi = max (0, nd-sign(1,nd), (nrepeat-1)*del_d, nd-sign(1,nd)+(nrepeat-1)*del_d)
		del_s_lo = min (0, ns-sign(1,ns), (nrepeat-1)*del_s, ns-sign(1,ns)+(nrepeat-1)*del_s)
		del_s_hi = max (0, ns-sign(1,ns), (nrepeat-1)*del_s, ns-sign(1,ns)+(nrepeat-1)*del_s)
		if (id .eq. -1) then	! get starting detector no. on basis of previous
			if (direction_d .ge. 0) then	! put at higher detector numbers
				id = id_hi - del_d_lo + 1
			else
				id = id_lo - del_d_hi - 1
			endif
		endif
		if (is .eq. -1) then	! get starting spectrum no. on basis of previous
			if (direction_s .ge. 0) then	! put at higher spectrum numbers
				is = is_hi - del_s_lo + 1
			else
				is = is_lo - del_s_hi - 1
			endif
		endif
		id_lo = id + del_d_lo
		id_hi = id + del_d_hi
		direction_d = del_d
		is_lo = is + del_s_lo
		is_hi = is + del_s_hi
		direction_s = del_s

		do k = 1, nrepeat
			do i = 1, abs(nd), abs(dd)
				do j = 1, min(abs(dd), abs(nd)-i+1)
					ndet = ndet + 1
					if (ndet .gt. ndet_totmax) then
						call remark ('ERROR: No. detectors/spectra exceeds maximum permitted in internal arrays.')
						stop
					endif
					detno(ndet) = id
					specno(ndet)= is
					id = id + sign(1,nd)
				end do
				is = is + sign(1,dd)
			end do
			id = id - nd + del_d
			is = is - ns + del_s
		end do

		goto 1000

	else if (l .eq. EOF) then	! end of file reached
		goto 2000
	else
		call remark ('ERROR: Error reading from data file.')
		istatus = shutfl(iu_data)
		ierr = ERR
		return
	endif

 2000	continue
	istatus = shutfl(iu_data)


! Check that detectors have been read from the data file:
! --------------------------------------------------------
	if (ndet .eq. 0) then
		call remark ('ERROR: No detectors in input data file.')
		ierr = ERR
		return
	endif

! Sort spectrum index array:
! ----------------------------
!	call indexxi (ndet, specno, indx)
! since reading process fills as it reads we need to have a temporary index array
allocate(indx(ndet))
    call IXFrank(specno(1:ndet),indx)
! Check all spectrum numbers are greater than zero, and warn if spectrum zero present or not 1:1 mapping:
	if (specno(indx(1)) .lt. 0) then
		call remark ('ERROR: At least one spectrum number is negative.')
		ierr=ERR
		return
	endif

	n_zero = 0
	do i = 1, ndet
		if (specno(i) .eq. 0) then
			n_zero = n_zero + 1
		endif
	end do
	if (n_zero .gt. 0) then
		l = itoc (n_zero, ch)
		call remark ('WARNING: There are '//ch(1:l)//' detectors assigned to spectrum 0.')
	endif

	if (ndet .ge. 2) then
		do i = 2, ndet
			if (specno(indx(i-1)) .eq. specno(indx(i))) then
				l = itoc(detno(i),ch)
				call remark ('Note that spectrum mapping file is not 1:1.')
				goto 2100
			endif
		end do
	endif
 2100	continue


! Sort detector index array:
! ---------------------------
!	call indexxi (ndet, detno, indx)
    call IXFrank(detno(1:ndet),indx)

! Are all detectors positive, and are there any repeated detector indicies ?
	if (detno(indx(1)) .le. 0) call remark ('WARNING: At least one detector has zero or negative detector number.')

	if (ndet .ge. 2) then
		do i = 2, ndet
			if (detno(indx(i-1)) .eq. detno(indx(i))) then
				l = itoc(detno(i),ch)
				call remark ('ERROR: Detector '//ch(1:l)//' appears at least twice.')
				ierr = ERR
				return
			endif
		end do
	endif


! Write results to output file:
! ==============================
	if (lenstr(spectrum_file) .gt. 0) then
		call IXFunitno(iout)
!		istatus = sys_trans_filename (spectrum_file, trans_name)
trans_name=IXFtranslate_write(spectrum_file,IXC_WRITE)
		if (istatus .ne. 0) then
			call remark ('ERROR: Unable to translate output file.')
			ierr=ERR
			return
		endif
		istatus = newfl (iout, trans_name, READWR)
		if (istatus .ne. 0) then
			call remark ('ERROR: Unable to open output file.')
			ierr=ERR
			return
		endif
	
		write (iout, '(''NDET :'')')
		write (iout, '(i10)') ndet
		write (iout, '(''DETECTOR      SPECTRUM'')')
		do i = 1, ndet
			write (iout,'(i10,2x,i10)') detno(indx(i)), specno(indx(i))
		end do
		istatus = shutfl(iout)
	endif
    deallocate(indx)
	end subroutine spectrum_table_main
end module IXMspectra

