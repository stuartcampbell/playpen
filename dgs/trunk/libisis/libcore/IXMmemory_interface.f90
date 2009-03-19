! $Id: IXMmemory_interface.f90 782 2006-08-21 17:01:42Z jdmc43 $
! Requires IXD_NAME IXD_FTYPE IXD_DIMS to de defined

#if defined(IXD_NAME) && defined(IXD_FTYPE) && defined(IXD_DIMS)

type memory_item&/**/
                &IXD_NAME
  type(IXTmemory_info) :: info
  IXD_FTYPE_FIXED , pointer :: array( IXD_DIMS ) => NULL()
  type(memory_item&/**/
                  &IXD_NAME ), pointer :: next => NULL()
end type

type memory_chain&/**/
                 &IXD_NAME
   type(memory_item&/**/
                   &IXD_NAME ), pointer :: head => NULL()
   integer :: count = 0 !! how many items in this bucket
end type

type(memory_chain&/**/
                 &IXD_NAME ), save :: stack&/**/
                                           &IXD_NAME (IXCmemory_buckets)
                                           
interface find_memory
   module procedure find_memory&/**/
                               &IXD_NAME
end interface find_memory
      
interface push_memory
    module procedure push_memory&/**/
                                &IXD_NAME
end interface push_memory
      
interface IXFAllocDims
   module procedure allocDims&/**/
                             &IXD_NAME
end interface IXFAllocDims
     
interface IXFreallocDims
   module procedure reallocDims&/**/
                               &IXD_NAME
end interface IXFreallocDims


interface IXFcopyToVector
   module procedure copyToVector&/**/
                                &IXD_NAME
end interface IXFcopyToVector
      
interface IXFcopyFromVector
    module procedure copyFromVector&/**/
                                   &IXD_NAME
end interface IXFcopyFromVector

!! This behaves like the fortran allocate statement when applied to 
!! a pointer e.g. if you IXFalloc an array pointer that is already associated
!! with storage the previous storage is untouched. If you wish to free/reuse the
!! existing storage you should use IXFrealloc instead
  interface IXFalloc
      module procedure alloc_array_&/**/
                                   &IXD_NAME
  end interface IXFalloc

!! This behaves like the fortran allocate statement when applied to 
!! an allocatable array i.e. if you IXFallocdimsFortran an array that is already associated
!! with storage you will get an "already allocated" error
  interface IXFallocdimsFortran
      module procedure allocdims_fortran_array_&/**/
                                           &IXD_NAME
  end interface IXFallocdimsFortran

  interface IXFallocFortran
      module procedure alloc_fortran_array_&/**/
                                           &IXD_NAME
  end interface IXFallocFortran
  
  interface IXFrealloc
      module procedure realloc_array_&/**/
                                     &IXD_NAME
  end interface IXFrealloc

  interface IXFreallocFortran
      module procedure realloc_fortran_array_&/**/
                                             &IXD_NAME
  end interface IXFreallocFortran

  interface IXFreallocdimsFortran
      module procedure reallocdims_fortran_array_&/**/
                                             &IXD_NAME
  end interface IXFreallocdimsFortran


  interface IXFdealloc
      module procedure free_array_&/**/
                                  &IXD_NAME
  end interface IXFdealloc

  interface IXFdeallocFortran
      module procedure free_fortran_array_&/**/
                                          &IXD_NAME
  end interface IXFdeallocFortran

  interface associate_array
      module procedure associate_array_&/**/
                                       &IXD_NAME
  end interface associate_array

#undef IXD_NAME
#undef IXD_FTYPE
#undef IXD_FTYPE_FIXED
#undef IXD_DIMS

#endif /* defined(IXD_NAME) && defined(IXD_FTYPE) && defined(IXD_DIMS) */
