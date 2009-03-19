!------------------------------
! MODULE: IXMdataset_nd
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1000 $ ($Date: 2007-01-24 06:35:45 -0500 (Wed, 24 Jan 2007) $)
!!
!! FORTRAN definition of IXMdataset_nd object 

module IXMdataset_nd
  use IXMtype_definitions
  use IXMstatus
  use IXMdataset_common
  use IXMpointer_to_array
  implicit none
  public :: IXTdataset_nd
  type IXTdataset_nd
     private
     type(IXTbase):: base
     character(len=long_len) title	!! Title of dataset for plotting purposes
     integer(i4b), pointer :: nx(:)		!! Number of data points in each dimension
     real(dp),  pointer :: s(:)		!! size(s)=size(e) = nx(1)*nx(2)*nx(3) ... nx(size(nx)), and can reshape in any graphics plotting package
     real(dp),  pointer :: e(:)		!! Standard error
     character(len=long_len) s_label	!! Signal axis title (e.g. 'Counts')
     !! x-axes: the length of the array will be size(nx)
     !! - XHISTOGRAM=.TRUE  (values of bin boundaries) 
     !! - XHISTOGRAM=.FALSE  (values of data point positions)
     type (IXTpointer_to_array),  pointer :: x(:)
     character(len=long_len),  pointer :: x_label(:) !! Annotation of x-axis for plotting purposes	[NeXus: LONG_NAME attribute]
     character(len=short_len), pointer :: x_units(:) !! Units along x-axis e.g. 'meV' or 'microseconds' [NeXus: UNITS attribute]
     !! x-distribution
     !! - X_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - X_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical, pointer :: x_distribution(:)
     !! x_histogram in principle not needed, but easy if we have it
     !! - If X_HISTOGRAM=.TRUE.  then x-axis data are histogram boundaries
     !! - If X_HISTOGRAM=.FALSE. then x-axis data are point values
     logical, pointer :: x_histogram(:)
  end type IXTdataset_nd
end module IXMdataset_nd
