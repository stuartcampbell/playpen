module IXMshift
  use IXMtype_definitions
  contains

!! subroutine to shift all the elements in a supplied array by a constant (supplied) amount 
  subroutine IXFshift(array_in,array_out,shift) 
  implicit none

  real(dp),intent(in) :: array_in(:)!!input array 
  real(dp),intent(in) :: shift !! shift amount
  real(dp),intent(out) :: array_out(:) !!output array

  array_out=array_in+shift !elemental function

  end subroutine IXFshift
 

end module IXMshift



