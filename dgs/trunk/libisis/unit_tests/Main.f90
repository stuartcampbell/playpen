!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
program unit_testing
  use libisis_UnitTest  
  implicit none
  call runlibisisUnitTest()
  pause 'press any key to finish'
end program unit_testing
