!------------------------
! Driver of all unit tests
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module libisis_UnitTest
  use fruit
  use dataset_1d_test
  use dataset_2d_test
contains

  subroutine runlibisisUnitTest    
    implicit none
    
    call initializeFruit
    
    call d1d_Tests()
    call d2d_Tests()
    
    call getTestSummary
    
  end subroutine runlibisisUnitTest
  
end module libisis_UnitTest
