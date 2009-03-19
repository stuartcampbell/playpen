!------------------------
!
! Author: Andrew H. Chen chen@meihome.com
! Last modified: 2004/01/12
! Version : $Revision: 1.1.1.1 $
!------------------------
module dataset_2d_test
  use fruit
  use IXMdataset_2d

contains

  subroutine d2d_Tests
    implicit none
    
  end subroutine


!  subroutine allUtilTest
!    implicit none;
!  
!    call floatEuqalTest
!
!  end subroutine allUtilTest
!  
!  
!  subroutine floatEuqalTest
!    use util
!    use fruit
!    implicit none;
!    
!    real :: number1 = 3.001
!    real :: number2 = 3.001
!    real :: number3 = 3.2
!
!   call assertTrue (floatEqual (number1, number2), "floatEuqalTest" )
!   call assertTrue ( .not. floatEqual (number1, number3), "floatEuqalTest")
!    
!  end subroutine floatEuqalTest

end module dataset_2d_test
