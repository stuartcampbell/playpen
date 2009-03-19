  subroutine associate_x_array_i4(value, x, n1, n2, n3, n4)
    use IXMtype_definitions
	implicit none
    integer  :: n1, n2, n3, n4
    integer(i4b), target :: x(n1, n2, n3, n4)
    integer(i4b), pointer ::value(:,:,:,:)
    value => x
  end subroutine
  
  subroutine associate_x_array_i3(value, x, nx, ny, nz)
    use IXMtype_definitions
	implicit none
    integer  :: nx, ny, nz
    integer(i4b), target :: x(nx, ny, nz)
    integer(i4b), pointer ::value(:,:,:)
    value => x
  end subroutine

  subroutine associate_x_array_i2(value, x, nx, ny)
    use IXMtype_definitions
	implicit none
    integer  :: nx, ny
    integer(i4b), target :: x(nx, ny)
    integer(i4b), pointer ::value(:,:)
    value => x
  end subroutine
  
  subroutine associate_x_array_i1(value, x, nx)
    use IXMtype_definitions
	implicit none
    integer  :: nx
    integer(i4b), target :: x(nx)
    integer(i4b), pointer ::value(:)
    value => x
  end subroutine

  subroutine associate_x_array_dp4(value, x, n1, n2, n3, n4)
    use IXMtype_definitions
	implicit none
    integer  :: n1, n2, n3, n4
    real(dp), target :: x(n1, n2, n3, n4)
    real(dp), pointer ::value(:,:,:,:)
    value => x
  end subroutine
  
  subroutine associate_x_array_dp3(value, x, nx, ny, nz)
    use IXMtype_definitions
	implicit none
    integer  :: nx, ny, nz
    real(dp), target :: x(nx, ny, nz)
    real(dp), pointer ::value(:,:,:)
    value => x
  end subroutine

  subroutine associate_x_array_dp2(value, x, nx, ny)
    use IXMtype_definitions
	implicit none
    integer  :: nx, ny
    real(dp), target :: x(nx, ny)
    real(dp), pointer ::value(:,:)
    value => x
  end subroutine

  subroutine associate_x_array_dp1(value, x, nx)
    use IXMtype_definitions
	implicit none
    integer  :: nx
    real(dp), target :: x(nx)
    real(dp), pointer ::value(:)
    value => x
  end subroutine

  subroutine associate_x_array_c1(value, x, nx)
    use IXMtype_definitions
	implicit none
    integer  :: nx
    character(len=*), pointer :: value(:)
!    we cannot do this properly as character strings do not map
!         character(len=*), target :: x(nx)
    integer(i1b) :: x(nx)
    value => NULL()
  end subroutine

