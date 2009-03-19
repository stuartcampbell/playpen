module IXMindex
  use IXMtype_definitions


  interface IXFlower_index
     module procedure lower_index_dp, lower_index_sp, lower_index_i4b
  end interface

  interface IXFupper_index
     module procedure upper_index_dp, upper_index_sp, upper_index_i4b
  end interface

contains

  function lower_index_dp (arr, val)
    implicit none
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    !	Given monotonically increasing array ARR the function returns the smallest index M
    !     ARR(M) >= VAL
    !
    !	If no such M (i.e. ARR(M) < VAL) then M=size(arr) + 1
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First formal release
    !	T.G. Perring		2003-08-13		Changed output index M from M=0 to M=size(arr)+1 if an error
    !										This is so that we can consistentlythink of both lower_index and
    !										upper_index as having elements arr(0) = -infinity and arr(size(arr)+1) = +infinity
    !
    !-----------------------------------------------------------------------------------------------------------------------------------


    real(dp), intent(in) :: arr(:), val
    integer(i4b) :: lower_index_dp

    integer(i4b) :: n, ml, mm, mh

    n = size(arr)

    ! return if array has zero length:
    if (n == 0) then
       lower_index_dp = n+1
       return
    endif

    ! find extremal cases:
    if (arr(1) >= val) then
       lower_index_dp = 1
       return
    else if (arr(n) < val) then
       lower_index_dp = n+1
       return
    endif

    ! binary chop to find solution
    ml = 1
    mh = n
10  mm = (ml+mh)/2
    if (mm == ml) then
       lower_index_dp = mh
       return
    endif
    if (arr(mm) < val) then
       ml = mm
    else
       mh = mm
    endif
    goto 10

  end function lower_index_dp
  !---------------------------------------------------------------------------------------------
  function lower_index_sp (arr, val)
    implicit none
    !
    !	Given monotonically increasing array ARR the function returns the smallest index M
    !     ARR(M) >= VAL
    !
    !  If no such M (i.e. ARR(M) < VAL) then M=0
    !
    real(sp), intent(in) :: arr(:), val
    integer(i4b) :: lower_index_sp

    integer(i4b) :: n, ml, mm, mh

    n = size(arr)

    ! return if array has zero length:
    if (n == 0) then
       lower_index_sp = n+1
       return
    endif

    ! find extremal cases:
    if (arr(1) >= val) then
       lower_index_sp = 1
       return
    else if (arr(n) < val) then
       lower_index_sp = n+1
       return
    endif

    ! binary chop to find solution
    ml = 1
    mh = n
10  mm = (ml+mh)/2
    if (mm == ml) then
       lower_index_sp = mh
       return
    endif
    if (arr(mm) < val) then
       ml = mm
    else
       mh = mm
    endif
    goto 10

  end function lower_index_sp
  !---------------------------------------------------------------------------------------------
  function lower_index_i4b (arr, val)
    implicit none
    !
    !	Given monotonically increasing array ARR the function returns the smallest index M
    !     ARR(M) >= VAL
    !
    !  If no such M (i.e. ARR(M) < VAL) then M=0
    !
    integer(i4b), intent(in) :: arr(:), val
    integer(i4b) :: lower_index_i4b

    integer(i4b) :: n, ml, mm, mh

    n = size(arr)

    ! return if array has zero length:
    if (n == 0) then
       lower_index_i4b = n+1
       return
    endif

    ! find extremal cases:
    if (arr(1) >= val) then
       lower_index_i4b = 1
       return
    else if (arr(n) < val) then
       lower_index_i4b = n+1
       return
    endif

    ! binary chop to find solution
    ml = 1
    mh = n
10  mm = (ml+mh)/2
    if (mm == ml) then
       lower_index_i4b = mh
       return
    endif
    if (arr(mm) < val) then
       ml = mm
    else
       mh = mm
    endif
    goto 10

  end function lower_index_i4b
  !---------------------------------------------------------------------------------------------

  function upper_index_dp (arr, val)
    implicit none
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    !	Given monotonically increasing array ARR the function returns the largest index M
    !     ARR(M) =< VAL
    !
    !	If no such M (i.e. ARR(1) > VAL) then M=0
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First formal release
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------


    real(dp), intent(in) :: arr(:), val
    integer(i4b) :: upper_index_dp
    integer(i4b) :: n, ml, mm, mh

    n = size(arr)

    ! return if array has zero length:
    if (n == 0) then
       upper_index_dp = 0
       return
    endif
    ! find extremal cases:
    if (arr(n) <= val) then
       upper_index_dp = n
       return
    else if (arr(1) > val) then
       upper_index_dp = 0
       return
    endif
    ! binary chop to find solution
    ml = 1
    mh = n
10  mm = (ml+mh)/2
    if (mm == ml) then
       upper_index_dp = ml
       return
    endif
    if (arr(mm) > val) then
       mh = mm
    else
       ml = mm
    endif
    goto 10

  end function upper_index_dp
  !---------------------------------------------------------------------------------------------
  function upper_index_sp (arr, val)
    implicit none
    !
    !	Given monotonically increasing array ARR the function returns the largest index M
    !     ARR(M) =< VAL
    !
    !  If no such M (i.e. ARR(1) > VAL) then M=0
    !
    real(sp), intent(in) :: arr(:), val
    integer(i4b) :: upper_index_sp
    integer(i4b) :: n, ml, mm, mh

    n = size(arr)
    ! return if array has zero length:
    if (n == 0) then
       upper_index_sp = 0
       return
    endif
    ! find extremal cases:
    if (arr(n) <= val) then
       upper_index_sp = n
       return
    else if (arr(1) > val) then
       upper_index_sp = 0
       return
    endif
    ! binary chop to find solution
    ml = 1
    mh = n
10  mm = (ml+mh)/2
    if (mm == ml) then
       upper_index_sp = ml
       return
    endif
    if (arr(mm) > val) then
       mh = mm
    else
       ml = mm
    endif
    goto 10
  end function upper_index_sp
  !---------------------------------------------------------------------------------------------
  function upper_index_i4b (arr, val)
    implicit none
    !
    !	Given monotonically increasing array ARR the function returns the largest index M
    !     ARR(M) =< VAL
    !
    !  If no such M (i.e. ARR(1) > VAL) then M=0
    !
    integer(i4b), intent(in) :: arr(:), val
    integer(i4b) :: upper_index_i4b
    integer(i4b) :: n, ml, mm, mh

    n = size(arr)
    ! return if array has zero length:
    if (n == 0) then
       upper_index_i4b = 0
       return
    endif
    ! find extremal cases:
    if (arr(n) <= val) then
       upper_index_i4b = n
       return
    else if (arr(1) > val) then
       upper_index_i4b = 0
       return
    endif
    ! binary chop to find solution
    ml = 1
    mh = n
10  mm = (ml+mh)/2
    if (mm == ml) then
       upper_index_i4b = ml
       return
    endif
    if (arr(mm) > val) then
       mh = mm
    else
       ml = mm
    endif
    goto 10
  end function upper_index_i4b

end module IXMindex
