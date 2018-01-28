! { dg-do run }
! PR fortran/21177
module mymod
  interface tt
    module procedure tt_i, tt_r, tt_l, tt_c4, tt_c8
  end interface tt
contains
  function tt_l(x) result(y)
    integer :: y
    logical, pointer :: x
    y = 0
  end function
  function tt_i(x) result(y)
    integer :: y
    integer, pointer :: x
    y = 1
  end function
  function tt_r(x) result(y)
    integer :: y
    real, pointer :: x
    y = 2
  end function
  function tt_c4(x) result(y)
    integer :: y
    complex(4), pointer :: x
    y = 3
  end function
  function tt_c8(x) result(y)
    integer :: y
    complex(8), pointer :: x
    y = 4
  end function
end module mymod

program test
  use mymod
  logical, pointer :: l
  integer, pointer :: i
  real, pointer :: r
  complex(4), pointer :: c4
  complex(8), pointer :: c8
  
  if (tt(l) /= 0) stop 1
  if (tt(i) /= 1) stop 1
  if (tt(r) /= 2) stop 1
  if (tt(c4) /= 3) stop 1
  if (tt(c8) /= 4) stop 1
  if (tt(null(l)) /= 0) stop 1
  if (tt(null(i)) /= 1) stop 1
  if (tt(null(r)) /= 2) stop 1
  if (tt(null(c4)) /= 3) stop 1
  if (tt(null(c8)) /= 4) stop 1
end program test
