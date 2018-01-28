! { dg-do run }
! { dg-options "-fno-range-check -O0" }
!
! This testcase arose from PR 31262
  integer :: a
  integer(kind=8) :: b
  a = -3
  b = -huge(b) / 7
  a = a ** 73
  b = 7894_8 * b - 78941_8
  if ((-3)**73 /= a) stop 1
  if (7894_8 * (-huge(b) / 7) - 78941_8 /= b) stop 1

  a = 1234789786453123
  if (a - 1234789786453123 /= a - (-426244989)) stop 1
  end
