! Test for the ISNAN intrinsic
!
! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
  implicit none
  real :: x
  x = -1.0
  x = sqrt(x)
  if (.not. isnan(x)) stop 1
  x = 0.0
  x = x / x
  if (.not. isnan(x)) stop 1

  x = 5.0
  if (isnan(x)) stop 1
  x = huge(x)
  x = 2*x
  if (isnan(x)) stop 1
end
