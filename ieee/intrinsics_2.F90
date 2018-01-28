! { dg-do run }
! { dg-additional-options "-fno-range-check" }
!
! Check handling of special values by FRACTION, EXPONENT,
! SPACING, RRSPACING and SET_EXPONENT.

program test
  implicit none
  real, parameter :: inf = 2 * huge(0.)
  real, parameter :: nan = 0. / 0.

  real, volatile :: x

  x = 0.
  call check_positive_zero(fraction(x))
  if (exponent(x) /= 0) stop 1
  if (spacing(x) /= spacing(tiny(x))) stop 1
  call check_positive_zero(rrspacing(x))
  call check_positive_zero(set_exponent(x,42))

  x = -0.
  call check_negative_zero(fraction(x))
  if (exponent(x) /= 0) stop 1
  if (spacing(x) /= spacing(tiny(x))) stop 1
  call check_positive_zero(rrspacing(x))
  call check_negative_zero(set_exponent(x,42))

  x = inf
  if (.not. isnan(fraction(x))) stop 1
  if (exponent(x) /= huge(0)) stop 1
  if (.not. isnan(spacing(x))) stop 1
  if (.not. isnan(rrspacing(x))) stop 1
  if (.not. isnan(set_exponent(x, 42))) stop 1

  x = -inf
  if (.not. isnan(fraction(x))) stop 1
  if (exponent(x) /= huge(0)) stop 1
  if (.not. isnan(spacing(x))) stop 1
  if (.not. isnan(rrspacing(x))) stop 1
  if (.not. isnan(set_exponent(x, 42))) stop 1

  x = nan
  if (.not. isnan(fraction(x))) stop 1
  if (exponent(x) /= huge(0)) stop 1
  if (.not. isnan(spacing(x))) stop 1
  if (.not. isnan(rrspacing(x))) stop 1
  if (.not. isnan(set_exponent(x, 42))) stop 1

contains

  subroutine check_positive_zero(x)
    use ieee_arithmetic
    implicit none
    real, value :: x

    if (ieee_class (x) /= ieee_positive_zero) stop 1
  end

  subroutine check_negative_zero(x)
    use ieee_arithmetic
    implicit none
    real, value :: x

    if (ieee_class (x) /= ieee_negative_zero) stop 1
  end

end
