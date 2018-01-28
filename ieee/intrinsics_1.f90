! { dg-do run }
! { dg-additional-options "-fno-range-check" }
!
! Check compile-time simplification of functions FRACTION, EXPONENT,
! SPACING, RRSPACING and SET_EXPONENT for special values.

program test
  implicit none
  real, parameter :: inf = 2 * huge(0.)
  real, parameter :: nan = 0. / 0.

  call check_positive_zero(fraction(0.))
  call check_negative_zero(fraction(-0.))
  if (.not. isnan(fraction(inf))) stop 1
  if (.not. isnan(fraction(-inf))) stop 1
  if (.not. isnan(fraction(nan))) stop 1

  if (exponent(0.) /= 0) stop 1
  if (exponent(-0.) /= 0) stop 1
  if (exponent(inf) /= huge(0)) stop 1
  if (exponent(-inf) /= huge(0)) stop 1
  if (exponent(nan) /= huge(0)) stop 1

  if (spacing(0.) /= spacing(tiny(0.))) stop 1
  if (spacing(-0.) /= spacing(tiny(0.))) stop 1
  if (.not. isnan(spacing(inf))) stop 1
  if (.not. isnan(spacing(-inf))) stop 1
  if (.not. isnan(spacing(nan))) stop 1

  call check_positive_zero(rrspacing(0.))
  call check_positive_zero(rrspacing(-0.))
  if (.not. isnan(rrspacing(inf))) stop 1
  if (.not. isnan(rrspacing(-inf))) stop 1
  if (.not. isnan(rrspacing(nan))) stop 1

  call check_positive_zero(set_exponent(0.,42))
  call check_negative_zero(set_exponent(-0.,42))
  if (.not. isnan(set_exponent(inf, 42))) stop 1
  if (.not. isnan(set_exponent(-inf, 42))) stop 1
  if (.not. isnan(set_exponent(nan, 42))) stop 1

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
