! { dg-do run }
! PR fortran/28276
! Original code submitted by Harald Anlauf
! Converted to Dejagnu for the testsuite by Steven G. Kargl
!
program gfcbug36
  implicit none
  real, parameter :: one = 1.0
  real :: a = one

  if (fraction(a) /= 0.5) stop 1
  if (fraction(one) /= 0.5) stop 1
  if (fraction(1.0) /= 0.5) stop 1

  if (exponent(a) /= 1.0) stop 1
  if (exponent(one) /= 1.0) stop 1
  if (exponent (1.0) /= 1.0) stop 1

  if (scale(fraction(a),   exponent(a))   / a   /= 1.) stop 1
  if (scale(fraction(one), exponent(one)) / one /= 1.) stop 1
  if (scale(fraction(1.0), exponent(1.0)) / 1.0 /= 1.) stop 1

end program gfcbug36
