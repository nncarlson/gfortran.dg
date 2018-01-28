! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
! PR fortran/34209
!
! Test run-time implementation of NEAREST
!
program test
  implicit none
  real(4), volatile :: r4
  real(8), volatile :: r8

! Single precision with single-precision sign

  r4 = 0.0_4
  ! 0+ > 0
  if (nearest(r4, 1.0) &
      <= r4) &
    stop 1
  ! 0++ > 0+
  if (nearest(nearest(r4, 1.0), 1.0) &
      <= nearest(r4, 1.0)) &
    stop 1
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r4, 1.0), 1.0), 1.0) &
      <= nearest(nearest(r4, 1.0), 1.0)) &
    stop 1
  ! 0+- = 0
  if (nearest(nearest(r4, 1.0), -1.0) &
      /= r4) &
    stop 1
  ! 0++- = 0+
  if (nearest(nearest(nearest(r4, 1.0), 1.0), -1.0) &
      /= nearest(r4, 1.0)) &
    stop 1
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r4, 1.0), 1.0), -1.0), -1.0) &
      /= r4) &
    stop 1

  ! 0- < 0
  if (nearest(r4, -1.0) &
      >= r4) &
    stop 1
  ! 0-- < 0+
  if (nearest(nearest(r4, -1.0), -1.0) &
      >= nearest(r4, -1.0)) &
    stop 1
  ! 0--- < 0--
  if (nearest(nearest(nearest(r4, -1.0), -1.0), -1.0) &
      >= nearest(nearest(r4, -1.0), -1.0)) &
    stop 1
  ! 0-+ = 0
  if (nearest(nearest(r4, -1.0), 1.0) &
      /= r4) &
    stop 1
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r4, -1.0), -1.0), 1.0) &
      /= nearest(r4, -1.0)) &
    stop 1
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r4, -1.0), -1.0), 1.0), 1.0) &
      /= r4) &
    stop 1

  r4 = 42.0_4
  ! 42++ > 42+
  if (nearest(nearest(r4, 1.0), 1.0) &
      <= nearest(r4, 1.0)) &
    stop 1
  ! 42-- < 42-
  if (nearest(nearest(r4, -1.0), -1.0) &
      >= nearest(r4, -1.0)) &
    stop 1
  ! 42-+ = 42
  if (nearest(nearest(r4, -1.0), 1.0) &
      /= r4) &
    stop 1
  ! 42+- = 42
  if (nearest(nearest(r4, 1.0), -1.0) &
      /= r4) &
    stop 1

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0/r4, 1.0) /= 1.0/r4) stop 1
  ! -INF- = -INF
  if (nearest(-1.0/r4, -1.0) /= -1.0/r4) stop 1
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0))) stop 1
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0))) stop 1

! Double precision with single-precision sign

  r8 = 0.0_8
  ! 0+ > 0
  if (nearest(r8, 1.0) &
      <= r8) &
    stop 1
  ! 0++ > 0+
  if (nearest(nearest(r8, 1.0), 1.0) &
      <= nearest(r8, 1.0)) &
    stop 1
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r8, 1.0), 1.0), 1.0) &
      <= nearest(nearest(r8, 1.0), 1.0)) &
    stop 1
  ! 0+- = 0
  if (nearest(nearest(r8, 1.0), -1.0) &
      /= r8) &
    stop 1
  ! 0++- = 0+
  if (nearest(nearest(nearest(r8, 1.0), 1.0), -1.0) &
      /= nearest(r8, 1.0)) &
    stop 1
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r8, 1.0), 1.0), -1.0), -1.0) &
      /= r8) &
    stop 1

  ! 0- < 0
  if (nearest(r8, -1.0) &
      >= r8) &
    stop 1
  ! 0-- < 0+
  if (nearest(nearest(r8, -1.0), -1.0) &
      >= nearest(r8, -1.0)) &
    stop 1
  ! 0--- < 0--
  if (nearest(nearest(nearest(r8, -1.0), -1.0), -1.0) &
      >= nearest(nearest(r8, -1.0), -1.0)) &
    stop 1
  ! 0-+ = 0
  if (nearest(nearest(r8, -1.0), 1.0) &
      /= r8) &
    stop 1
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r8, -1.0), -1.0), 1.0) &
      /= nearest(r8, -1.0)) &
    stop 1
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r8, -1.0), -1.0), 1.0), 1.0) &
      /= r8) &
    stop 1

  r8 = 42.0_8
  ! 42++ > 42+
  if (nearest(nearest(r8, 1.0), 1.0) &
      <= nearest(r8, 1.0)) &
    stop 1
  ! 42-- < 42-
  if (nearest(nearest(r8, -1.0), -1.0) &
      >= nearest(r8, -1.0)) &
    stop 1
  ! 42-+ = 42
  if (nearest(nearest(r8, -1.0), 1.0) &
      /= r8) &
    stop 1
  ! 42+- = 42
  if (nearest(nearest(r8, 1.0), -1.0) &
      /= r8) &
    stop 1

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0/r4, 1.0) /= 1.0/r4) stop 1
  ! -INF- = -INF
  if (nearest(-1.0/r4, -1.0) /= -1.0/r4) stop 1
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0))) stop 1
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0))) stop 1


! Single precision with double-precision sign

  r4 = 0.0_4
  ! 0+ > 0
  if (nearest(r4, 1.0d0) &
      <= r4) &
    stop 1
  ! 0++ > 0+
  if (nearest(nearest(r4, 1.0d0), 1.0d0) &
      <= nearest(r4, 1.0d0)) &
    stop 1
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r4, 1.0d0), 1.0d0), 1.0d0) &
      <= nearest(nearest(r4, 1.0d0), 1.0d0)) &
    stop 1
  ! 0+- = 0
  if (nearest(nearest(r4, 1.0d0), -1.0d0) &
      /= r4) &
    stop 1
  ! 0++- = 0+
  if (nearest(nearest(nearest(r4, 1.0d0), 1.0d0), -1.0d0) &
      /= nearest(r4, 1.0d0)) &
    stop 1
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r4, 1.0d0), 1.0d0), -1.0d0), -1.0d0) &
      /= r4) &
    stop 1

  ! 0- < 0
  if (nearest(r4, -1.0d0) &
      >= r4) &
    stop 1
  ! 0-- < 0+
  if (nearest(nearest(r4, -1.0d0), -1.0d0) &
      >= nearest(r4, -1.0d0)) &
    stop 1
  ! 0--- < 0--
  if (nearest(nearest(nearest(r4, -1.0d0), -1.0d0), -1.0d0) &
      >= nearest(nearest(r4, -1.0d0), -1.0d0)) &
    stop 1
  ! 0-+ = 0
  if (nearest(nearest(r4, -1.0d0), 1.0d0) &
      /= r4) &
    stop 1
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r4, -1.0d0), -1.0d0), 1.0d0) &
      /= nearest(r4, -1.0d0)) &
    stop 1
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r4, -1.0d0), -1.0d0), 1.0d0), 1.0d0) &
      /= r4) &
    stop 1

  r4 = 42.0_4
  ! 42++ > 42+
  if (nearest(nearest(r4, 1.0d0), 1.0d0) &
      <= nearest(r4, 1.0d0)) &
    stop 1
  ! 42-- < 42-
  if (nearest(nearest(r4, -1.0d0), -1.0d0) &
      >= nearest(r4, -1.0d0)) &
    stop 1
  ! 42-+ = 42
  if (nearest(nearest(r4, -1.0d0), 1.0d0) &
      /= r4) &
    stop 1
  ! 42+- = 42
  if (nearest(nearest(r4, 1.0d0), -1.0d0) &
      /= r4) &
    stop 1

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0d0/r4, 1.0d0) /= 1.0d0/r4) stop 1
  ! -INF- = -INF
  if (nearest(-1.0d0/r4, -1.0d0) /= -1.0d0/r4) stop 1
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0d0))) stop 1
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0d0))) stop 1

! Double precision with double-precision sign

  r8 = 0.0_8
  ! 0+ > 0
  if (nearest(r8, 1.0d0) &
      <= r8) &
    stop 1
  ! 0++ > 0+
  if (nearest(nearest(r8, 1.0d0), 1.0d0) &
      <= nearest(r8, 1.0d0)) &
    stop 1
  ! 0+++ > 0++
  if (nearest(nearest(nearest(r8, 1.0d0), 1.0d0), 1.0d0) &
      <= nearest(nearest(r8, 1.0d0), 1.0d0)) &
    stop 1
  ! 0+- = 0
  if (nearest(nearest(r8, 1.0d0), -1.0d0) &
      /= r8) &
    stop 1
  ! 0++- = 0+
  if (nearest(nearest(nearest(r8, 1.0d0), 1.0d0), -1.0d0) &
      /= nearest(r8, 1.0d0)) &
    stop 1
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(r8, 1.0d0), 1.0d0), -1.0d0), -1.0d0) &
      /= r8) &
    stop 1

  ! 0- < 0
  if (nearest(r8, -1.0d0) &
      >= r8) &
    stop 1
  ! 0-- < 0+
  if (nearest(nearest(r8, -1.0d0), -1.0d0) &
      >= nearest(r8, -1.0d0)) &
    stop 1
  ! 0--- < 0--
  if (nearest(nearest(nearest(r8, -1.0d0), -1.0d0), -1.0d0) &
      >= nearest(nearest(r8, -1.0d0), -1.0d0)) &
    stop 1
  ! 0-+ = 0
  if (nearest(nearest(r8, -1.0d0), 1.0d0) &
      /= r8) &
    stop 1
  ! 0--+ = 0-
  if (nearest(nearest(nearest(r8, -1.0d0), -1.0d0), 1.0d0) &
      /= nearest(r8, -1.0d0)) &
    stop 1
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(r8, -1.0d0), -1.0d0), 1.0d0), 1.0d0) &
      /= r8) &
    stop 1

  r8 = 42.0_8
  ! 42++ > 42+
  if (nearest(nearest(r8, 1.0d0), 1.0d0) &
      <= nearest(r8, 1.0d0)) &
    stop 1
  ! 42-- < 42-
  if (nearest(nearest(r8, -1.0d0), -1.0d0) &
      >= nearest(r8, -1.0d0)) &
    stop 1
  ! 42-+ = 42
  if (nearest(nearest(r8, -1.0d0), 1.0d0) &
      /= r8) &
    stop 1
  ! 42+- = 42
  if (nearest(nearest(r8, 1.0d0), -1.0d0) &
      /= r8) &
    stop 1

  r4 = 0.0
  ! INF+ = INF
  if (nearest(1.0d0/r4, 1.0d0) /= 1.0d0/r4) stop 1
  ! -INF- = -INF
  if (nearest(-1.0d0/r4, -1.0d0) /= -1.0d0/r4) stop 1
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0/r4,  1.0d0))) stop 1
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0/r4, -1.0d0))) stop 1

end program test
