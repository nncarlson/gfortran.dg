! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }
!
! PR fortran/34192
!
! Test compile-time implementation of NEAREST
!
program test
  implicit none

! Single precision

  ! 0+ > 0
  if (nearest(0.0, 1.0) &
      <= 0.0) &
    stop 1
  ! 0++ > 0+
  if (nearest(nearest(0.0, 1.0), 1.0) &
      <= nearest(0.0, 1.0)) &
    stop 1
  ! 0+++ > 0++
  if (nearest(nearest(nearest(0.0, 1.0), 1.0), 1.0) &
      <= nearest(nearest(0.0, 1.0), 1.0)) &
    stop 1
  ! 0+- = 0
  if (nearest(nearest(0.0, 1.0), -1.0) &
      /= 0.0) &
    stop 1
  ! 0++- = 0+
  if (nearest(nearest(nearest(0.0, 1.0), 1.0), -1.0) &
      /= nearest(0.0, 1.0)) &
    stop 1
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(0.0, 1.0), 1.0), -1.0), -1.0) &
      /= 0.0) &
    stop 1

  ! 0- < 0
  if (nearest(0.0, -1.0) &
      >= 0.0) &
    stop 1
  ! 0-- < 0+
  if (nearest(nearest(0.0, -1.0), -1.0) &
      >= nearest(0.0, -1.0)) &
    stop 1
  ! 0--- < 0--
  if (nearest(nearest(nearest(0.0, -1.0), -1.0), -1.0) &
      >= nearest(nearest(0.0, -1.0), -1.0)) &
    stop 1
  ! 0-+ = 0
  if (nearest(nearest(0.0, -1.0), 1.0) &
      /= 0.0) &
    stop 1
  ! 0--+ = 0-
  if (nearest(nearest(nearest(0.0, -1.0), -1.0), 1.0) &
      /= nearest(0.0, -1.0)) &
    stop 1
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(0.0, -1.0), -1.0), 1.0), 1.0) &
      /= 0.0) &
    stop 1

  ! 42++ > 42+
  if (nearest(nearest(42.0, 1.0), 1.0) &
      <= nearest(42.0, 1.0)) &
    stop 1
  ! 42-- < 42-
  if (nearest(nearest(42.0, -1.0), -1.0) &
      >= nearest(42.0, -1.0)) &
    stop 1
  ! 42-+ = 42
  if (nearest(nearest(42.0, -1.0), 1.0) &
      /= 42.0) &
    stop 1
  ! 42+- = 42
  if (nearest(nearest(42.0, 1.0), -1.0) &
      /= 42.0) &
    stop 1

  ! INF+ = INF
  if (nearest(1.0/0.0, 1.0) /= 1.0/0.0) stop 1
  ! -INF- = -INF
  if (nearest(-1.0/0.0, -1.0) /= -1.0/0.0) stop 1
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0d0/0.0,  1.0))) stop 1
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0d0/0.0, -1.0))) stop 1

! Double precision

  ! 0+ > 0
  if (nearest(0.0d0, 1.0) &
      <= 0.0d0) &
    stop 1
  ! 0++ > 0+
  if (nearest(nearest(0.0d0, 1.0), 1.0) &
      <= nearest(0.0d0, 1.0)) &
    stop 1
  ! 0+++ > 0++
  if (nearest(nearest(nearest(0.0d0, 1.0), 1.0), 1.0) &
      <= nearest(nearest(0.0d0, 1.0), 1.0)) &
    stop 1
  ! 0+- = 0
  if (nearest(nearest(0.0d0, 1.0), -1.0) &
      /= 0.0d0) &
    stop 1
  ! 0++- = 0+
  if (nearest(nearest(nearest(0.0d0, 1.0), 1.0), -1.0) &
      /= nearest(0.0d0, 1.0)) &
    stop 1
  ! 0++-- = 0
  if (nearest(nearest(nearest(nearest(0.0d0, 1.0), 1.0), -1.0), -1.0) &
      /= 0.0d0) &
    stop 1

  ! 0- < 0
  if (nearest(0.0d0, -1.0) &
      >= 0.0d0) &
    stop 1
  ! 0-- < 0+
  if (nearest(nearest(0.0d0, -1.0), -1.0) &
      >= nearest(0.0d0, -1.0)) &
    stop 1
  ! 0--- < 0--
  if (nearest(nearest(nearest(0.0d0, -1.0), -1.0), -1.0) &
      >= nearest(nearest(0.0d0, -1.0), -1.0)) &
    stop 1
  ! 0-+ = 0
  if (nearest(nearest(0.0d0, -1.0), 1.0) &
      /= 0.0d0) &
    stop 1
  ! 0--+ = 0-
  if (nearest(nearest(nearest(0.0d0, -1.0), -1.0), 1.0) &
      /= nearest(0.0d0, -1.0)) &
    stop 1
  ! 0--++ = 0
  if (nearest(nearest(nearest(nearest(0.0d0, -1.0), -1.0), 1.0), 1.0) &
      /= 0.0d0) &
    stop 1

  ! 42++ > 42+
  if (nearest(nearest(42.0d0, 1.0), 1.0) &
      <= nearest(42.0d0, 1.0)) &
    stop 1
  ! 42-- < 42-
  if (nearest(nearest(42.0d0, -1.0), -1.0) &
      >= nearest(42.0d0, -1.0)) &
    stop 1
  ! 42-+ = 42
  if (nearest(nearest(42.0d0, -1.0), 1.0) &
      /= 42.0d0) &
    stop 1
  ! 42+- = 42
  if (nearest(nearest(42.0d0, 1.0), -1.0) &
      /= 42.0d0) &
    stop 1

  ! INF+ = INF
  if (nearest(1.0d0/0.0d0, 1.0) /= 1.0d0/0.0d0) stop 1
  ! -INF- = -INF
  if (nearest(-1.0d0/0.0d0, -1.0) /= -1.0d0/0.0d0) stop 1
  ! NAN- = NAN
  if (.not.isnan(nearest(0.0d0/0.0,  1.0))) stop 1
  ! NAN+ = NAN
  if (.not.isnan(nearest(0.0d0/0.0, -1.0))) stop 1
end program test
