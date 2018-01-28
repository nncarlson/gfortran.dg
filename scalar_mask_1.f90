! { dg-do run }
program main
  implicit none
  real, dimension(2) :: a
  a(1) = 2.0
  a(2) = 3.0
  if (product (a, .false.) /= 1.0) stop 1
  if (product (a, .true.) /= 6.0) stop 1
  if (sum (a, .false.) /= 0.0) stop 1
  if (sum (a, .true.) /= 5.0) stop 1
  if (maxval (a, .true.) /= 3.0) stop 1
  if (maxval (a, .false.) > -1e38) stop 1
  if (maxloc (a, 1, .true.) /= 2) stop 1
  if (maxloc (a, 1, .false.) /= 0) stop 1 ! Change to F2003 requirement.
end program main
