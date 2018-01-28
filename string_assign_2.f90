! { dg-do run }
! { dg-options "-ffrontend-optimize" }
program main
  character (len=:), allocatable :: a
  a = 'a'
  if (len(a) /= 1) stop 1
  a = '  '
  if (len(a) /= 2) stop 1
end program main
