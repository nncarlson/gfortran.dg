! { dg-do run }
! { dg-options "-fno-range-check" }
program test
  integer :: i
  i = int(z'FFFFFFFF',kind(i))
  if (i /= -1) stop 1
  if (int(z'FFFFFFFF',kind(i)) /= -1) stop 1

  if (popcnt(int(z'0F00F00080000001',8)) /= 10) stop 1
  if (popcnt(int(z'800F0001',4)) /= 6) stop 1

end program test
