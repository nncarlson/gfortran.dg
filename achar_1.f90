! { dg-do run }
! achar() should work with all supported integer kinds.
program  bug6
  integer(1) :: i = 65
  character a
  a = achar(i)
  if (a /= 'A') stop 1
end program  bug6
