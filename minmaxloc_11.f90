! { dg-do run }
program main
  character(len=3), dimension(2) :: a
  a(1) = 'aaa'
  a(2) = 'bbb'
  if (maxloc(a,dim=1) /= 2) stop 1
  if (minloc(a,dim=1) /= 1) stop 1

end program main
