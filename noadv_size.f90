! { dg-do run }
! PR 20774: Handle size parameter for non-advancing I/O correctly
program main
  open(77,status='scratch')
  write(77,'(A)') '123'
  rewind(77)
  read(77,'(2I2)',advance='no',iostat=k,size=n) i1,i2
  if (k >=0) stop 1
  if (n /= 3) stop 1
  if (i1 /= 12 .or. i2 /= 3) stop 1
end program main
