! { dg-do run }
! { dg-options "-Wall -pedantic" }
!
! PR fortran/41872
!
!  (De)allocate tests
!
program test
  implicit none
  integer, allocatable :: a, b, c
  integer :: stat
  stat=99
  allocate(a, stat=stat)
  if (stat /= 0) stop 1
  allocate(a, stat=stat)
  if (stat == 0) stop 1

  allocate (b)
  deallocate (b, stat=stat)
  if (stat /= 0) stop 1
  deallocate (b, stat=stat)
  if (stat == 0) stop 1

  deallocate (c, stat=stat)
  if (stat == 0) stop 1
end program test
