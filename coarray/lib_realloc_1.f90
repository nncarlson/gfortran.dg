! { dg-do run }
!
! Test that for CAF components _gfortran_caf_deregister is called
! Test that norealloc happens for CAF components during assignment
!
module m
type t
  integer, allocatable :: CAF[:]
end type t
end module m

program main
use m
type(t), target :: x,y
integer, pointer :: ptr
allocate(x%caf[*], y%caf[*])
ptr => y%caf
ptr = 6
if (.not.allocated(x%caf)) stop 1
if (.not.allocated(y%caf)) stop 1
if (y%caf /= 6) stop 1
x = y
if (x%caf /= 6) stop 1
if (.not. associated (ptr,y%caf)) stop 1
if (associated (ptr,x%caf)) stop 1
ptr = 123
if (y%caf /= 123) stop 1
if (x%caf /= 6) stop 1
end program main
