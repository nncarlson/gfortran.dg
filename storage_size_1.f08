! { dg-do run }
!
! PR 44649: [OOP] F2008: storage_size intrinsic
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

type :: t
  integer(4) :: i
  real(4) :: r
end type

type,extends(t) :: t2
  integer(4) :: j
end type

type(t) :: a
type(t), dimension(1:3) :: b
class(t), allocatable :: cp

allocate(t2::cp)

if (sizeof(a)        /=  8) stop 1
if (storage_size(a)  /= 64) stop 1

if (sizeof(b)        /= 24) stop 1
if (storage_size(b)  /= 64) stop 1

if (sizeof(cp)       /= 12) stop 1
if (storage_size(cp) /= 96) stop 1

end
