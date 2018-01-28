! { dg-do run }
!
! Test for polymorphic coarrays
!
type t
end type t
class(t), allocatable :: A(:)[:,:]
allocate (A(2)[1:4,-5:*])
if (any (lcobound(A) /= [1, -5])) stop 1
if (num_images() == 1) then
  if (any (ucobound(A) /= [4, -5])) stop 1
else
  if (ucobound(A,dim=1) /= 4) stop 1
end if
if (allocated(A)) i = 5
call s(A)
!call st(A) ! FIXME

contains

subroutine s(x)
  class(t),allocatable :: x(:)[:,:]
  if (any (lcobound(x) /= [1, -5])) stop 1
  if (num_images() == 1) then
    if (any (ucobound(x) /= [4, -5])) stop 1
  else
    if (ucobound(x,dim=1) /= 4) stop 1
  end if
end subroutine s

subroutine st(x)
  class(t) :: x(:)[4,2:*]
! FIXME
!  if (any (lcobound(x) /= [1, 2])) stop 1
!  if (lcobound(x, dim=1) /= 1) stop 1
!  if (lcobound(x, dim=2) /= 2) stop 1
!  if (this_image() == 1) then
!     if (any (this_image(x) /= lcobound(x))) stop 1
!     if (this_image(x, dim=1) /= lcobound(x, dim=1)) stop 1
!     if (this_image(x, dim=2) /= lcobound(x, dim=2)) stop 1
!  end if
!  if (num_images() == 1) then
!     if (any (ucobound(x) /= [4, 2])) stop 1
!     if (ucobound(x, dim=1) /= 4) stop 1
!     if (ucobound(x, dim=2) /= 2) stop 1
!  else
!    if (ucobound(x,dim=1) /= 4) stop 1
!  end if
end subroutine st
end

