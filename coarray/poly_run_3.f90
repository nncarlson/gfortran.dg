! { dg-do run }
!
! Check that the bounds of polymorphic coarrays is
! properly handled.
!
type t
end type t
class(t), allocatable :: a(:)[:]
class(t), allocatable :: b[:], d[:]

allocate(a(1)[*])
if (this_image() == 1 .and. any (this_image(a) /= lcobound(a))) &
  stop 1
if (any (lcobound(a) /= 1)) stop 1
if (any (ucobound(a) /= this_image())) stop 1
deallocate(a)

allocate(b[*])
if (this_image() == 1 .and. any (this_image(b) /= lcobound(b))) &
  stop 1
if (any (lcobound(b) /= 1)) stop 1
if (any (ucobound(b) /= this_image())) stop 1
deallocate(b)

allocate(a(1)[-10:*])
if (this_image() == 1 .and. any (this_image(a) /= lcobound(a))) &
  stop 1
if (any (lcobound(a) /= -10)) stop 1
if (any (ucobound(a) /= -11+this_image())) stop 1
deallocate(a)

allocate(d[23:*])
if (this_image() == 1 .and. any (this_image(d) /= lcobound(d))) &
  stop 1
if (any (lcobound(d) /= 23)) stop 1
if (any (ucobound(d) /= 22+this_image())) stop 1
deallocate(d)

end
