! { dg-do run }
! { dg-additional-options "-fcoarray=single -fdump-tree-original" }
!
! Check that {L,U}{,CO}BOUND intrinsics are properly simplified.
!
  type :: t
    integer :: c
  end type t

  type(t) :: d(3:8) = t(7)
  type(t) :: e[5:9,-1:*]

  if (lbound(d,      1) /= 3) stop 1
  if (lbound(d(3:5), 1) /= 1) stop 1
  if (lbound(d%c,    1) /= 1) stop 1  
  if (ubound(d,      1) /= 8) stop 1  
  if (ubound(d(3:5), 1) /= 3) stop 1
  if (ubound(d%c,    1) /= 6) stop 1  

  if (lcobound(e,   1) /=  5) stop 1
  if (lcobound(e%c, 1) /=  5) stop 1
  if (lcobound(e,   2) /= -1) stop 1
  if (lcobound(e%c, 2) /= -1) stop 1
  if (ucobound(e,   1) /=  9) stop 1
  if (ucobound(e%c, 1) /=  9) stop 1
  ! no simplification for ucobound(e{,%c}, dim=2)
end
! { dg-final { scan-tree-dump-not "bound" "original" } }
! { dg-final { scan-tree-dump-not "abort" "original" } }
