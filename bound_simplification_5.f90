! { dg-do run }
! { dg-additional-options "-fcoarray=single -fdump-tree-original" }
!
! Check that {L,U}{,CO}BOUND intrinsics are properly simplified.
!
  implicit none

  type :: t
    integer :: c
  end type t

  type(t) :: d(3:8) = t(7)
  type(t) :: e[5:9,-1:*]
  type(t) :: h(3), j(4), k(0)

  !Test full arrays vs subarrays
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

  if (any(lbound(d     ) /= [3])) stop 1
  if (any(lbound(d(3:5)) /= [1])) stop 1
  if (any(lbound(d%c   ) /= [1])) stop 1
  if (any(ubound(d     ) /= [8])) stop 1
  if (any(ubound(d(3:5)) /= [3])) stop 1
  if (any(ubound(d%c   ) /= [6])) stop 1  

  if (any(lcobound(e  ) /=  [5, -1])) stop 1
  if (any(lcobound(e%c) /=  [5, -1])) stop 1
  ! no simplification for ucobound(e{,%c})

  call test_empty_arrays(h, j, k)

contains
  subroutine test_empty_arrays(a, c, d)
    type(t) :: a(:), c(-3:0), d(3:1)
    type(t) :: f(4:2), g(0:6)

    if (lbound(a, 1) /=  1) stop 1
    if (lbound(c, 1) /= -3) stop 1
    if (lbound(d, 1) /=  1) stop 1
    if (lbound(f, 1) /=  1) stop 1
    if (lbound(g, 1) /=  0) stop 1

    if (ubound(c, 1) /=  0) stop 1
    if (ubound(d, 1) /=  0) stop 1
    if (ubound(f, 1) /=  0) stop 1
    if (ubound(g, 1) /=  6) stop 1

    if (any(lbound(a) /= [ 1])) stop 1
    if (any(lbound(c) /= [-3])) stop 1
    if (any(lbound(d) /= [ 1])) stop 1
    if (any(lbound(f) /= [ 1])) stop 1
    if (any(lbound(g) /= [ 0])) stop 1

    if (any(ubound(c) /= [0])) stop 1
    if (any(ubound(d) /= [0])) stop 1
    if (any(ubound(f) /= [0])) stop 1
    if (any(ubound(g) /= [6])) stop 1

  end subroutine
end
! { dg-final { scan-tree-dump-not "abort" "original" } }
