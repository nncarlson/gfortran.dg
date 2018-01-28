! { dg-do run }
! { dg-options "-std=gnu" }
! PR fortran/29391
! This file is here to check that LBOUND and UBOUND return correct values
!
! Contributed by Francois-Xavier Coudert (coudert@clipper.ens.fr)
  implicit none
  integer, allocatable :: i(:,:), j(:), u(:,:,:,:)

  allocate (i(-1:1,-1:1))
  i = 0
  allocate (j(-1:2))
  j = 0
  allocate (u(7,4,2,9))

  call foo(u,4)
  call jackal(-1,-8)
  call jackal(-1,8)

  if (any(lbound(i(-1:1,-1:1)) /= 1)) stop 1
  if (lbound(i(-1:1,-1:1), 1) /= 1) stop 1
  if (lbound(i(-1:1,-1:1), 2) /= 1) stop 1

  if (any(ubound(i(-1:1,-1:1)) /= 3)) stop 1
  if (ubound(i(-1:1,-1:1), 1) /= 3) stop 1
  if (ubound(i(-1:1,-1:1), 2) /= 3) stop 1

  if (any(lbound(i(:,:)) /= 1)) stop 1
  if (lbound(i(:,:), 1) /= 1) stop 1
  if (lbound(i(:,:), 2) /= 1) stop 1

  if (any(ubound(i(:,:)) /= 3)) stop 1
  if (ubound(i(:,:), 1) /= 3) stop 1
  if (ubound(i(:,:), 2) /= 3) stop 1

  if (any(lbound(i(0:,-1:)) /= 1)) stop 1
  if (lbound(i(0:,-1:), 1) /= 1) stop 1
  if (lbound(i(0:,-1:), 2) /= 1) stop 1

  if (any(ubound(i(0:,-1:)) /= [2,3])) stop 1
  if (ubound(i(0:,-1:), 1) /= 2) stop 1
  if (ubound(i(0:,-1:), 2) /= 3) stop 1

  if (any(lbound(i(:0,:0)) /= 1)) stop 1
  if (lbound(i(:0,:0), 1) /= 1) stop 1
  if (lbound(i(:0,:0), 2) /= 1) stop 1

  if (any(ubound(i(:0,:0)) /= 2)) stop 1
  if (ubound(i(:0,:0), 1) /= 2) stop 1
  if (ubound(i(:0,:0), 2) /= 2) stop 1

  if (any(lbound(transpose(i)) /= 1)) stop 1
  if (lbound(transpose(i), 1) /= 1) stop 1
  if (lbound(transpose(i), 2) /= 1) stop 1

  if (any(ubound(transpose(i)) /= 3)) stop 1
  if (ubound(transpose(i), 1) /= 3) stop 1
  if (ubound(transpose(i), 2) /= 3) stop 1

  if (any(lbound(reshape(i,[2,2])) /= 1)) stop 1
  if (lbound(reshape(i,[2,2]), 1) /= 1) stop 1
  if (lbound(reshape(i,[2,2]), 2) /= 1) stop 1

  if (any(ubound(reshape(i,[2,2])) /= 2)) stop 1
  if (ubound(reshape(i,[2,2]), 1) /= 2) stop 1
  if (ubound(reshape(i,[2,2]), 2) /= 2) stop 1

  if (any(lbound(cshift(i,-1)) /= 1)) stop 1
  if (lbound(cshift(i,-1), 1) /= 1) stop 1
  if (lbound(cshift(i,-1), 2) /= 1) stop 1

  if (any(ubound(cshift(i,-1)) /= 3)) stop 1
  if (ubound(cshift(i,-1), 1) /= 3) stop 1
  if (ubound(cshift(i,-1), 2) /= 3) stop 1

  if (any(lbound(eoshift(i,-1)) /= 1)) stop 1
  if (lbound(eoshift(i,-1), 1) /= 1) stop 1
  if (lbound(eoshift(i,-1), 2) /= 1) stop 1

  if (any(ubound(eoshift(i,-1)) /= 3)) stop 1
  if (ubound(eoshift(i,-1), 1) /= 3) stop 1
  if (ubound(eoshift(i,-1), 2) /= 3) stop 1

  if (any(lbound(spread(i,1,2)) /= 1)) stop 1
  if (lbound(spread(i,1,2), 1) /= 1) stop 1
  if (lbound(spread(i,1,2), 2) /= 1) stop 1

  if (any(ubound(spread(i,1,2)) /= [2,3,3])) stop 1
  if (ubound(spread(i,1,2), 1) /= 2) stop 1
  if (ubound(spread(i,1,2), 2) /= 3) stop 1
  if (ubound(spread(i,1,2), 3) /= 3) stop 1

  if (any(lbound(maxloc(i)) /= 1)) stop 1
  if (lbound(maxloc(i), 1) /= 1) stop 1

  if (any(ubound(maxloc(i)) /= 2)) stop 1
  if (ubound(maxloc(i), 1) /= 2) stop 1

  if (any(lbound(minloc(i)) /= 1)) stop 1
  if (lbound(minloc(i), 1) /= 1) stop 1

  if (any(ubound(minloc(i)) /= 2)) stop 1
  if (ubound(minloc(i), 1) /= 2) stop 1

  if (any(lbound(maxval(i,2)) /= 1)) stop 1
  if (lbound(maxval(i,2), 1) /= 1) stop 1

  if (any(ubound(maxval(i,2)) /= 3)) stop 1
  if (ubound(maxval(i,2), 1) /= 3) stop 1

  if (any(lbound(minval(i,2)) /= 1)) stop 1
  if (lbound(minval(i,2), 1) /= 1) stop 1

  if (any(ubound(minval(i,2)) /= 3)) stop 1
  if (ubound(minval(i,2), 1) /= 3) stop 1

  if (any(lbound(any(i==1,2)) /= 1)) stop 1
  if (lbound(any(i==1,2), 1) /= 1) stop 1

  if (any(ubound(any(i==1,2)) /= 3)) stop 1
  if (ubound(any(i==1,2), 1) /= 3) stop 1

  if (any(lbound(count(i==1,2)) /= 1)) stop 1
  if (lbound(count(i==1,2), 1) /= 1) stop 1

  if (any(ubound(count(i==1,2)) /= 3)) stop 1
  if (ubound(count(i==1,2), 1) /= 3) stop 1

  if (any(lbound(merge(i,i,.true.)) /= 1)) stop 1
  if (lbound(merge(i,i,.true.), 1) /= 1) stop 1
  if (lbound(merge(i,i,.true.), 2) /= 1) stop 1

  if (any(ubound(merge(i,i,.true.)) /= 3)) stop 1
  if (ubound(merge(i,i,.true.), 1) /= 3) stop 1
  if (ubound(merge(i,i,.true.), 2) /= 3) stop 1

  if (any(lbound(lbound(i)) /= 1)) stop 1
  if (lbound(lbound(i), 1) /= 1) stop 1

  if (any(ubound(lbound(i)) /= 2)) stop 1
  if (ubound(lbound(i), 1) /= 2) stop 1

  if (any(lbound(ubound(i)) /= 1)) stop 1
  if (lbound(ubound(i), 1) /= 1) stop 1

  if (any(ubound(ubound(i)) /= 2)) stop 1
  if (ubound(ubound(i), 1) /= 2) stop 1

  if (any(lbound(shape(i)) /= 1)) stop 1
  if (lbound(shape(i), 1) /= 1) stop 1

  if (any(ubound(shape(i)) /= 2)) stop 1
  if (ubound(shape(i), 1) /= 2) stop 1

  if (any(lbound(product(i,2)) /= 1)) stop 1
  if (any(ubound(product(i,2)) /= 3)) stop 1
  if (any(lbound(sum(i,2)) /= 1)) stop 1
  if (any(ubound(sum(i,2)) /= 3)) stop 1
  if (any(lbound(matmul(i,i)) /= 1)) stop 1
  if (any(ubound(matmul(i,i)) /= 3)) stop 1
  if (any(lbound(pack(i,.true.)) /= 1)) stop 1
  if (any(ubound(pack(i,.true.)) /= 9)) stop 1
  if (any(lbound(unpack(j,[.true.],[2])) /= 1)) stop 1
  if (any(ubound(unpack(j,[.true.],[2])) /= 1)) stop 1

  call sub1(i,3)
  call sub1(reshape([7,9,4,6,7,9],[3,2]),3)
  call sub2

contains

  subroutine sub1(a,n)
    integer :: n, a(2:n+1,4:*)

    if (any([lbound(a,1), lbound(a,2)] /= [2, 4])) stop 1
    if (any(lbound(a) /= [2, 4])) stop 1
  end subroutine sub1

  subroutine sub2
    integer :: x(3:2, 1:2)

    if (size(x) /= 0) stop 1
    if (lbound (x, 1) /= 1 .or. lbound(x, 2) /= 1) stop 1
    if (any (lbound (x) /= [1, 1])) stop 1
    if (ubound (x, 1) /= 0 .or. ubound(x, 2) /= 2) stop 1
    if (any (ubound (x) /= [0, 2])) stop 1
  end subroutine sub2

  subroutine sub3
    integer :: x(4:5, 1:2)

    if (size(x) /= 0) stop 1
    if (lbound (x, 1) /= 4 .or. lbound(x, 2) /= 1) stop 1
    if (any (lbound (x) /= [4, 1])) stop 1
    if (ubound (x, 1) /= 4 .or. ubound(x, 2) /= 2) stop 1
    if (any (ubound (x) /= [4, 2])) stop 1
  end subroutine sub3

  subroutine foo (x,n)
    integer :: x(7,n,2,*), n

    if (ubound(x,1) /= 7 .or. ubound(x,2) /= 4 .or. ubound(x,3) /= 2) stop 1
  end subroutine foo

  subroutine jackal (b, c)
    integer :: b, c
    integer :: soda(b:c, 3:4)

    if (b > c) then
      if (size(soda) /= 0) stop 1
      if (lbound (soda, 1) /= 1 .or. ubound (soda, 1) /= 0) stop 1
    else
      if (size(soda) /= 2*(c-b+1)) stop 1
      if (lbound (soda, 1) /= b .or. ubound (soda, 1) /= c) stop 1
    end if

    if (lbound (soda, 2) /= 3 .or. ubound (soda, 2) /= 4) stop 1
    if (any (lbound (soda) /= [lbound(soda,1), lbound(soda,2)])) stop 1
    if (any (ubound (soda) /= [ubound(soda,1), ubound(soda,2)])) stop 1

  end subroutine jackal

end
