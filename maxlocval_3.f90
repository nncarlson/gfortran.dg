! { dg-do run }
  integer :: a(3), h
  integer, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  h = -huge(h)
  h = h - 1
  allocate (c(3))
  a(:) = 5
  if (maxloc (a, dim = 1).ne.1) stop 1
  if (maxval (a, dim = 1).ne.5) stop 1
  a(2) = huge(h)
  if (maxloc (a, dim = 1).ne.2) stop 1
  if (maxval (a, dim = 1).ne.huge(h)) stop 1
  a(:) = h
  if (maxloc (a, dim = 1).ne.1) stop 1
  if (maxval (a, dim = 1).ne.h) stop 1
  a(3) = -huge(h)
  if (maxloc (a, dim = 1).ne.3) stop 1
  if (maxval (a, dim = 1).ne.-huge(h)) stop 1
  c(:) = 5
  if (maxloc (c, dim = 1).ne.1) stop 1
  if (maxval (c, dim = 1).ne.5) stop 1
  c(2) = huge(h)
  if (maxloc (c, dim = 1).ne.2) stop 1
  if (maxval (c, dim = 1).ne.huge(h)) stop 1
  c(:) = h
  if (maxloc (c, dim = 1).ne.1) stop 1
  if (maxval (c, dim = 1).ne.h) stop 1
  c(3) = -huge(h)
  if (maxloc (c, dim = 1).ne.3) stop 1
  if (maxval (c, dim = 1).ne.-huge(h)) stop 1
  l = .false.
  l2(:) = .false.
  a(:) = 5
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.h) stop 1
  a(2) = huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.h) stop 1
  a(:) = h
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.h) stop 1
  a(3) = -huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.h) stop 1
  c(:) = 5
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.h) stop 1
  c(2) = huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.h) stop 1
  c(:) = h
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.h) stop 1
  c(3) = -huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.h) stop 1
  l = .true.
  l2(:) = .true.
  a(:) = 5
  if (maxloc (a, dim = 1, mask = l).ne.1) stop 1
  if (maxval (a, dim = 1, mask = l).ne.5) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.1) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.5) stop 1
  a(2) = huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.2) stop 1
  if (maxval (a, dim = 1, mask = l).ne.huge(h)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.2) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.huge(h)) stop 1
  a(:) = h
  if (maxloc (a, dim = 1, mask = l).ne.1) stop 1
  if (maxval (a, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.1) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.h) stop 1
  a(3) = -huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.3) stop 1
  if (maxval (a, dim = 1, mask = l).ne.-huge(h)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.3) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.-huge(h)) stop 1
  c(:) = 5
  if (maxloc (c, dim = 1, mask = l).ne.1) stop 1
  if (maxval (c, dim = 1, mask = l).ne.5) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.1) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.5) stop 1
  c(2) = huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.2) stop 1
  if (maxval (c, dim = 1, mask = l).ne.huge(h)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.2) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.huge(h)) stop 1
  c(:) = h
  if (maxloc (c, dim = 1, mask = l).ne.1) stop 1
  if (maxval (c, dim = 1, mask = l).ne.h) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.1) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.h) stop 1
  c(3) = -huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.3) stop 1
  if (maxval (c, dim = 1, mask = l).ne.-huge(h)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.3) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.-huge(h)) stop 1
  deallocate (c)
  allocate (c(-2:-3))
  if (maxloc (c, dim = 1).ne.0) stop 1
  if (maxval (c, dim = 1).ne.h) stop 1
end
