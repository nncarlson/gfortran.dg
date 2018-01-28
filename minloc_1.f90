! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
  real :: a(3), nan, minf, pinf
  integer :: ia(1)
  real, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  nan = 0.0
  minf = 0.0
  pinf = 0.0
  nan = 0.0/nan
  minf = -1.0/minf
  pinf = 1.0/pinf

  allocate (c(3))
  a(:) = nan
  ia = minloc (a)
  if (ia(1).ne.1) stop 1
  a(:) = pinf
  ia = minloc (a)
  if (ia(1).ne.1) stop 1
  a(1:2) = nan
  ia = minloc (a)
  if (ia(1).ne.3) stop 1
  a(2) = 1.0
  ia = minloc (a)
  if (ia(1).ne.2) stop 1
  a(2) = minf
  ia = minloc (a)
  if (ia(1).ne.2) stop 1
  c(:) = nan
  ia = minloc (c)
  if (ia(1).ne.1) stop 1
  c(:) = pinf
  ia = minloc (c)
  if (ia(1).ne.1) stop 1
  c(1:2) = nan
  ia = minloc (c)
  if (ia(1).ne.3) stop 1
  c(2) = 1.0
  ia = minloc (c)
  if (ia(1).ne.2) stop 1
  c(2) = minf
  ia = minloc (c)
  if (ia(1).ne.2) stop 1
  l = .false.
  l2(:) = .false.
  a(:) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) stop 1
  a(:) = pinf
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) stop 1
  a(1:2) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) stop 1
  a(2) = 1.0
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) stop 1
  a(2) = minf
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) stop 1
  c(:) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) stop 1
  c(:) = pinf
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) stop 1
  c(1:2) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) stop 1
  c(2) = 1.0
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) stop 1
  c(2) = minf
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) stop 1
  l = .true.
  l2(:) = .true.
  a(:) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.1) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.1) stop 1
  a(:) = pinf
  ia = minloc (a, mask = l)
  if (ia(1).ne.1) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.1) stop 1
  a(1:2) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.3) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.3) stop 1
  a(2) = 1.0
  ia = minloc (a, mask = l)
  if (ia(1).ne.2) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.2) stop 1
  a(2) = minf
  ia = minloc (a, mask = l)
  if (ia(1).ne.2) stop 1
  ia = minloc (a, mask = l2)
  if (ia(1).ne.2) stop 1
  c(:) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.1) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.1) stop 1
  c(:) = pinf
  ia = minloc (c, mask = l)
  if (ia(1).ne.1) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.1) stop 1
  c(1:2) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.3) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.3) stop 1
  c(2) = 1.0
  ia = minloc (c, mask = l)
  if (ia(1).ne.2) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.2) stop 1
  c(2) = minf
  ia = minloc (c, mask = l)
  if (ia(1).ne.2) stop 1
  ia = minloc (c, mask = l2)
  if (ia(1).ne.2) stop 1
  deallocate (c)
  allocate (c(-2:-3))
  ia = minloc (c)
  if (ia(1).ne.0) stop 1
end
