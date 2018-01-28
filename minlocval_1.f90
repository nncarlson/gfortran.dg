! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
  real :: a(3), nan, minf, pinf
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
  if (minloc (a, dim = 1).ne.1) stop 1
  if (.not.isnan(minval (a, dim = 1))) stop 1
  a(:) = pinf
  if (minloc (a, dim = 1).ne.1) stop 1
  if (minval (a, dim = 1).ne.pinf) stop 1
  a(1:2) = nan
  if (minloc (a, dim = 1).ne.3) stop 1
  if (minval (a, dim = 1).ne.pinf) stop 1
  a(2) = 1.0
  if (minloc (a, dim = 1).ne.2) stop 1
  if (minval (a, dim = 1).ne.1) stop 1
  a(2) = minf
  if (minloc (a, dim = 1).ne.2) stop 1
  if (minval (a, dim = 1).ne.minf) stop 1
  c(:) = nan
  if (minloc (c, dim = 1).ne.1) stop 1
  if (.not.isnan(minval (c, dim = 1))) stop 1
  c(:) = pinf
  if (minloc (c, dim = 1).ne.1) stop 1
  if (minval (c, dim = 1).ne.pinf) stop 1
  c(1:2) = nan
  if (minloc (c, dim = 1).ne.3) stop 1
  if (minval (c, dim = 1).ne.pinf) stop 1
  c(2) = 1.0
  if (minloc (c, dim = 1).ne.2) stop 1
  if (minval (c, dim = 1).ne.1) stop 1
  c(2) = minf
  if (minloc (c, dim = 1).ne.2) stop 1
  if (minval (c, dim = 1).ne.minf) stop 1
  l = .false.
  l2(:) = .false.
  a(:) = nan
  if (minloc (a, dim = 1, mask = l).ne.0) stop 1
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  a(:) = pinf
  if (minloc (a, dim = 1, mask = l).ne.0) stop 1
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  a(1:2) = nan
  if (minloc (a, dim = 1, mask = l).ne.0) stop 1
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  a(2) = 1.0
  if (minloc (a, dim = 1, mask = l).ne.0) stop 1
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  a(2) = minf
  if (minloc (a, dim = 1, mask = l).ne.0) stop 1
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  c(:) = nan
  if (minloc (c, dim = 1, mask = l).ne.0) stop 1
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  c(:) = pinf
  if (minloc (c, dim = 1, mask = l).ne.0) stop 1
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  c(1:2) = nan
  if (minloc (c, dim = 1, mask = l).ne.0) stop 1
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  c(2) = 1.0
  if (minloc (c, dim = 1, mask = l).ne.0) stop 1
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  c(2) = minf
  if (minloc (c, dim = 1, mask = l).ne.0) stop 1
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) stop 1
  l = .true.
  l2(:) = .true.
  a(:) = nan
  if (minloc (a, dim = 1, mask = l).ne.1) stop 1
  if (.not.isnan(minval (a, dim = 1, mask = l))) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.1) stop 1
  if (.not.isnan(minval (a, dim = 1, mask = l2))) stop 1
  a(:) = pinf
  if (minloc (a, dim = 1, mask = l).ne.1) stop 1
  if (minval (a, dim = 1, mask = l).ne.pinf) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.1) stop 1
  if (minval (a, dim = 1, mask = l2).ne.pinf) stop 1
  a(1:2) = nan
  if (minloc (a, dim = 1, mask = l).ne.3) stop 1
  if (minval (a, dim = 1, mask = l).ne.pinf) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.3) stop 1
  if (minval (a, dim = 1, mask = l2).ne.pinf) stop 1
  a(2) = 1.0
  if (minloc (a, dim = 1, mask = l).ne.2) stop 1
  if (minval (a, dim = 1, mask = l).ne.1) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.2) stop 1
  if (minval (a, dim = 1, mask = l2).ne.1) stop 1
  a(2) = minf
  if (minloc (a, dim = 1, mask = l).ne.2) stop 1
  if (minval (a, dim = 1, mask = l).ne.minf) stop 1
  if (minloc (a, dim = 1, mask = l2).ne.2) stop 1
  if (minval (a, dim = 1, mask = l2).ne.minf) stop 1
  c(:) = nan
  if (minloc (c, dim = 1, mask = l).ne.1) stop 1
  if (.not.isnan(minval (c, dim = 1, mask = l))) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.1) stop 1
  if (.not.isnan(minval (c, dim = 1, mask = l2))) stop 1
  c(:) = pinf
  if (minloc (c, dim = 1, mask = l).ne.1) stop 1
  if (minval (c, dim = 1, mask = l).ne.pinf) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.1) stop 1
  if (minval (c, dim = 1, mask = l2).ne.pinf) stop 1
  c(1:2) = nan
  if (minloc (c, dim = 1, mask = l).ne.3) stop 1
  if (minval (c, dim = 1, mask = l).ne.pinf) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.3) stop 1
  if (minval (c, dim = 1, mask = l2).ne.pinf) stop 1
  c(2) = 1.0
  if (minloc (c, dim = 1, mask = l).ne.2) stop 1
  if (minval (c, dim = 1, mask = l).ne.1) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.2) stop 1
  if (minval (c, dim = 1, mask = l2).ne.1) stop 1
  c(2) = minf
  if (minloc (c, dim = 1, mask = l).ne.2) stop 1
  if (minval (c, dim = 1, mask = l).ne.minf) stop 1
  if (minloc (c, dim = 1, mask = l2).ne.2) stop 1
  if (minval (c, dim = 1, mask = l2).ne.minf) stop 1
  deallocate (c)
  allocate (c(-2:-3))
  if (minloc (c, dim = 1).ne.0) stop 1
  if (minval (c, dim = 1).ne.huge(pinf)) stop 1
end
