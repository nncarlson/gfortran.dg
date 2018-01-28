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
  if (maxloc (a, dim = 1).ne.1) stop 1
  if (.not.isnan(maxval (a, dim = 1))) stop 1
  a(:) = minf
  if (maxloc (a, dim = 1).ne.1) stop 1
  if (maxval (a, dim = 1).ne.minf) stop 1
  a(1:2) = nan
  if (maxloc (a, dim = 1).ne.3) stop 1
  if (maxval (a, dim = 1).ne.minf) stop 1
  a(2) = 1.0
  if (maxloc (a, dim = 1).ne.2) stop 1
  if (maxval (a, dim = 1).ne.1) stop 1
  a(2) = pinf
  if (maxloc (a, dim = 1).ne.2) stop 1
  if (maxval (a, dim = 1).ne.pinf) stop 1
  c(:) = nan
  if (maxloc (c, dim = 1).ne.1) stop 1
  if (.not.isnan(maxval (c, dim = 1))) stop 1
  c(:) = minf
  if (maxloc (c, dim = 1).ne.1) stop 1
  if (maxval (c, dim = 1).ne.minf) stop 1
  c(1:2) = nan
  if (maxloc (c, dim = 1).ne.3) stop 1
  if (maxval (c, dim = 1).ne.minf) stop 1
  c(2) = 1.0
  if (maxloc (c, dim = 1).ne.2) stop 1
  if (maxval (c, dim = 1).ne.1) stop 1
  c(2) = pinf
  if (maxloc (c, dim = 1).ne.2) stop 1
  if (maxval (c, dim = 1).ne.pinf) stop 1
  l = .false.
  l2(:) = .false.
  a(:) = nan
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  a(:) = minf
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  a(1:2) = nan
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  a(2) = 1.0
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  a(2) = pinf
  if (maxloc (a, dim = 1, mask = l).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  c(:) = nan
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  c(:) = minf
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  c(1:2) = nan
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  c(2) = 1.0
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  c(2) = pinf
  if (maxloc (c, dim = 1, mask = l).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.0) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) stop 1
  l = .true.
  l2(:) = .true.
  a(:) = nan
  if (maxloc (a, dim = 1, mask = l).ne.1) stop 1
  if (.not.isnan(maxval (a, dim = 1, mask = l))) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.1) stop 1
  if (.not.isnan(maxval (a, dim = 1, mask = l2))) stop 1
  a(:) = minf
  if (maxloc (a, dim = 1, mask = l).ne.1) stop 1
  if (maxval (a, dim = 1, mask = l).ne.minf) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.1) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.minf) stop 1
  a(1:2) = nan
  if (maxloc (a, dim = 1, mask = l).ne.3) stop 1
  if (maxval (a, dim = 1, mask = l).ne.minf) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.3) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.minf) stop 1
  a(2) = 1.0
  if (maxloc (a, dim = 1, mask = l).ne.2) stop 1
  if (maxval (a, dim = 1, mask = l).ne.1) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.2) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.1) stop 1
  a(2) = pinf
  if (maxloc (a, dim = 1, mask = l).ne.2) stop 1
  if (maxval (a, dim = 1, mask = l).ne.pinf) stop 1
  if (maxloc (a, dim = 1, mask = l2).ne.2) stop 1
  if (maxval (a, dim = 1, mask = l2).ne.pinf) stop 1
  c(:) = nan
  if (maxloc (c, dim = 1, mask = l).ne.1) stop 1
  if (.not.isnan(maxval (c, dim = 1, mask = l))) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.1) stop 1
  if (.not.isnan(maxval (c, dim = 1, mask = l2))) stop 1
  c(:) = minf
  if (maxloc (c, dim = 1, mask = l).ne.1) stop 1
  if (maxval (c, dim = 1, mask = l).ne.minf) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.1) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.minf) stop 1
  c(1:2) = nan
  if (maxloc (c, dim = 1, mask = l).ne.3) stop 1
  if (maxval (c, dim = 1, mask = l).ne.minf) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.3) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.minf) stop 1
  c(2) = 1.0
  if (maxloc (c, dim = 1, mask = l).ne.2) stop 1
  if (maxval (c, dim = 1, mask = l).ne.1) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.2) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.1) stop 1
  c(2) = pinf
  if (maxloc (c, dim = 1, mask = l).ne.2) stop 1
  if (maxval (c, dim = 1, mask = l).ne.pinf) stop 1
  if (maxloc (c, dim = 1, mask = l2).ne.2) stop 1
  if (maxval (c, dim = 1, mask = l2).ne.pinf) stop 1
  deallocate (c)
  allocate (c(-2:-3))
  if (maxloc (c, dim = 1).ne.0) stop 1
  if (maxval (c, dim = 1).ne.-huge(minf)) stop 1
end
