! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
  real :: a(3,3), b(3), nan, minf, pinf, h
  logical :: l, l2
  logical :: l3(3,3), l4(3,3), l5(3,3)

  nan = 0.0
  minf = 0.0
  pinf = 0.0
  nan = 0.0/nan
  minf = -1.0/minf
  pinf = 1.0/pinf
  h = -huge(h)
  l = .false.
  l2 = .true.
  l3 = .false.
  l4 = .true.
  l5 = .true.
  l5(1,1) = .false.
  l5(1,2) = .false.
  l5(2,3) = .false.
  a = reshape ((/ nan, nan, nan, minf, minf, minf, minf, pinf, minf /), (/ 3, 3 /))
  if (maxval (a).ne.pinf) stop 1
  if (any (maxloc (a).ne.(/ 2, 3 /))) stop 1
  b = maxval (a, dim = 1)
  if (.not.isnan(b(1))) stop 1
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, pinf /))) stop 1
  if (any (maxloc (a, dim = 1).ne.(/ 1, 1, 2 /))) stop 1
  b = maxval (a, dim = 2)
  if (any (b.ne.(/ minf, pinf, minf /))) stop 1
  if (any (maxloc (a, dim = 2).ne.(/ 2, 3, 2 /))) stop 1
  if (maxval (a, mask = l).ne.h) stop 1
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) stop 1
  b = maxval (a, dim = 1, mask = l)
  if (any (b.ne.(/ h, h, h /))) stop 1
  if (any (maxloc (a, dim = 1, mask = l).ne.(/ 0, 0, 0 /))) stop 1
  b = maxval (a, dim = 2, mask = l)
  if (any (b.ne.(/ h, h, h /))) stop 1
  if (any (maxloc (a, dim = 2, mask = l).ne.(/ 0, 0, 0 /))) stop 1
  if (maxval (a, mask = l3).ne.h) stop 1
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) stop 1
  b = maxval (a, dim = 1, mask = l3)
  if (any (b.ne.(/ h, h, h /))) stop 1
  if (any (maxloc (a, dim = 1, mask = l3).ne.(/ 0, 0, 0 /))) stop 1
  b = maxval (a, dim = 2, mask = l3)
  if (any (b.ne.(/ h, h, h /))) stop 1
  if (any (maxloc (a, dim = 2, mask = l3).ne.(/ 0, 0, 0 /))) stop 1
  if (maxval (a, mask = l2).ne.pinf) stop 1
  if (maxval (a, mask = l4).ne.pinf) stop 1
  if (any (maxloc (a, mask = l2).ne.(/ 2, 3 /))) stop 1
  if (any (maxloc (a, mask = l4).ne.(/ 2, 3 /))) stop 1
  b = maxval (a, dim = 1, mask = l2)
  if (.not.isnan(b(1))) stop 1
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, pinf /))) stop 1
  if (any (maxloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) stop 1
  b = maxval (a, dim = 2, mask = l2)
  if (any (b.ne.(/ minf, pinf, minf /))) stop 1
  if (any (maxloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) stop 1
  b = maxval (a, dim = 1, mask = l4)
  if (.not.isnan(b(1))) stop 1
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, pinf /))) stop 1
  if (any (maxloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) stop 1
  b = maxval (a, dim = 2, mask = l4)
  if (any (b.ne.(/ minf, pinf, minf /))) stop 1
  if (any (maxloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) stop 1
  if (maxval (a, mask = l5).ne.minf) stop 1
  if (any (maxloc (a, mask = l5).ne.(/ 2, 2 /))) stop 1
  b = maxval (a, dim = 1, mask = l5)
  if (.not.isnan(b(1))) stop 1
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, minf /))) stop 1
  if (any (maxloc (a, dim = 1, mask = l5).ne.(/ 2, 2, 1 /))) stop 1
  b = maxval (a, dim = 2, mask = l5)
  if (any (b.ne.(/ minf, minf, minf /))) stop 1
  if (any (maxloc (a, dim = 2, mask = l5).ne.(/ 3, 2, 2 /))) stop 1
  a = nan
  if (.not.isnan(maxval (a))) stop 1
  if (maxval (a, mask = l).ne.h) stop 1
  if (.not.isnan(maxval (a, mask = l2))) stop 1
  if (maxval (a, mask = l3).ne.h) stop 1
  if (.not.isnan(maxval (a, mask = l4))) stop 1
  if (.not.isnan(maxval (a, mask = l5))) stop 1
  if (any (maxloc (a).ne.(/ 1, 1 /))) stop 1
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) stop 1
  if (any (maxloc (a, mask = l2).ne.(/ 1, 1 /))) stop 1
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) stop 1
  if (any (maxloc (a, mask = l4).ne.(/ 1, 1 /))) stop 1
  if (any (maxloc (a, mask = l5).ne.(/ 2, 1 /))) stop 1
  a = minf
  if (maxval (a).ne.minf) stop 1
  if (maxval (a, mask = l).ne.h) stop 1
  if (maxval (a, mask = l2).ne.minf) stop 1
  if (maxval (a, mask = l3).ne.h) stop 1
  if (maxval (a, mask = l4).ne.minf) stop 1
  if (maxval (a, mask = l5).ne.minf) stop 1
  if (any (maxloc (a).ne.(/ 1, 1 /))) stop 1
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) stop 1
  if (any (maxloc (a, mask = l2).ne.(/ 1, 1 /))) stop 1
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) stop 1
  if (any (maxloc (a, mask = l4).ne.(/ 1, 1 /))) stop 1
  if (any (maxloc (a, mask = l5).ne.(/ 2, 1 /))) stop 1
  a = nan
  a(1,3) = minf
  if (maxval (a).ne.minf) stop 1
  if (maxval (a, mask = l).ne.h) stop 1
  if (maxval (a, mask = l2).ne.minf) stop 1
  if (maxval (a, mask = l3).ne.h) stop 1
  if (maxval (a, mask = l4).ne.minf) stop 1
  if (maxval (a, mask = l5).ne.minf) stop 1
  if (any (maxloc (a).ne.(/ 1, 3 /))) stop 1
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) stop 1
  if (any (maxloc (a, mask = l2).ne.(/ 1, 3 /))) stop 1
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) stop 1
  if (any (maxloc (a, mask = l4).ne.(/ 1, 3 /))) stop 1
  if (any (maxloc (a, mask = l5).ne.(/ 1, 3 /))) stop 1
end
