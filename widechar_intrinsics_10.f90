! { dg-do run }
! { dg-options "-fbackslash" }

  implicit none
  character(kind=1,len=3) :: s1(3)
  character(kind=4,len=3) :: s4(3)

  s1 = [ "abc", "def", "ghi" ]
  s4 = s1
  s4 = [ "abc", "def", "ghi" ]

  if (any (cshift (s1, 0) /= s1)) stop 1
  if (any (cshift (s4, 0) /= s4)) stop 1
  if (any (cshift (s1, 3) /= s1)) stop 1
  if (any (cshift (s4, 3) /= s4)) stop 1
  if (any (cshift (s1, 6) /= s1)) stop 1
  if (any (cshift (s4, 6) /= s4)) stop 1
  if (any (cshift (s1, -3) /= s1)) stop 1
  if (any (cshift (s4, -3) /= s4)) stop 1
  if (any (cshift (s1, -6) /= s1)) stop 1
  if (any (cshift (s4, -6) /= s4)) stop 1

  if (any (cshift (s1, 1) /= [ s1(2:3), s1(1) ])) stop 1
  if (any (cshift (s1, -1) /= [ s1(3), s1(1:2) ])) stop 1
  if (any (cshift (s1, 4) /= [ s1(2:3), s1(1) ])) stop 1
  if (any (cshift (s1, -4) /= [ s1(3), s1(1:2) ])) stop 1

  if (any (cshift (s4, 1) /= [ s4(2:3), s4(1) ])) stop 1
  if (any (cshift (s4, -1) /= [ s4(3), s4(1:2) ])) stop 1
  if (any (cshift (s4, 4) /= [ s4(2:3), s4(1) ])) stop 1
  if (any (cshift (s4, -4) /= [ s4(3), s4(1:2) ])) stop 1

  if (any (cshift (s1, 2) /= [ s1(3), s1(1:2) ])) stop 1
  if (any (cshift (s1, -2) /= [ s1(2:3), s1(1) ])) stop 1
  if (any (cshift (s1, 5) /= [ s1(3), s1(1:2) ])) stop 1
  if (any (cshift (s1, -5) /= [ s1(2:3), s1(1) ])) stop 1

  if (any (cshift (s4, 2) /= [ s4(3), s4(1:2) ])) stop 1
  if (any (cshift (s4, -2) /= [ s4(2:3), s4(1) ])) stop 1
  if (any (cshift (s4, 5) /= [ s4(3), s4(1:2) ])) stop 1
  if (any (cshift (s4, -5) /= [ s4(2:3), s4(1) ])) stop 1


  if (any (eoshift (s1, 0) /= s1)) stop 1
  if (any (eoshift (s4, 0) /= s4)) stop 1
  if (any (eoshift (s1, 3) /= "")) stop 1
  if (any (eoshift (s4, 3) /= 4_"")) stop 1
  if (any (eoshift (s1, 3, "   ") /= "")) stop 1
  if (any (eoshift (s4, 3, 4_"   ") /= 4_"")) stop 1
  if (any (eoshift (s1, 3, " x ") /= " x")) stop 1
  if (any (eoshift (s4, 3, 4_" x ") /= 4_" x")) stop 1
  if (any (eoshift (s1, -3) /= "")) stop 1
  if (any (eoshift (s4, -3) /= 4_"")) stop 1
  if (any (eoshift (s1, -3, "   ") /= "")) stop 1
  if (any (eoshift (s4, -3, 4_"   ") /= 4_"")) stop 1
  if (any (eoshift (s1, -3, " x ") /= " x")) stop 1
  if (any (eoshift (s4, -3, 4_" x ") /= 4_" x")) stop 1
  if (any (eoshift (s1, 4) /= "")) stop 1
  if (any (eoshift (s4, 4) /= 4_"")) stop 1
  if (any (eoshift (s1, 4, "   ") /= "")) stop 1
  if (any (eoshift (s4, 4, 4_"   ") /= 4_"")) stop 1
  if (any (eoshift (s1, 4, " x ") /= " x")) stop 1
  if (any (eoshift (s4, 4, 4_" x ") /= 4_" x")) stop 1
  if (any (eoshift (s1, -4) /= "")) stop 1
  if (any (eoshift (s4, -4) /= 4_"")) stop 1
  if (any (eoshift (s1, -4, "   ") /= "")) stop 1
  if (any (eoshift (s4, -4, 4_"   ") /= 4_"")) stop 1
  if (any (eoshift (s1, -4, " x ") /= " x")) stop 1
  if (any (eoshift (s4, -4, 4_" x ") /= 4_" x")) stop 1

  if (any (eoshift (s1, 1) /= [ s1(2:3), "   " ])) stop 1
  if (any (eoshift (s1, -1) /= [ "   ", s1(1:2) ])) stop 1
  if (any (eoshift (s1, 1, " x ") /= [ s1(2:3), " x " ])) stop 1
  if (any (eoshift (s1, -1, " x ") /= [ " x ", s1(1:2) ])) stop 1
  if (any (eoshift (s4, 1) /= [ s4(2:3), 4_"   " ])) stop 1
  if (any (eoshift (s4, -1) /= [ 4_"   ", s4(1:2) ])) stop 1
  if (any (eoshift (s4, 1, 4_" x ") /= [ s4(2:3), 4_" x " ])) stop 1
  if (any (eoshift (s4, -1, 4_" x ") /= [ 4_" x ", s4(1:2) ])) stop 1

  if (any (eoshift (s1, 2) /= [ s1(3), "   ", "   " ])) stop 1
  if (any (eoshift (s1, -2) /= [ "   ", "   ", s1(1) ])) stop 1
  if (any (eoshift (s1, 2, " x ") /= [ s1(3), " x ", " x " ])) stop 1
  if (any (eoshift (s1, -2, " x ") /= [ " x ", " x ", s1(1) ])) stop 1
  if (any (eoshift (s4, 2) /= [ s4(3), 4_"   ", 4_"   " ])) stop 1
  if (any (eoshift (s4, -2) /= [ 4_"   ", 4_"   ", s4(1) ])) stop 1
  if (any (eoshift (s4, 2, 4_" x ") /= [ s4(3), 4_" x ", 4_" x " ])) stop 1
  if (any (eoshift (s4, -2, 4_" x ") /= [ 4_" x ", 4_" x ", s4(1) ])) stop 1

end
