! { dg-do run { target i?86-*-* x86_64-*-* } }
! { dg-require-effective-target ilp32 }
! { dg-options "-fdefault-integer-8 -fdefault-real-8 -fdefault-double-8" }
program a
   logical l
   integer i
   real x
   double precision d
   if (kind(l) /= 8) stop 1
   if (kind(i) /= 8) stop 1
   if (kind(x) /= 8) stop 1
   if (kind(d) /= 8) stop 1
end program a
