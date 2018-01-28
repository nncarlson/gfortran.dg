! { dg-do run }
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
!
! PR fortran/34318
!
! Infinity and NaN were not properly written to the .mod file.
!
module nonordinal
  implicit none
  real, parameter :: inf = 1./0., nan = 0./0., minf = -1./0.0
end module nonordinal

program a
  use nonordinal
  implicit none
  character(len=20) :: str
  if (log(abs(inf))  < huge(inf)) stop 1
  if (log(abs(minf)) < huge(inf)) stop 1
  if (.not. isnan(nan)) stop 1
  write(str,"(sp,f10.2)") inf
  if (adjustl(str) /= "+Infinity") stop 1
  write(str,*) minf
  if (adjustl(str) /= "-Infinity") stop 1
  write(str,*) nan
  if (adjustl(str) /= "NaN") stop 1
end program a
