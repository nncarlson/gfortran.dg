! { dg-do run }
! { dg-options "-finit-integer=-1 -finit-logical=false -finit-real=nan" }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }

program init_flag_3
  call real_test
  call logical_test
  call int_test
  call complex_test
end program init_flag_3

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine real_test
  real r1
  real r2(10)
  dimension r3(10,10)
  if (r1 .eq. r1) stop 1
  if (r2(2) .eq. r2(2)) stop 1
  if (r3(5,5) .eq. r3(5,5)) stop 1
  if (r4 .eq. r4) stop 1
end subroutine real_test

subroutine logical_test
  logical l1
  logical l2(2)
  if (l1 .neqv. .false.) stop 1
  if (l2(2) .neqv. .false.) stop 1
end subroutine logical_test

subroutine int_test
  integer i1
  integer i2(10)
  dimension i3(10,10)
  if (i1 /= -1) stop 1
  if (i2(2) /= -1) stop 1
  if (i3(5,5) /= -1) stop 1
  if (i4 /= -1) stop 1
end subroutine int_test

subroutine complex_test
  complex c1
  complex c2(20,20)
  if (c1 .eq. c1) stop 1
  if (c2(1,1) .eq. c2(1,1)) stop 1 
end subroutine complex_test
