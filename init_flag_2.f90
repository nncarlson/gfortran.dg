! { dg-do run }
! { dg-options "-finit-integer=1 -finit-logical=true -finit-real=zero" }

program init_flag_2
  call real_test
  call logical_test
  call int_test
  call complex_test
end program init_flag_2

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine real_test
  real r1
  real r2(10)
  dimension r3(10,10)
  if (r1 /= 0.0) stop 1
  if (r2(2) /= 0.0) stop 1
  if (r3(5,5) /= 0.0) stop 1
  if (r4 /= 0.0) stop 1
end subroutine real_test

subroutine logical_test
  logical l1
  logical l2(2)
  if (l1 .neqv. .true.) stop 1
  if (l2(2) .neqv. .true.) stop 1
end subroutine logical_test

subroutine int_test
  integer i1
  integer i2(10)
  dimension i3(10,10)
  if (i1 /= 1) stop 1
  if (i2(2) /= 1) stop 1
  if (i3(5,5) /= 1) stop 1
  if (i4 /= 1) stop 1
end subroutine int_test

subroutine complex_test
  complex c1
  complex c2(20,20)
  if (c1 /= (0.0,0.0)) stop 1
  if (c2(1,1) /= (0.0,0.0)) stop 1 
end subroutine complex_test
