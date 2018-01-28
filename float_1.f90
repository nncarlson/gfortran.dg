! { dg-do run }
! PR fortran/26816
program test_float
   integer(1) :: i1 = 1
   integer(2) :: i2 = 1
   integer(4) :: i4 = 1
   integer(8) :: i8 = 1
   if (float(i1) /= 1.) stop 1                ! { dg-warning "non-default INTEGER" }
   if (float(i2) /= 1.) stop 1                ! { dg-warning "non-default INTEGER" }
   if (float(i4) /= 1.) stop 1
   if (float(i8) /= 1.) stop 1                ! { dg-warning "non-default INTEGER" }

   if (kind(float(i4)) /= kind(1.0)) stop 1
   if (kind(float(i8)) /= kind(1.0)) stop 1   ! { dg-warning "non-default INTEGER" }
end program test_float
