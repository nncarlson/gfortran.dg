! { dg-do run }
! PR fortran/66864
!
program t
   implicit none
   real(8) x
   x = 2.0d0**26.5d0
   if (floor(x) /= 94906265) stop 1
   if (floor(2.0d0**26.5d0)/= 94906265) stop 1
   x = 777666555.6d0
   if (floor(x) /= 777666555) stop 1
   if (floor(777666555.6d0) /= 777666555) stop 1
   x = 2000111222.6d0
   if (floor(x) /= 2000111222) stop 1
   if (floor(2000111222.6d0) /= 2000111222) stop 1
end program t
