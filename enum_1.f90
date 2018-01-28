! { dg-do run }
! Program to test ENUM parsing 

program main
  implicit none
  enum, bind (c)
    enumerator :: red, black
    enumerator blue
  end enum
  if (red /= 0) stop 1
end program main
