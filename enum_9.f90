! { dg-do run }
! { dg-options "-fshort-enums" }
! { dg-options "-fshort-enums -Wl,--no-enum-size-warning" { target arm_eabi } }
! Program to test enumerations when option -fshort-enums is given

program main
  implicit none
  enum, bind (c)
    enumerator :: red, black = 127
    enumerator blue
  end enum
  if (red /= 0) stop 1
  if (black /= 127) stop 1
  if (blue /= 128) stop 1
end program main
