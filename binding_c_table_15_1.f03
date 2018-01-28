! { dg-do run }
! Test the named constants in Table 15.1.
program a
  use, intrinsic :: iso_c_binding
  implicit none
  if (C_NULL_CHAR       /=  CHAR(0) ) stop 1
  if (C_ALERT           /= ACHAR(7) ) stop 1
  if (C_BACKSPACE       /= ACHAR(8) ) stop 1
  if (C_FORM_FEED       /= ACHAR(12)) stop 1
  if (C_NEW_LINE        /= ACHAR(10)) stop 1
  if (C_CARRIAGE_RETURN /= ACHAR(13)) stop 1
  if (C_HORIZONTAL_TAB  /= ACHAR(9) ) stop 1
  if (C_VERTICAL_TAB    /= ACHAR(11)) stop 1
end program a
