! { dg-do compile }
! PR 30452 - this used to cause syntax errors due to the presence,
!            as characters, of bytes 0xfe and 0xff.
program main
  if (char (254) /= "þ") stop 1
  if (char (255) /= "ÿ") stop 1
end program main
