! { dg-do run }
! PR fortran/31803
! Assigning a substring to a pointer

program test
  implicit none
  character (len = 7), target :: textt
  character (len = 7), pointer :: textp
  character (len = 5), pointer :: textp2
  textp => textt
  textp2 => textt(1:5)
  if(len(textp)  /= 7) stop 1
  if(len(textp2) /= 5) stop 1
  textp  = 'aaaaaaa'
  textp2 = 'bbbbbbb'
  if(textp  /= 'bbbbbaa') stop 1
  if(textp2 /= 'bbbbb') stop 1
end program test
