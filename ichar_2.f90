! { dg-do run }
! Test char and ichar intrinsic functions
Program test
integer i

if (ichar (char (0)) .ne. 0) stop 1
if (ichar (char (255)) .ne. 255) stop 1
if (ichar (char (127)) .ne. 127) stop 1

i = 0
if (ichar (char (i)) .ne. i) stop 1
i = 255
if (ichar (char (i)) .ne. i) stop 1
i = 127
if (ichar (char (i)) .ne. i) stop 1
end
