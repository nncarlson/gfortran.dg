! { dg-do run }
! Test the verify intrinsic.  We were ignoring the last character.
program prog
  character(len=1) :: c1
  character(len=4) :: c4
  c1 = "E"
  if (verify(c1, "1") .ne. 1) stop 1
  c4 = "ABBA"
  if (verify(c4, "A") .ne. 2) stop 1
  if (verify(c4, "A", back = .true.) .ne. 3) stop 1
  if (verify(c4, "AB") .ne. 0) stop 1
end program
