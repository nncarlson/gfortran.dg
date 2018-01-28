! { dg-do run }
! Program to test terminators in list-directed input
program list_read_1
  character(len=5) :: s

  open (unit=11, status="SCRATCH")
  ! The / terminator was causing the next value to be skipped.
  write (11, '(a)') " 42 /"
  write (11, '(a)') " 43"
  write (11, '(a)') " 44"

  rewind(11)

  read (11, *) i
  if (i .ne. 42) stop 1
  read (11, *) i
  if (i .ne. 43) stop 1
  read (11, *) i
  if (i .ne. 44) stop 1
  close (11)
end

