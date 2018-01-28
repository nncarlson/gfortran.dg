! { dg-do run }
!
! Checks the fix for PR67977 in which automatic reallocation on assignment
! was performed when the lhs had a substring reference.
!
! Contributed by Anton Shterenlikht  <mexas@bristol.ac.uk>
!
  character(:), allocatable :: z
  integer :: length
  z = "cockatoo"
  length = len (z)
  z(:) = ''
  if (len(z) .ne. length) stop 1
  if (trim (z) .ne. '') stop 1
  z(:3) = "foo"
  if (len(z) .ne. length) stop 1
  if (trim (z) .ne. "foo") stop 1
  z(4:) = "__bar"
  if (len(z) .ne. length) stop 1
  if (trim (z) .ne. "foo__bar") stop 1
  deallocate (z)
end
