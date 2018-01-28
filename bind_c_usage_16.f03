! { dg-do run }
! { dg-additional-sources bind_c_usage_16_c.c }
!
! PR fortran/34079
!
! Ensure character-returning, bind(C) function work.
!
module mod
  use iso_c_binding
  implicit none
contains
  function bar(x)  bind(c, name="returnA")
    character(len=1,kind=c_char) :: bar, x
    bar = x
    bar = 'A'
  end function bar
  function foo()  bind(c, name="returnB")
    character(len=1,kind=c_char) :: foo
    foo = 'B'
  end function foo
end module mod

subroutine test() bind(c)
  use mod
  implicit none
  character(len=1,kind=c_char) :: a
  character(len=3,kind=c_char) :: b
  character(len=1,kind=c_char) :: c(3)
  character(len=3,kind=c_char) :: d(3)
  integer :: i

  a = 'z'
  b = 'fffff'
  c = 'h'
  d = 'uuuuu'

  a = bar('x')
  if (a /= 'A') stop 1
  b = bar('y')
  if (b /= 'A' .or. iachar(b(2:2))/=32 .or. iachar(b(3:3))/=32) stop 1
  c = bar('x')
  if (any(c /= 'A')) stop 1
  d = bar('y')
  if (any(d /= 'A')) stop 1

  a = foo()
  if (a /= 'B') stop 1
  b = foo()
  if (b /= 'B') stop 1
  c = foo()
  if (any(c /= 'B')) stop 1
  d = foo()
  if (any(d /= 'B')) stop 1
  do i = 1,3
    if(iachar(d(i)(2:2)) /=32 .or. iachar(d(i)(3:3)) /= 32) stop 1
  end do
end subroutine
