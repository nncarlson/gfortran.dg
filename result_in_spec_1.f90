! { dg-do run }
! Tests the check for PR31215, in which actual/formal interface 
! was not being correctly handled for the size of 'r' because
! it is a result.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
module test1
  implicit none
contains
  character(f(x)) function test2(x) result(r)
    implicit integer (x)
    dimension r(len(r)+1)
    integer, intent(in) :: x
    interface
      pure function f(x)
        integer, intent(in) :: x
        integer f
      end function f
    end interface
    integer i
    do i = 1, len(r)
      r(:)(i:i) = achar(mod(i,32)+iachar('@'))
    end do
  end function test2
end module test1

program test
  use test1
  implicit none
! Original problem
  if (len(test2(10)) .ne. 21) stop 1
! Check non-intrinsic calls are OK and check that fix does
! not confuse result variables.
  if (any (myfunc (test2(1)) .ne. "ABC")) stop 1
contains
  function myfunc (ch) result (chr)
    character (*) :: ch(:)
    character(len(ch)) :: chr(4)
    if (len (ch) .ne. 3) stop 1
    if (any (ch .ne. "ABC")) stop 1
    chr = test2 (1)
    if (len(test2(len(chr))) .ne. 7) stop 1
  end function myfunc
end program test

pure function f(x)
  integer, intent(in) :: x
  integer f
  f = 2*x+1
end function f
