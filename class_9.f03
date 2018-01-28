! { dg-do run }
! Test the fix for PR41706, in which arguments of class methods that
! were themselves class methods did not work.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>
!
module m
type :: t
  real :: v = 1.5
contains
  procedure, nopass :: a
  procedure, nopass :: b
  procedure, pass :: c
  procedure, nopass :: d
end type

contains

  real function a (x)
    real :: x
    a = 2.*x
  end function

  real function b (x)
    real :: x
    b = 3.*x
  end function

  real function c (x)
    class (t) :: x
    c = 4.*x%v
  end function

  subroutine d (x)
    real :: x
    if (abs(x-3.0)>1E-3) stop 1
  end subroutine

  subroutine s (x)
    class(t) :: x
    real :: r
    r = x%a (1.1)       ! worked
    if (r .ne. a (1.1)) stop 1

    r = x%a (b (1.2))   ! worked
    if (r .ne. a(b (1.2))) stop 1

    r = b ( x%a (1.3))  ! worked
    if (r .ne. b(a (1.3))) stop 1

    r = x%a(x%b (1.4))   ! failed
    if (r .ne. a(b (1.4))) stop 1

    r = x%a(x%c ())   ! failed
    if (r .ne. a(c (x))) stop 1

    call x%d (x%a(1.5))  ! failed

  end subroutine

end

  use m
  class(t),allocatable :: x
  allocate(x)
  call s (x)
end
